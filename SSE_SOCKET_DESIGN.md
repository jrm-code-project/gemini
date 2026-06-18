# Stateful SSE Socket Design & Integration Plan

This document details the architectural design, critique, and integration plan for a stateful **SSE Socket State Machine** and **Monitor Thread** for the Gemini Common Lisp SDK.

The design decouples the user-facing thread (which receives terminal interrupts) from the background network worker thread (which performs blocking socket I/O). This guarantees absolute responsiveness during `Control-C` interrupts or network freezes, preventing recursive loops and socket leaks.

---

## 1. Critique & Security Analysis of the Preliminary Design

A rigorous review of the initial design revealed four critical failure modes that have been resolved in this revised blueprint:

1. **Mutex Lock Deadlock Risk:** In the initial design, `transition-sse-state` called `close-sse-socket-resources-safely` while holding the state lock mutex. Since `close` on a wedged socket stream can block or take time, holding the state lock during I/O would deadlock any other threads trying to inspect or update the socket state.
   * *Resolution:* The state lock is now held *only* to update the state variable. The physical resource teardown is executed **outside** the lock.
2. **Asynchronous Thread Termination Lock Leak:** Forcefully calling `sb-thread:terminate-thread` on the network thread can interrupt it while it holds internal system locks (such as OpenSSL/`cl+ssl` locks, memory allocation locks, or stream I/O locks), permanently wedging the Lisp runtime.
   * *Resolution:* Graceful shutdown is prioritized. Closing the `body-stream` first naturally causes the blocking socket read in the network thread to fail with an immediate `end-of-file` or `stream-error`, allowing it to terminate cleanly on its own. `terminate-thread` is used only as an absolute last resort with a grace period.
3. **Interrupt Latency (Polling Sleep):** Polling the state machine using a hard `(sleep 0.2)` in the monitor thread creates up to a 200ms lag when a user hits `Control-C`.
   * *Resolution:* Incorporated SBCL condition variables (`sb-thread:make-waitqueue`). When the user thread receives an interrupt, it notifies the queue, waking the monitor thread **instantly** (sub-millisecond latency).
4. **Draining State Leak:** The initial design transitioned to `:draining` but did not execute resource cleanup on that transition, leading to permanent stream and thread leaks for aborted connections.
   * *Resolution:* The `:draining` state transition is now fully bound to immediate socket stream closure, guaranteeing no resource leaks.

---

## 2. State Machine Definition

A `stateful-sse-socket` transitions through the following lifecycle states:

```
                  ┌──────────────┐
                  │ :UNCONNECTED │
                  └──────┬───────┘
                         │ (connect)
                         ▼
                  ┌──────────────┐
                  │  :CONNECTING  │
                  └──────┬───────┘
                         │ (headers-received)
                         ▼
                  ┌──────────────┐
                  │  :STREAMING  │◄──┐
                  └──────┬───────┴───┘ (data-chunk-received)
                         │
         ┌───────────────┴───────────────┐
         │ (interrupt-received)          │ (completion / EOF / error)
         ▼                               ▼
  ┌──────────────┐                ┌──────────────┐
  │  :DRAINING   │                │   :CLOSED    │
  └──────┬───────┘                └──────────────┘
         │ (fully-drained)
         ▼
  ┌──────────────┐
  │  :ABORTED    │
  └──────────────┘
```

### State Semantics
* **`:UNCONNECTED`**: Initial state. Socket class instantiated but no request has been made.
* **`:CONNECTING`**: Request sent; waiting for HTTP response headers.
* **`:STREAMING`**: Handshake successful, actively parsing SSE line events and calling the receiver callback.
* **`:DRAINING`**: An interrupt (`Control-C`) or timeout occurred. We stop forwarding new SSE events and immediately close the underlying network stream to unblock the network thread.
* **`:CLOSED`**: Normal completion, or error termination. Stream and connection are closed.
* **`:ABORTED`**: Forcefully abandoned due to a socket hang or non-responsiveness.

---

## 3. Class Definition

We model the socket as a CLOS class with explicit locks, condition variables, and metadata to coordinate across threads:

```lisp
(in-package "GEMINI")

(defclass stateful-sse-socket ()
  ((state :initform :unconnected
          :accessor sse-socket-state
          :type (member :unconnected :connecting :streaming :draining :closed :aborted))
   (state-lock :initform (sb-thread:make-mutex :name "sse-state-lock")
               :reader sse-socket-state-lock)
   (waitqueue :initform (sb-thread:make-waitqueue)
              :reader sse-socket-waitqueue)
   (stream :initform nil
           :accessor sse-socket-stream
           :documentation "The raw HTTP/SSE body-stream returned by Dexador.")
   (network-thread :initform nil
                   :accessor sse-socket-network-thread
                   :documentation "The thread performing the active block reads.")
   (monitor-thread :initform nil
                   :accessor sse-socket-monitor-thread
                   :documentation "The thread watching for hangs, timeouts, and state updates.")
   (last-activity-time :initform (get-universal-time)
                       :accessor sse-socket-last-activity-time
                       :documentation "Timestamp of the last successfully parsed byte or SSE chunk.")
   (read-timeout :initarg :read-timeout
                 :initform 300
                 :reader sse-socket-read-timeout)
   (receiver :initarg :receiver
             :reader sse-socket-receiver
             :documentation "Callback function for parsed SSE events.")
   (abort-requested-p :initform nil
                      :accessor sse-socket-abort-requested-p
                      :documentation "Flag indicating that the client thread requested an abort.")
   (cleanup-hook :initform nil
                 :accessor sse-socket-cleanup-hook
                 :documentation "Closure to run on final termination to release resources.")))
```

---

## 4. Concurrency Model and Thread Roles

### A. The Client Thread (Owner)
* Calls `invoke-backend`.
* Initiates the connection.
* If a `Control-C` (`sb-sys:interactive-interrupt`) is caught by this thread, it **does not wait for the socket read to finish**. It flags the state machine as `:draining` (and `abort-requested-p` to `t`) and wakes up the monitor thread via the waitqueue instantly.
* It can then exit immediately and return cleanly, leaving the monitor thread to execute the physical socket closure.

### B. The Network Thread (Reader)
* Spawned to perform the blocking read loop (`google::process-sse-stream` or the JSON-decoding `do` loop).
* Pushes parsed events to the client's receiver callback.
* Updates `last-activity-time` upon every successful read.
* Safe from swallowing interrupts because it runs in a background thread that does not receive the interactive terminal signal.

### C. The Monitor Thread (Guardian)
* Periodically wakes up (or is woken up instantly by waitqueue signaling) to check:
  1. **Inactivity Hangs:** If `(get-universal-time) - last-activity-time > read-timeout` and the state is `:streaming`, it transitions the socket state to `:aborted` and forcefully kills the network thread.
  2. **Abort Requests:** If `abort-requested-p` is true, it changes the state to `:draining` and gracefully shuts down the socket.
  3. **Resource Collection:** If the network thread completes normally or with an error, the monitor thread safely closes the `body-stream` stream, invokes the `cleanup-hook`, and transitions to `:closed`.

---

## 5. State Transition & Thread-Safe Cleanup

To prevent deadlocks, the lock is acquired purely to perform state validation and assignment. The expensive or blocking I/O cleanup operations are executed outside of the mutex lock.

```lisp
(defun transition-sse-state (socket new-state)
  "Thread-safely transitions the socket state, checking valid lifecycles."
  (let ((trigger-cleanup nil))
    (sb-thread:with-mutex ((sse-socket-state-lock socket))
      (let ((old-state (sse-socket-state socket)))
        (unless (eq old-state new-state)
          (log-debug "SSE Socket State Transition: ~A -> ~A" old-state new-state)
          (setf (sse-socket-state socket) new-state)
          (when (member new-state '(:draining :closed :aborted))
            (setf trigger-cleanup t)))))
    ;; Perform teardown OUTSIDE the mutex lock to prevent blocking deadlocks
    (when trigger-cleanup
      (close-sse-socket-resources-safely socket))))

(defun close-sse-socket-resources-safely (socket)
  "Executes physical resource teardown. Idempotent and thread-safe."
  ;; 1. Close Stream first (this unblocks the network-thread from read() immediately)
  (let ((stream (sse-socket-stream socket)))
    (when (and stream (open-stream-p stream))
      (ignore-errors (close stream))
      (setf (sse-socket-stream socket) nil)))

  ;; 2. Run teardown hook
  (let ((hook (sse-socket-cleanup-hook socket)))
    (when hook
      (setf (sse-socket-cleanup-hook socket) nil)
      (ignore-errors (funcall hook))))

  ;; 3. Terminate network thread gracefully, fallback to forceful terminate-thread only if wedged
  (let ((net-thread (sse-socket-network-thread socket)))
    (when (and net-thread 
               (sb-thread:thread-alive-p net-thread)
               (not (eq sb-thread:*current-thread* net-thread)))
      ;; Allow a very brief window (50ms) for the thread to exit naturally due to closed stream
      (loop repeat 5
            while (sb-thread:thread-alive-p net-thread)
            do (sleep 0.01))
      ;; Forceful fallback if still alive
      (when (sb-thread:thread-alive-p net-thread)
        (ignore-errors (sb-thread:terminate-thread net-thread)))
      (setf (sse-socket-network-thread socket) nil))))
```

### The Waitqueue-Driven Monitor Loop
Using `sb-thread:condition-wait` allows the monitor thread to sleep indefinitely or block with a timeout, waking up instantly on a `Control-C` abort notification.

```lisp
(defun start-sse-monitor-thread (socket)
  "Launches the guardian thread to oversee connection safety."
  (setf (sse-socket-monitor-thread socket)
        (sb-thread:make-thread
         (lambda ()
           (unwind-protect
                (loop
                  (let ((state nil)
                        (now (get-universal-time)))
                    ;; Safely read current state
                    (sb-thread:with-mutex ((sse-socket-state-lock socket))
                      (setf state (sse-socket-state socket)))
                    
                    ;; Case 1: Check for timeouts (socket is frozen/wedged)
                    (when (and (eq state :streaming)
                               (> (- now (sse-socket-last-activity-time socket))
                                  (sse-socket-read-timeout socket)))
                      (log-error "SSE Socket read-timeout exceeded. Socket is frozen.")
                      (transition-sse-state socket :aborted)
                      (return))

                    ;; Case 2: Teardown if client thread aborted
                    (when (sse-socket-abort-requested-p socket)
                      (transition-sse-state socket :draining)
                      (return))

                    ;; Case 3: Complete monitor loop if connection closed cleanly
                    (when (member state '(:closed :aborted))
                      (return))

                    ;; Wait on waitqueue for up to 0.5s, or wake up instantly on notification
                    (sb-thread:with-mutex ((sse-socket-state-lock socket))
                      (sb-thread:condition-wait (sse-socket-waitqueue socket)
                                                (sse-socket-state-lock socket)
                                                :timeout 0.5))))
             ;; Final cleanup safety net
             (transition-sse-state socket :closed)))
         :name "SSE Socket Monitor")))

(defun signal-sse-abort (socket)
  "Instantly signals an abort request to the monitor thread."
  (sb-thread:with-mutex ((sse-socket-state-lock socket))
    (setf (sse-socket-abort-requested-p socket) t))
  (sb-thread:condition-notify (sse-socket-waitqueue socket)))
```

---

## 6. Integration Plan

### Step 1: Declare the Class and Export Symbols
* **File:** `package.lisp`
* **Changes:** Add and export `STATEFUL-SSE-SOCKET`, `SSE-SOCKET-STATE`, and utility methods to the `GEMINI` package.

### Step 2: Inject Stateful Streaming into `interaction.lisp`
* **File:** `interaction.lisp`
* **Changes:**
  1. Modify `google-interactions-post-streaming` to instantiate `stateful-sse-socket` instead of making a direct `dex:post` call.
  2. The function will:
     - Wrap `dex:post` with `:want-stream t` in the network thread.
     - Associate the returned stream with the `stateful-sse-socket` instance.
     - Register a cleanup hook to ensure the network thread is closed on exit.
     - Launch the `monitor-thread`.
  3. Change the streaming receiver loop to update `(sse-socket-last-activity-time socket)` upon every incoming SSE event packet.
  4. Ensure mock-detection path (where `google:google-post` is mocked in legacy tests) bypasses the socket state machine cleanly, invoking the mock callbacks immediately and transitioning to `:closed` to retain test backward-compatibility.

### Step 3: Rewrite Retry Logic to Respect Abort Requests
* **File:** `interaction.lisp`
* **Changes:** In `post-interactions-streaming-with-retry` and `post-interactions-with-retry`, update the error handler to explicitly **not retry** if the error is triggered by a socket abort or interrupt:

```lisp
(error (e)
  (if (and (not (sse-socket-abort-requested-p socket)) ;; Do not retry if we are aborting!
           (interactions-malformed-tool-call-error-p e)
           (< attempt +interactions-malformed-tool-call-max-retries+))
      (progn
        (incf attempt)
        (log-warn "Interactions malformed_tool_call response. Retrying...")
        (post-once))
      (error e)))
```

### Step 4: Graceful Drain on Control-C
* **File:** `interaction.lisp` (inside `invoke-backend` for `interactions-backend`)
* **Changes:** Wrap the execution block in a `handler-bind` for `sb-sys:interactive-interrupt` (and standard `error`). If caught, call `(signal-sse-abort socket)` to instantly alert the monitor thread, and cleanly propagate/unwind:

```lisp
(handler-bind ((sb-sys:interactive-interrupt
                (lambda (c)
                  (declare (ignore c))
                  (signal-sse-abort socket)
                  (transition-sse-state socket :draining)
                  ;; Cleanly unwind past the network layers without triggering retries
                  (return-from invoke-backend (values nil nil)))))
  ...)
```

### Step 5: Test and Validate
* **File:** `tests/main.lisp`
* **Changes:** Add unit tests to simulate:
  - An unresponsive socket (by mocking `google:*dex-post*` to return a stream that blocks indefinitely) and verifying the monitor thread aborts the request precisely after the timeout.
  - A `Control-C` interruption (via `sb-thread:interrupt-thread`) and verifying that the `body-stream` is closed immediately and cleanly without leaking connections.
