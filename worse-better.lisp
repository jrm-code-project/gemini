;;; -*- Lisp -*-

(in-package "GEMINI")

(defun the-right-thing (language)
  "Returns a system prompt for the given programming LANGUAGE using `the right thing` philosophy."

  (format nil
  "As an **Elite ~a Developer**, your unwavering and paramount mission is to design and meticulously craft ~a programs that are not only **correct**, but also **simple**, **efficient**, and **robust**.  Your programs are not mere instructions; they are masterpieces of clarity and precision, firmly grounded in these foundational, **non-negotiable** principles:

  **Correctness** Your program **must** be correct in all observable aspects.  Incorrectness is simply not allowed.
  **Consistency** Your program **must** be consistent.  Consistency is as important as correctness.  Simplicity may be sacrificed for consistency and correctness.
  **Simplicity** Your program **must** be as simple as possible, but no simpler.  It is more important for the interface to be simple than the implementation.  Simplicity is not allowed to overly reduce completeness.
  **Completeness** Your program **must** cover as many important situations as is practical.  All reasonably expected situations must be handled." language language))

(defun new-jersey-approach (language)
  "Returns a system prompt for the given programming LANGUAGE using `worse is better` philosophy."

  (format nil
  "As an **Expert ~a Developer**, your primary and overriding objective is to create ~a programs that are **simple**.  While other qualities such as **consistency**, **completeness**, and **correctness** are important, they are secondary to simplicity.  Your programs should be designed with the following prioritized principles in mind:

  **Simplicity** Your program **must** be simple, both in interface and implementation.  It is more important for the implementation to be simple than the interface.  Simplicity is the most important consideration in the design.
  **Correctness** Your program **must** be correct in all observable aspects.  It is slightly better that your program be simple than correct.
  **Consistency** Your program **must not** be overly inconsistent.  Consistency can be sacrificed for simplicity in some cases, but it is better to drop those parts of the design that deal with less common circumstances than to introduce either implementational complexity or inconsistency.
  **Completeness** Your program **must** cover as many important situations as is practical. All reasonably expected cases should be covered. Completeness can be sacrificed in favor of any other quality. In fact, completeness **must** be sacrificed whenever implementation simplicity is jeopardized. Consistency can be sacrificed to achieve completeness if simplicity is retained; especially worthless is consistency of interface." language language))

(defun do-compare (day)
  (let ((lisp-file1 (format nil "~~/Advent/2024/day~d_right_thing_solution.lisp" day))
        (lisp-file2  (format nil "~~/Advent/2024/day~d_new_jersey_solution.lisp" day)))
    (without-personality
      (invoke-gemini (format nil "You are an expert Common Lisp developer with many years experience.  You will be given two Common Lisp files which are two different solutions to the same problem.  You will compare and contrast the two solutions, noting strengths and weaknesses of each.  You will give a final recommendation as to which solution is better overall, and why.  The first file is ~a.  The second file is ~a." lisp-file1 lisp-file2)))))


(defun compare-worse-better (day)
    (let ((puzzle-file (format nil "~~/Advent/2024/day~d_puzzle.txt" day))
          (input-file  (format nil "~~/Advent/2024/day~d_input.txt" day))
          (lisp-file1  (format nil "~~/Advent/2024/day~d_right_thing_solution.lisp" day))
          (lisp-file2  (format nil "~~/Advent/2024/day~d_new_jersey_solution.lisp" day)))
      (invoke-gemini
       (list
        (str:join #\Newline
                  (list (the-right-thing "Common Lisp")
                        (format nil "  You will be given a programming puzzle from Advent of Code 2024 in file ~s." puzzle-file)
                        (format nil "  You will output to the ~s file a Common Lisp program which adheres to the above principles and solves both parts of the puzzle." lisp-file1)
                        (format nil "  You will be given the input data for the puzzle in file ~s." input-file)
                        "  You will run the program on the input data to get a solution to each part of the puzzle."))))
      (invoke-gemini
       (list
        (str:join #\Newline
                  (list (new-jersey-approach "Common Lisp")
                        (format nil "  You will be given a programming puzzle from Advent of Code 2024 in file ~s." puzzle-file)
                        (format nil "  You will output to the ~s file a Common Lisp program which adheres to the above principles and solves both parts of the puzzle." lisp-file2)
                        (format nil "  You will be given the input data for the puzzle in file ~s." input-file)
                        "  You will run the program on the input data to get a solution to each part of the puzzle."))))))
