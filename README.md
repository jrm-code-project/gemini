# Common Lisp library to access Google Gemini LLM APIs

This library provides a Common Lisp interface to Google's Gemini Large Language Models.

## Installation

### Manual Installation
Clone this repository into your local-projects directory:
```bash
git clone https://github.com/jrm-code-project/gemini.git ~/quicklisp/local-projects/gemini
```
Then load with Quicklisp or ASDF as below.

You can install the `gemini` library using Quicklisp, ASDF, or manually:

### Using Quicklisp
Note, this is not part of the Quicklisp distribution, so you must manually install it into your `local-projects`
```common-lisp
(ql:quickload "gemini")
```

### Using ASDF
```common-lisp
(asdf:load-system "gemini")
```

## Setting your GOOGLE_API_KEY API key

You can obtain an API key from the [Google AI Studio](https://aistudio.google.com/app/apikey).

Put your default project in `~/.config/googleapis/default-project` and put your
API key in `~/.config/googleapis/{default-project}/apikey`.

or

Put your API key in `~/.config/googleapis/default-api-key`

or

Define the `GOOGLE_API_KEY` environment variable with the value of your API key. 

You can also set the API key at runtime:
```common-lisp
(setf (getenv "GOOGLE_API_KEY") "your-api-key")
```

## Switching Between Google and OpenAI-Compatible Backends

Backend selection is configured per persona via that persona's `config.lisp`.

Preferred backend values are symbolic:

```common-lisp
;; Google Gemini REST API
:googleapi :google-api

;; Google Interactions API over SSE
:googleapi :google-interactions-api

;; OpenAI-compatible chat completions
:googleapi :openai-api
:model "gpt-4o-mini"
:url "https://api.openai.com/v1/chat/completions"
```

Notes:
- `:googleapi :google-api` uses the Gemini REST API flow.
- `:googleapi :google-interactions-api` uses the stateful Interactions API and updates the active `runtime-session` with interaction state.
- `:googleapi :openai-api` routes requests through the OpenAI-compatible chat completions path.
- `:url` is optional for `:openai-api`. If omitted, the default is `http://localhost:1234/v1/chat/completions`.
- Legacy truthy / `nil` `:googleapi` values still work for compatibility, but new configs should prefer the symbolic values above.
- OpenAI-compatible authorization is now resolved in this order:
    1. Runtime variable `gemini::*openai-authorization*` (full header value, e.g. `"Bearer <token>"`)
    2. Environment variable `OPENAI_AUTHORIZATION` (full header value)
    3. Environment variable `OPENAI_API_KEY` (wrapped as `Bearer <key>`)
    4. Optional local fallback `Bearer lm-studio` when `gemini::*openai-use-lm-studio-default-authorization*` is set to non-NIL

Examples:

```common-lisp
;; Use explicit runtime header
(setf gemini::*openai-authorization* "Bearer your-token")

;; Or enable explicit LM Studio fallback compatibility mode
(setf gemini::*openai-use-lm-studio-default-authorization* t)
```

## Dependencies

This library depends on:
- `alexandria` - Quicklisp
- `asdf` - Quicklisp
- `chanl` - Quicklisp
- `cl-base64` - Quicklisp
- `cl-json` - Quicklisp
- `cl-ppcre` - Quicklisp
- `dexador` - Quicklisp
- `fold` - jrm-code-project
- `function` - jrm-code-project
- `google` - jrm-code-project
- `jsonx` - jrm-code-project
- `named-let` - jrm-code-project
- `promise` - jrm-code-project
- `series` - Sourceforge (series.sourceforge.net)[https://series.sourceforge.net/]
- `str` - Quicklisp
- `trivial-backtrace` - Quicklisp
- `uiop` - Quicklisp

Ensure these have been quickloaded or are available in your Quicklisp local-projects or via ASDF.

If you use Quicklisp, dependencies will be handled automatically. For manual installation, ensure all dependencies are present.

## Usage

Load the library using Quicklisp or ASDF:
```common-lisp
(ql:quickload "gemini")
;; or
(asdf:load-system "gemini")
```

### MCP tool lifecycle

Loading the `gemini` system no longer starts MCP servers automatically.

- If a persona has `:enable-mcp-tools t`, shared MCP servers are started lazily on first MCP-backed use.
- The persona-specific memory MCP server is also created lazily rather than during `content-generator` construction.
- If you want explicit control over process lifetime, call:

```common-lisp
(gemini:start-mcp-servers)
;; ... use MCP-backed features ...
(gemini:stop-mcp-servers)
```

To refresh configured servers:

```common-lisp
(gemini:restart-mcp-servers)
```

If no `~/.config/mcp/mcp.lisp` configuration file is present, these functions simply leave MCP disabled.

## Testing

Run the full suite from the repository root:

```common-lisp
(asdf:test-system "gemini-tests")
```

For a focused suite, load the test system and run the FiveAM suite directly:

```common-lisp
(asdf:load-system "gemini-tests")
(fiveam:run! 'gemini-tests::interaction-payload-tests)
```

Focused suites currently include:

- `gemini-tests::interaction-payload-tests` for payload normalization and serialization
- `gemini-tests::interaction-backend-tests` for Interactions backend invocation and tool translation
- `gemini-tests::interaction-stream-tests` for Interactions and LM Studio stream/event handling
- `gemini-tests::lmstudio-backend-tests` for LM Studio backend and bridge behavior
- `gemini-tests::interaction-live-tests` for opt-in live backend coverage

The default `gemini-tests` run is hermetic. Live backend coverage is isolated in `interaction-live-tests` and only exercises external services when its environment gates are enabled:

- `GEMINI_RUN_LIVE_INTERACTIONS_STREAM_TEST` enables live Google Interactions coverage
- `GEMINI_RUN_LIVE_LMSTUDIO_STREAM_TEST` enables live LM Studio streaming and tool-bridge coverage

## API Overview

The main exported functions are:

- `gemini:invoke-gemini` — Generate text from a prompt using a fresh context.
- `gemini:continue-gemini` — Continue a conversation with the Gemini model using the existing context.
- `gemini:invoke-gemini-with-session` / `gemini:continue-gemini-with-session` — Run the same flows against an explicit `runtime-session` instead of relying on compatibility globals.
- `gemini:new-chat-with-session` / `gemini:chat-with-session` — Session-first chat entry points.
- `gemini:make-runtime-session` / `gemini:with-runtime-session` — Create and install explicit session state for multi-turn or concurrent flows.
- `gemini:invoke-interaction-with-session` — Session-first entry point for the Google Interactions backend.
- `gemini:start-mcp-servers` / `gemini:stop-mcp-servers` — Explicitly manage MCP server lifetime when using MCP-backed tools.

See the source for additional utility functions.

### Basic use

To generate text from a prompt:
```common-lisp
(gemini:invoke-gemini "In one sentence, explain how AI works to a child.")
;; => "AI is like a super smart computer brain that learns from information to answer questions and do tasks."
```

### Continuing a conversation
```common-lisp
(gemini:continue-gemini "What happens next?")
```

### Model selection
```common-lisp
(gemini:invoke-gemini "Translate to French: Hello!" :model "gemini-2.5-pro")
```

or

```common-lisp
(setq gemini:+default-model+ "gemini-2.5-pro")
```

## Configuration

You can configure the API key and default model using environment variables or files as described above. The following environment variables are supported:

- `GOOGLE_API_KEY` — Your API key

## Troubleshooting

- Ensure your API key is valid and accessible.
- Check that all dependencies are loaded.
- For network errors, verify your internet connection and proxy settings.
- For authentication errors, confirm the API key location and permissions.

### Dynamic Personalities

A unique feature of this library is its dynamic personality system, designed to make interactions more engaging and varied.

By default (`*enable-personality*` is `t`), the system will automatically select a new, random personality each day from a predefined list. All responses from the LLM, including code explanations and conversational filler, will be delivered in the voice of that character. Today you might be talking to a pirate; tomorrow, a film noir detective.

#### Controlling the Personality

While this feature is intended to be fun, you may find yourself needing to change or disable the current personality. This can be controlled via the following functions:

*   **Change the Daily Personality:** If you are not enjoying the day's randomly selected persona, you can force a new one to be chosen:
    ```common-lisp
    (gemini:new-personality)
    ```
    This will randomly select a new personality from the list that will remain active for the rest of the day (or until `new-personality` is called again).

*   **Temporarily Disable Personality:** For a more straightforward, professional interaction, you can temporarily disable the personality system within a specific block of code using the `without-personality` macro:
    ```common-lisp
    (gemini:without-personality
      (gemini:invoke-gemini "Please explain this concept plainly."))
    ```
    Inside this block, the LLM will respond in a neutral, helpful-assistant tone.

*   **Globally Disable Personality:** To turn the feature off entirely for your session, you can set the special variable `*enable-personality*` to `nil`:
    ```common-lisp
    (setq gemini:*enable-personality* nil)
    ```

## License

See `LICENSE` for details. This project is licensed under the MIT License.

### Known Limitations & Future Roadmap

This library was initially developed as a personal tool and, as such, contains certain architectural decisions that reflect its original scope. Users and potential contributors should be aware of the following limitations, which are the primary targets for future refactoring and development:

1.  **Backend Abstraction Is Still In Progress:**
    The implementation now supports per-persona switching between the Google Gemini API and OpenAI-compatible chat-completions backends. However, core conversation logic is still centered on Gemini-style internal objects and not yet a fully provider-agnostic client interface. Some provider-specific behavior (tool-calling semantics, advanced safety controls, and payload feature parity) still requires further normalization.
    *   **Roadmap:** Continue refactoring toward a generic backend abstraction layer with a common "LLM client" interface. Provider adapters (Gemini, OpenAI-compatible, and others) should each implement the same contract so core logic can remain backend-neutral.

2.  **Compatibility Globals Still Exist at the API Boundary:**
    The codebase now has an explicit `runtime-session` abstraction and session-first entry points, but compatibility wrappers still synchronize legacy globals for older call sites. That means newer code can run isolated, concurrent sessions, while some older helper and app entry points still rely on the compatibility layer.
    *   **Roadmap:** Continue pushing explicit-session entry points outward and keep shrinking the compatibility boundary so multi-session and nested orchestration flows never need ambient global state.

Contributions and ideas for tackling these architectural improvements are highly encouraged.

## Contributing

Pull requests and issues are welcome! Please follow standard Common Lisp style and include tests for new features.
Please contact me if you have questions or need help.

## Acknowledgments

This library uses [Dexador](https://github.com/fukamachi/dexador) for HTTP requests and [cl-json](https://github.com/hankhero/cl-json) for JSON parsing.
```
