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

## API Overview

The main exported functions are:

- `gemini:invoke-gemini` — Generate text from a prompt using a fresh context.
- `gemini:continue-gemini` — Continue a conversation with the Gemini model using the existing context.

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

1.  **Tight Coupling to Google Gemini API:**
    The current implementation is hardwired directly to the Google Gemini API. All HTTP requests, authentication methods, and payload structures in `gemini.lisp` are specific to Google's backend. This makes the system inflexible and not easily adaptable to other LLM providers (e.g., Anthropic, OpenAI, Together.ai, or local models).
    *   **Roadmap:** A major future goal is to introduce a generic backend abstraction layer. This will involve defining a common "LLM client" interface and refactoring the core logic to operate against that interface. Specific API providers, including the existing Gemini implementation, will then be moved into their own backend modules that adhere to this new standard.

2.  **Global, Single-Threaded Conversation Context:**
    The conversational state is managed via a global special variable (`*context*`). This model is simple and effective for linear, single-user, single-model conversations. However, it becomes unwieldy for more complex scenarios, such as:
    *   Recursive calls to the LLM for sub-problems within a larger conversation.
    *   Orchestrating conversations between multiple models.
    *   Handling multi-user or multi-threaded interactions.
    *   **Roadmap:** The plan is to refactor the context management system away from a global state. This will likely involve introducing "conversation" objects that encapsulate their own history and state. Core functions like `invoke-gemini` would be modified to operate on a specific conversation object, allowing for a multitude of parallel, independent, and nested conversations.

Contributions and ideas for tackling these architectural improvements are highly encouraged.

## Contributing

Pull requests and issues are welcome! Please follow standard Common Lisp style and include tests for new features.
Please contact me if you have questions or need help.

## Acknowledgments

This library uses [Dexador](https://github.com/fukamachi/dexador) for HTTP requests and [cl-json](https://github.com/hankhero/cl-json) for JSON parsing.
```


