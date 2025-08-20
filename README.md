# Common Lisp library to access Google Gemini LLM APIs

This library provides a Common Lisp interface to Google's Gemini Large Language Models.

## Installation

You can install the `gemini` library using Quicklisp, ASDF, or manually:

### Using Quicklisp
```common-lisp
(ql:quickload "gemini")
```

### Using ASDF
```common-lisp
(asdf:load-system "gemini")
```

### Manual Installation
Clone this repository into your local-projects directory:
```bash
git clone https://github.com/jrm-code-project/gemini.git ~/quicklisp/local-projects/gemini
```
Then load with Quicklisp or ASDF as above.

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
- `alexandria`
- `asdf`
- `cl-json`
- `dexador`
- `fold`
- `function`
- `named-let`
- `trivial-backtrace`
- `uiop`

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

- `gemini:invoke-gemini` — Generate text from a prompt.
- `gemini:gemini-continue` — Continue a conversation with the Gemini model.

See the source for additional utility functions.

### Basic use

To generate text from a prompt:
```common-lisp
(gemini:invoke-gemini "In one sentence, explain how AI works to a child.")
;; => "AI is like a super smart computer brain that learns from information to answer questions and do tasks."
```

### Continuing a conversation
```common-lisp
(gemini:gemini-continue "What happens next?")
```

### Model selection
```common-lisp
(gemini:invoke-gemini "Translate to French: Hello!" :model "gemini-2.5-pro")
```
## Configuration

You can configure the API key and default model using environment variables or files as described above. The following environment variables are supported:

- `GOOGLE_API_KEY` — Your API key

## Troubleshooting

- Ensure your API key is valid and accessible.
- Check that all dependencies are loaded.
- For network errors, verify your internet connection and proxy settings.
- For authentication errors, confirm the API key location and permissions.

## License

See `LICENSE` for details. This project is licensed under the MIT License.

## Contributing

Pull requests and issues are welcome! Please follow standard Common Lisp style and include tests for new features.

## Acknowledgments

This library uses [Dexador](https://github.com/fukamachi/dexador) for HTTP requests and [cl-json](https://github.com/hankhero/cl-json) for JSON parsing.
```


