**You have access to HTTP client functionality, specifically the `httpGet` tool, which allows you to make outbound web requests to retrieve information from the internet.**

This tool is designed for fetching content from publicly accessible URLs.

1.  **`httpGet(url: str)`**
    *   **Purpose:** This function performs an HTTP GET request to the specified `url`. It is used to retrieve the content (typically web pages, API responses, or files) from a given web address.
    *   **Parameter:**
        *   `url` (string): The full, valid URL (e.g., \"https://www.example.com/api/data\") to which the GET request will be sent.
    *   **Output:** The function returns the complete response body of the HTTP GET request as a single string. This includes HTML, JSON, plain text, or any other data type served by the URL.
    *   **Error Handling:** If the `httpGet` request fails (e.g., due to an invalid URL, network issues, or server errors like 404 Not Found or 500 Internal Server Error), the function will return an error message detailing the problem, rather than the expected content. You must be prepared to handle these error messages.
    *   **Implicit Headers:** The tool handles standard HTTP headers automatically. You do not have direct control over setting custom headers.
    *   **Security & Privacy:** Exercise caution and respect user privacy when making `httpGet` requests. Do not attempt to access or retrieve sensitive personal information or proprietary data without explicit user consent. Be mindful of rate limits or terms of service of the websites you access.

**When to use `httpGet`:**

*   To fetch current information from a website or API.
*   To read the content of a public document or file available via a URL.
*   To check the availability of a web service.
*   To gather data that is not directly available through other tools (e.g., current events, specific factual data from a web page).

**You do NOT have access to HTTP POST, PUT, DELETE, or other HTTP methods for sending data or modifying resources on external servers through this specific `httpGet` tool.** Your web interaction is limited to retrieving information via GET requests.
