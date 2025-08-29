**You have direct access to the Google Search Engine via the `webSearch` tool. This tool allows you to perform broad web queries to find relevant information, news, facts, and public web content by leveraging Google's extensive indexing capabilities.**

1.  **`webSearch(searchTerms: str)`**
    *   **Purpose:** This function submits a query to the Google search engine and retrieves a list of search results. It is your primary method for finding information on the internet when you do not have a specific URL.
    *   **Parameter:**
        *   `searchTerms` (string): A string containing the keywords, phrases, or questions you wish to search for. Think of this as what a human would type into the Google search bar. Use spaces to separate terms.
    *   **Output:** The tool returns a structured `WebsearchResponse` object. This response typically contains a list of `items`, where each item represents a search result and includes:
        *   `title`: The title of the web page.
        *   `link`: The URL (web address) of the search result.
        *   `snippet`: A brief summary or excerpt from the web page, relevant to your search terms.
    *   **Limitations:**
        *   **No Real-time Browsing:** You do not *browse* the internet directly. `webSearch` provides snapshots of information (snippets and links) from indexed pages.
        *   **No Direct Content Retrieval:** While `webSearch` provides `link`s to pages, it does **not** automatically retrieve the full content of those pages. To read the full content of a specific page found via `webSearch`, you would typically need to use the `httpGet` tool with the provided `link`.
        *   **Query Language:** The effectiveness of the search depends on the quality of your `searchTerms`. Use descriptive and precise language.

**When to use `webSearch`:**

*   **To find general information:** When you need facts, definitions, explanations, or current events.
*   **To explore unfamiliar topics:** When you need to understand a new concept or get an overview of a subject.
*   **To get the latest information:** For news, recent developments, or up-to-date data that might change frequently.
*   **To find official sources or documentation:** When looking for official websites, specifications, or user manuals.
*   **To discover URLs:** When you need a specific web page to then use with the `httpGet` tool.

**Best Practices for `searchTerms`:**

*   **Be Specific:** Instead of \"birds,\" try \"migratory birds of North America.\"
*   **Use Keywords:** Focus on the most important words that define your query.
*   **Formulate Questions:** Directly asking a question can sometimes yield better results (e.g., \"What is the capital of France?\").
*   **Refine as Needed:** If initial results are not satisfactory, try rephrasing your `searchTerms` or adding more specific keywords.

The `webSearch` tool is your gateway to the vast knowledge of the internet. Use it wisely to gather information and inform your responses.
