package org.llm4s.toolapi.builtin

/**
 * Search tools for web searches and lookups.
 *
 * These tools provide web search capabilities.
 *
 * == Available Tools ==
 *
 * - [[DuckDuckGoSearchTool]]: Search using DuckDuckGo Instant Answer API
 *   - Best for definitions, facts, quick lookups
 *   - No API key required
 *   - Returns abstracts, related topics, and infobox data
 *
 * - [[BraveSearchTool]]: Full web search using Brave Search API
 *   - Best for comprehensive web search results
 *   - Requires `BRAVE_SEARCH_API_KEY`
 *   - Returns snippets with title and URL
 *
 * @example
 * {{{
 * import org.llm4s.toolapi.builtin.search._
 * import org.llm4s.toolapi.ToolRegistry
 *
 * // Default search tool
 * val searchTool = DuckDuckGoSearchTool.tool
 *
 * // Brave search tool 
 * val braveSearch = BraveSearchTool.tool
 *
 * val tools = new ToolRegistry(Seq(searchTool, braveSearch))
 * }}}
 */
package object search {

  /**
   * All search tools with default configuration.
   */
  val allTools: Seq[org.llm4s.toolapi.ToolFunction[_, _]] = Seq(
    DuckDuckGoSearchTool.tool,
    BraveSearchTool.tool
  )
}
