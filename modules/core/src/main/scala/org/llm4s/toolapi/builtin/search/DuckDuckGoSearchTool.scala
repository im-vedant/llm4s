package org.llm4s.toolapi.builtin.search

import org.llm4s.toolapi._
import upickle.default._

import java.net.URLEncoder
import scala.util.Try

/**
 * A related topic from web search.
 */
case class RelatedTopic(
  text: String,
  url: Option[String]
)

object RelatedTopic {
  implicit val relatedTopicRW: ReadWriter[RelatedTopic] = macroRW[RelatedTopic]
}

/**
 * DuckDuckGo search result.
 */
case class DuckDuckGoSearchResult(
  query: String,
  abstract_ : String,
  abstractSource: String,
  abstractUrl: String,
  answer: String,
  answerType: String,
  relatedTopics: Seq[RelatedTopic],
  infoboxContent: Option[String]
)

object DuckDuckGoSearchResult {
  implicit val duckDuckGoSearchResultRW: ReadWriter[DuckDuckGoSearchResult] = macroRW[DuckDuckGoSearchResult]
}

/**
 * Configuration for DuckDuckGo search tool.
 *
 * @param timeoutMs Request timeout in milliseconds.
 * @param maxResults Maximum number of related topics to return.
 * @param safeSearch Whether to enable safe search.
 */
case class DuckDuckGoSearchConfig(
  timeoutMs: Int = 10000,
  maxResults: Int = 10,
  safeSearch: Boolean = true
)

/**
 * Tool for web searching using DuckDuckGo's Instant Answer API.
 *
 * This tool provides quick answers and definitions without requiring an API key.
 * It's best suited for factual queries, definitions, and quick lookups.
 *
 * Note: This uses DuckDuckGo's free Instant Answer API which provides:
 * - Definitions from Wikipedia
 * - Quick facts
 * - Related topics
 * - Disambiguation pages
 *
 * It does NOT provide full web search results (that would require a paid API).
 *
 * @example
 * {{{
 * import org.llm4s.toolapi.builtin.search._
 *
 * val searchTool = DuckDuckGoSearchTool.create()
 *
 * val tools = new ToolRegistry(Seq(searchTool))
 * agent.run("What is Scala programming language?", tools)
 * }}}
 */
object DuckDuckGoSearchTool {

  private val DuckDuckGoApiUrl = "https://api.duckduckgo.com/"

  private def createSchema = Schema
    .`object`[Map[String, Any]]("DuckDuckGo search parameters")
    .withProperty(
      Schema.property(
        "search_query",
        Schema.string("The search query (best for definitions, facts, quick lookups)")
      )
    )

  /**
   * Create a DuckDuckGo search tool with the given configuration.
   */
  def create(
    config: DuckDuckGoSearchConfig = DuckDuckGoSearchConfig()
  ): ToolFunction[Map[String, Any], DuckDuckGoSearchResult] =
    ToolBuilder[Map[String, Any], DuckDuckGoSearchResult](
      name = "duckduckgo_search",
      description = "Search the web for definitions, facts, and quick answers using DuckDuckGo. " +
        "Best for factual queries and definitions. Does not provide full web search results. " +
        s"Timeout: ${config.timeoutMs}ms.",
      schema = createSchema
    ).withHandler { extractor =>
      for {
        searchQuery <- extractor.getString("search_query")
        result      <- search(searchQuery, config)
      } yield result
    }.build()

  /**
   * Default DuckDuckGo search tool with standard configuration.
   */
  val tool: ToolFunction[Map[String, Any], DuckDuckGoSearchResult] = create()
  val SAFE_SEARCH = "1"
  val UNSAFE_SEARCH = "-1"
  private def search(
    query: String,
    config: DuckDuckGoSearchConfig
  ): Either[String, DuckDuckGoSearchResult] = {
    import sttp.client4._

    Try {
      val backend = DefaultSyncBackend()

      val encodedQuery = URLEncoder.encode(query, "UTF-8")
      val safeSearch   = if (config.safeSearch) SAFE_SEARCH else UNSAFE_SEARCH
      val url =
        s"$DuckDuckGoApiUrl?q=$encodedQuery&format=json&no_html=1&skip_disambig=0&t=llm4s&safesearch=$safeSearch"

      val response = basicRequest
        .get(uri"$url")
        .header("User-Agent", "llm4s-duckduckgo-search/1.0")
        .readTimeout(scala.concurrent.duration.Duration(config.timeoutMs, "ms"))
        .send(backend)

      response.body match {
        case Right(body) =>
          // Parse the JSON response
          val json = ujson.read(body)

          val relatedTopics = json.obj
            .get("RelatedTopics")
            .map { topics =>
              topics.arr
                .take(config.maxResults)
                .flatMap { topic =>
                  topic.obj.get("Text").map { text =>
                    RelatedTopic(
                      text = text.str,
                      url = topic.obj.get("FirstURL").map(_.str)
                    )
                  }
                }
                .toSeq
            }
            .getOrElse(Seq.empty)

          val infobox = json.obj.get("Infobox").flatMap { infobox =>
            infobox.obj.get("content").map { content =>
              content.arr
                .map { item =>
                  val label = item.obj.get("label").map(_.str).getOrElse("")
                  val value = item.obj.get("value").map(_.str).getOrElse("")
                  s"$label: $value"
                }
                .mkString("\n")
            }
          }

          DuckDuckGoSearchResult(
            query = query,
            abstract_ = json.obj.get("Abstract").map(_.str).getOrElse(""),
            abstractSource = json.obj.get("AbstractSource").map(_.str).getOrElse(""),
            abstractUrl = json.obj.get("AbstractURL").map(_.str).getOrElse(""),
            answer = json.obj.get("Answer").map(_.str).getOrElse(""),
            answerType = json.obj.get("AnswerType").map(_.str).getOrElse(""),
            relatedTopics = relatedTopics,
            infoboxContent = infobox
          )
        case Left(error) =>
          throw new Exception(s"HTTP request failed: $error")
      }
    }.toEither.left.map(e => s"DuckDuckGo search failed: ${e.getMessage}")
  }
}
