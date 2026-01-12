package org.llm4s.toolapi.builtin.search

import org.llm4s.toolapi._
import upickle.default._
import org.llm4s.config.Llm4sConfig

import java.net.URLEncoder
import scala.util.Try

case class BraveSearchConfig(
  count: Int = 5,
  safeSearch: String = "moderate",
  extraParams: Map[String, Any] = Map.empty
)
case class BraveSearchResult(
  query: String,
  results: List[BraveWebResult]
)
case class BraveWebResult(
  title: String,
  url: String,
  description: String
)

object BraveWebResult {
  implicit val braveWebResultRW: ReadWriter[BraveWebResult] = macroRW[BraveWebResult]
}

object BraveSearchResult {
  implicit val braveSearchResultRW: ReadWriter[BraveSearchResult] = macroRW[BraveSearchResult]
}

object BraveSearchTool {

  private val BraveApiUrl = "https://api.search.brave.com/res/v1/web/search"

  private def createSchema = Schema
    .`object`[Map[String, Any]]("Brave Search parameters")
    .withProperty(
      Schema.property(
        "search_query",
        Schema.string("The search query")
      )
    )

  def create(
    config: BraveSearchConfig = BraveSearchConfig()
  ): ToolFunction[Map[String, Any], BraveSearchResult] =
    ToolBuilder[Map[String, Any], BraveSearchResult](
      name = "brave_search",
      description = "Searches the web using Brave Search",
      schema = createSchema
    ).withHandler { extractor =>
      for {
        apiKey <- Llm4sConfig.braveSearchApiKey().left.map(_.message)
        query  <- extractor.getString("search_query")
        result <- search(query, config, apiKey)
      } yield result
    }.build()

  /**
   * Default Brave search tool with standard configuration.
   */
  val tool: ToolFunction[Map[String, Any], BraveSearchResult] = create()

  def withApiKey(
    apiKey: String,
    config: BraveSearchConfig = BraveSearchConfig()
  ): ToolFunction[Map[String, Any], BraveSearchResult] =
    ToolBuilder[Map[String, Any], BraveSearchResult](
      name = "brave_search",
      description = "Searches the web using Brave Search",
      schema = createSchema
    ).withHandler { extractor =>
      for {
        query  <- extractor.getString("query")
        result <- search(query, config, apiKey)
      } yield result
    }.build()

  private def search(query: String, config: BraveSearchConfig, apiKey: String): Either[String, BraveSearchResult] = {
    import sttp.client4._

    Try {
      val backend = DefaultSyncBackend()

      // Build query parameters from config
      val baseParams = Map(
        "q"          -> URLEncoder.encode(query, "UTF-8"),
        "count"      -> config.count.toString,
        "safesearch" -> config.safeSearch
      )

      // Combine with extra parameters
      val allParams = baseParams ++ config.extraParams.map { case (key, value) => key -> value.toString }

      // Build query string
      val queryString = allParams.map { case (key, value) => s"$key=$value" }.mkString("&")

      val url = s"$BraveApiUrl?$queryString"

      val response = basicRequest
        .get(uri"$url")
        .header("Accept", "application/json")
        .header("Accept-Encoding", "gzip")
        .header("X-Subscription-Token", apiKey)
        .send(backend)

      response.body match {
        case Right(body) =>
          // Parse the JSON response and extract the first result
          val json    = ujson.read(body)
          val results = json("web")("results").arr.toList

          if (results.nonEmpty) {
            val braveWebResults = results.map { result =>
              BraveWebResult(
                title = result("title").str,
                url = result("url").str,
                description = result("description").str
              )
            }
            BraveSearchResult(
              query = query,
              results = braveWebResults
            )
          } else {
            throw new Exception("No search results found")
          }
        case Left(error) =>
          throw new Exception(s"HTTP request failed: $error")
      }
    }.toEither.left.map(_.getMessage)
  }

}
