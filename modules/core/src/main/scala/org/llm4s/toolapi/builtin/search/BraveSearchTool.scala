package org.llm4s.toolapi.builtin.search

import org.llm4s.toolapi._
import upickle.default._
import org.llm4s.config.Llm4sConfig

import java.net.URLEncoder
import scala.util.Try

sealed trait BraveSearchCategory[R] {
  def endpoint: String
  def toolName: String
  def description: String
  def parseResults(json: ujson.Value, query: String): R
}

object BraveSearchCategory {
  case object Web extends BraveSearchCategory[BraveWebSearchResult] {
    val endpoint    = "web/search"
    val toolName    = "brave_search"
    val description = "Searches the web using Brave Search"

    def parseResults(json: ujson.Value, query: String): BraveWebSearchResult = {
      val results = json("web")("results").arr.toList.map { r =>
        BraveWebResult(r("title").str, r("url").str, r("description").str)
      }
      BraveWebSearchResult(query, results)
    }
  }
  case object Image extends BraveSearchCategory[BraveImageSearchResult] {
    val endpoint    = "images/search"
    val toolName    = "brave_image_search"
    val description = "Searches for images using Brave Search"

    def parseResults(json: ujson.Value, query: String): BraveImageSearchResult = {
      val results = json("results").arr.toList.map { r =>
        BraveImageResult(r("title").str, r("url").str, r("thumbnail")("src").str)
      }
      BraveImageSearchResult(query, results)
    }
  }
  case object Video extends BraveSearchCategory[BraveVideoSearchResult] {
    val endpoint    = "videos/search"
    val toolName    = "brave_video_search"
    val description = "Searches for videos using Brave Search"

    def parseResults(json: ujson.Value, query: String): BraveVideoSearchResult = {
      val results = json("results").arr.toList.map { r =>
        BraveVideoResult(r("title").str, r("url").str, r("description").str)
      }
      BraveVideoSearchResult(query, results)
    }
  }
  case object News extends BraveSearchCategory[BraveNewsSearchResult] {
    val endpoint    = "news/search"
    val toolName    = "brave_news_search"
    val description = "Searches for news using Brave Search"

    def parseResults(json: ujson.Value, query: String): BraveNewsSearchResult = {
      val results = json("results").arr.toList.map { r =>
        BraveNewsResult(r("title").str, r("url").str, r("description").str)
      }
      BraveNewsSearchResult(query, results)
    }
  }
}
sealed trait SafeSearch {
  def value: String
}

object SafeSearch {
  case object Off      extends SafeSearch { val value = "off"      }
  case object Moderate extends SafeSearch { val value = "moderate" }
  case object Strict   extends SafeSearch { val value = "strict"   }
}

case class BraveSearchConfig(
  count: Int = 5,
  safeSearch: SafeSearch = SafeSearch.Strict,
  extraParams: Map[String, Any] = Map.empty
)
case class BraveNewsSearchResult(
  query: String,
  results: List[BraveNewsResult]
)
case class BraveNewsResult(
  title: String,
  url: String,
  description: String
)
case class BraveVideoSearchResult(
  query: String,
  results: List[BraveVideoResult]
)
case class BraveVideoResult(
  title: String,
  url: String,
  description: String
)
case class BraveImageSearchResult(
  query: String,
  results: List[BraveImageResult]
)
case class BraveImageResult(
  title: String,
  url: String,
  thumbnail: String
)
case class BraveWebSearchResult(
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

object BraveWebSearchResult {
  implicit val braveWebSearchResultRW: ReadWriter[BraveWebSearchResult] = macroRW[BraveWebSearchResult]
}

object BraveImageSearchResult {
  implicit val braveImageSearchResultRW: ReadWriter[BraveImageSearchResult] = macroRW[BraveImageSearchResult]
}

object BraveImageResult {
  implicit val braveImageResultRW: ReadWriter[BraveImageResult] = macroRW[BraveImageResult]
}

object BraveVideoResult {
  implicit val braveVideoResultRW: ReadWriter[BraveVideoResult] = macroRW[BraveVideoResult]
}

object BraveVideoSearchResult {
  implicit val braveVideoSearchResultRW: ReadWriter[BraveVideoSearchResult] = macroRW[BraveVideoSearchResult]
}

object BraveNewsResult {
  implicit val braveNewsResultRW: ReadWriter[BraveNewsResult] = macroRW[BraveNewsResult]
}

object BraveNewsSearchResult {
  implicit val braveNewsSearchResultRW: ReadWriter[BraveNewsSearchResult] = macroRW[BraveNewsSearchResult]
}

object BraveSearchTool {

  private val BraveApiUrl = "https://api.search.brave.com/res/v1"

  private def createSchema = Schema
    .`object`[Map[String, Any]]("Brave Search parameters")
    .withProperty(
      Schema.property(
        "search_query",
        Schema.string("The search query")
      )
    )

  def create[R: ReadWriter](
    category: BraveSearchCategory[R] = BraveSearchCategory.Web,
    config: BraveSearchConfig = BraveSearchConfig()
  ): ToolFunction[Map[String, Any], R] =
    ToolBuilder[Map[String, Any], R](
      name = category.toolName,
      description = category.description,
      schema = createSchema
    ).withHandler { extractor =>
      for {
        apiKey <- Llm4sConfig.braveSearchApiKey().left.map(_.message)
        query  <- extractor.getString("search_query")
        result <- search(query, config, apiKey, category)
      } yield result
    }.build()

  /**
   * Default Brave search tool with standard configuration.
   */
  val braveWebSearchTool: ToolFunction[Map[String, Any], BraveWebSearchResult] = create(BraveSearchCategory.Web)
  val braveImageSearchTool: ToolFunction[Map[String, Any], BraveImageSearchResult] = create(BraveSearchCategory.Image)
  val braveVideoSearchTool: ToolFunction[Map[String, Any], BraveVideoSearchResult] = create(BraveSearchCategory.Video)
  val braveNewsSearchTool: ToolFunction[Map[String, Any], BraveNewsSearchResult] = create(BraveSearchCategory.News)

  def withApiKey[R: ReadWriter](
    apiKey: String,
    category: BraveSearchCategory[R] = BraveSearchCategory.Web,
    config: BraveSearchConfig = BraveSearchConfig()
  ): ToolFunction[Map[String, Any], R] =
    ToolBuilder[Map[String, Any], R](
      name = category.toolName,
      description = category.description,
      schema = createSchema
    ).withHandler { extractor =>
      for {
        query  <- extractor.getString("query")
        result <- search(query, config, apiKey, category)
      } yield result
    }.build()

  private def search[R](
    query: String,
    config: BraveSearchConfig,
    apiKey: String,
    category: BraveSearchCategory[R]
  ): Either[String, R] = {
    import sttp.client4._

    Try {
      val backend = DefaultSyncBackend()

      // Build query parameters from config
      val baseParams = Map(
        "q"          -> URLEncoder.encode(query, "UTF-8"),
        "count"      -> config.count.toString,
        "safesearch" -> config.safeSearch.value
      )

      // Combine with extra parameters
      val allParams = baseParams ++ config.extraParams.map { case (key, value) => key -> value.toString }

      // Build query string
      val queryString = allParams.map { case (key, value) => s"$key=$value" }.mkString("&")

      val url = s"$BraveApiUrl/${category.endpoint}?$queryString"

      val response = basicRequest
        .get(uri"$url")
        .header("Accept", "application/json")
        .header("Accept-Encoding", "gzip")
        .header("X-Subscription-Token", apiKey)
        .send(backend)

      response.body match {
        case Right(body) =>
          // Parse the JSON response and extract the first result
          val json = ujson.read(body)
          category.parseResults(json, query)
        case Left(error) =>
          throw new Exception(s"HTTP request failed: $error")
      }
    }.toEither.left.map(_.getMessage)
  }

}
