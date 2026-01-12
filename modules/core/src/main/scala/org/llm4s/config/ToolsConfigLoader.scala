package org.llm4s.config

import org.llm4s.error.ConfigurationError
import org.llm4s.types.Result
import pureconfig.{ ConfigReader => PureConfigReader, ConfigSource }

/**
 * Internal PureConfig-based loader for tools configuration.
 *
 * This loader follows the same pattern as ProviderConfigLoader and EmbeddingsConfigLoader
 * to provide a consistent, scalable approach for loading tool API keys and configurations.
 *
 * External code should use Llm4sConfig.braveSearchApiKey() rather than this object directly.
 */
private[config] object ToolsConfigLoader {

  final private case class BraveSection(
    apiKey: Option[String]
  )

  final private case class ToolsRoot(
    brave: Option[BraveSection]
  )

  // ---- PureConfig readers for internal shapes ----

  implicit private val braveSectionReader: PureConfigReader[BraveSection] =
    PureConfigReader.forProduct1("apiKey")(BraveSection.apply)

  implicit private val toolsRootReader: PureConfigReader[ToolsRoot] =
    PureConfigReader.forProduct1("brave")(ToolsRoot.apply)

  // ---- Public API used by Llm4sConfig ----

  /**
   * Load Brave Search API key from configuration.
   *
   * Checks for the API key in the following order:
   * 1. Environment variable: BRAVE_SEARCH_API_KEY
   * 2. System properties
   * 3. application.conf: llm4s.tools.brave.apiKey
   * 4. reference.conf: llm4s.tools.brave.apiKey
   *
   * @param source The configuration source to load from
   * @return Right(apiKey) if found and non-empty, Left(ConfigurationError) otherwise
   */
  def loadBraveApiKey(source: ConfigSource): Result[String] = {
    val rootEither = source.at("llm4s.tools").load[ToolsRoot]

    rootEither.left
      .map { failures =>
        val msg = failures.toList.map(_.description).mkString("; ")
        ConfigurationError(s"Failed to load llm4s tools config via PureConfig: $msg")
      }
      .flatMap { root =>
        val apiKeyOpt = root.brave.flatMap(_.apiKey).map(_.trim).filter(_.nonEmpty)
        apiKeyOpt.toRight(
          ConfigurationError(
            "Missing Brave Search API key (llm4s.tools.brave.apiKey / BRAVE_SEARCH_API_KEY)"
          )
        )
      }
  }

}
