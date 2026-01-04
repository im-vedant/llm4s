package org.llm4s.samples.mcp

import org.llm4s.mcp._
import org.llm4s.toolapi._
import org.llm4s.toolapi.tools._
import org.slf4j.LoggerFactory
import scala.concurrent.duration._
import pureconfig._
import pureconfig.ConfigReader

/**
 * Example demonstrating basic MCP tool usage with automatic fallback
 *
 * This example shows how to use MCP tools directly via the tool registry.
 * The client automatically detects the best transport protocol with fallback.
 *
 * Start the MCPServer first: sbt "samples/runMain org.llm4s.samples.mcp.DemonstrationMCPServer"
 * Then run: sbt "samples/runMain org.llm4s.samples.mcp.MCPToolExample"
 */
object MCPToolExample {
  private val logger = LoggerFactory.getLogger(getClass)

  /** Configuration for the MCP client example. */
  case class McpClientConfig(
    name: String,
    port: Int,
    path: String,
    timeout: FiniteDuration,
    cacheTTL: FiniteDuration
  )

  /** Wrapper for the mcp-client section in application.conf. */
  case class AppSamplesConfig(mcpClient: McpClientConfig)

  /** Wrapper for the samples section in application.conf. */
  case class AppSamplesWrapper(samples: AppSamplesConfig)

  /** Root wrapper for the llm4s configuration hierarchy. */
  case class AppConfig(llm4s: AppSamplesWrapper)

  private val mcpClientReader: ConfigReader[McpClientConfig] =
    ConfigReader.forProduct5("name", "port", "path", "timeout", "cache-ttl")(McpClientConfig.apply)

  private val appSamplesConfigReader: ConfigReader[AppSamplesConfig] =
    ConfigReader.forProduct1("mcp-client")(AppSamplesConfig.apply)(mcpClientReader)

  private val appSamplesWrapperReader: ConfigReader[AppSamplesWrapper] =
    ConfigReader.forProduct1("samples")(AppSamplesWrapper.apply)(appSamplesConfigReader)

  implicit private val appConfigReader: ConfigReader[AppConfig] =
    ConfigReader.forProduct1("llm4s")(AppConfig.apply)(appSamplesWrapperReader)

  def main(args: Array[String]): Unit = {
    logger.info("🚀 MCP Tool Example - Basic MCP Tool Usage")

    // Load the configuration from application.conf using PureConfig
    val config = ConfigSource.default.load[AppConfig] match {
      case Right(conf) => conf.llm4s.samples.mcpClient
      case Left(failures) =>
        logger.error(s"Failed to load configuration: ${failures.prettyPrint()}")
        sys.exit(1)
    }

    // Create MCP server configuration using loaded settings
    val serverConfig = MCPServerConfig.streamableHTTP(
      name = config.name,
      url = s"http://localhost:${config.port}${config.path}",
      timeout = config.timeout
    )

    // Create local weather tool for comparison
    val localWeatherTool = WeatherTool.tool

    // Create MCP registry combining local and MCP tools using loaded settings
    val mcpRegistry = new MCPToolRegistry(
      mcpServers = Seq(serverConfig),
      localTools = Seq(localWeatherTool),
      cacheTTL = config.cacheTTL
    )

    // Show all available tools
    val allTools = mcpRegistry.getAllTools
    logger.info(s"📦 Available tools (${allTools.size} total):")
    allTools.zipWithIndex.foreach { case (tool, index) =>
      val source = if (tool.description.contains("local")) "local" else "MCP"
      logger.info(s"   ${index + 1}. ${tool.name} ($source): ${tool.description}")
    }
    logger.info("ℹ️  Note: Local tools take precedence over MCP tools with same name")

    // Test the tools
    runToolTests(mcpRegistry)

    // Clean up
    mcpRegistry.closeMCPClients()
    logger.info("✨ Example completed!")
  }

  private def runToolTests(registry: MCPToolRegistry): Unit = {
    logger.info("🧪 Testing MCP tools:")

    // Test 1: Weather tool (local version)
    logger.info("1️⃣ Testing local weather tool:")
    val localWeatherRequest = ToolCallRequest(
      functionName = "get_weather",
      arguments = ujson.Obj(
        "location" -> ujson.Str("London, UK"),
        "units"    -> ujson.Str("celsius")
      )
    )

    executeAndLog(registry, localWeatherRequest, "   ")

    // Test 2: Currency conversion (MCP tool)
    logger.info("2️⃣ Testing MCP currency conversion:")
    val currencyRequest = ToolCallRequest(
      functionName = "currency_convert",
      arguments = ujson.Obj(
        "amount"        -> ujson.Num(100),
        "from_currency" -> ujson.Str("USD"),
        "to_currency"   -> ujson.Str("EUR")
      )
    )

    executeAndLog(registry, currencyRequest, "   ")

    // Test 3: Weather tool (local tool will be used due to name conflict)
    logger.info("3️⃣ Testing weather tool (local tool takes precedence):")
    val mcpWeatherRequest = ToolCallRequest(
      functionName = "get_weather",
      arguments = ujson.Obj(
        "location" -> ujson.Str("Tokyo, Japan"),
        "units"    -> ujson.Str("celsius")
      )
    )

    executeAndLog(registry, mcpWeatherRequest, "   ")
  }

  private def executeAndLog(registry: MCPToolRegistry, request: ToolCallRequest, indent: String): Unit =
    registry.execute(request) match {
      case Right(result) =>
        logger.info(s"${indent}✅ Success: ${result.render()}")
      case Left(error) =>
        logger.error(s"${indent}❌ Failed: $error")
    }
}
