package org.llm4s.samples.agent

import org.llm4s.agent.Agent
import org.llm4s.config.Llm4sConfig
import org.llm4s.llmconnect.LLMConnect
import org.llm4s.toolapi.ToolRegistry
import org.llm4s.toolapi.builtin.search.{BraveSearchTool, SafeSearch}
import org.llm4s.toolapi.builtin.search.BraveSearchCategory
import org.llm4s.toolapi.builtin.search.BraveSearchConfig
import org.slf4j.LoggerFactory

/**
 * Researcher Agent Example - Demonstrates all four Brave Search tools in an intelligent research workflow.
 *
 * This example showcases how an agentic LLM can autonomously conduct multi-modal research by:
 * 1. Planning a research strategy for a given topic
 * 2. Executing searches across web, news, images, and videos
 * 3. Synthesizing findings into a comprehensive research report
 *
 * The agent decides which tools to use and in what order based on the research topic.
 *
 * @example
 * {{{
 * export LLM_MODEL=openai/gpt-4o
 * export OPENAI_API_KEY=sk-...
 * export BRAVE_SEARCH_API_KEY=your-brave-api-key
 * sbt "samples/runMain org.llm4s.samples.agent.ResearcherAgentExample"
 * }}}
 */
object ResearcherAgentExample {
  private val logger = LoggerFactory.getLogger(getClass)

  private val RESEARCH_TOPIC = "Climate change impacts on Arctic wildlife"

  def main(args: Array[String]): Unit = {
    logger.info("🔬 === Researcher Agent Example ===\n")
    logger.info("Research Topic: {}", RESEARCH_TOPIC)
    logger.info("=" * 70)

    // Create LLM client from configuration
    val clientResult = for {
      providerCfg <- Llm4sConfig.provider()
      client      <- LLMConnect.getClient(providerCfg)
    } yield client

    clientResult match {
      case Left(error) =>
        logger.error("Failed to create LLM client: {}", error)
        logger.error("Make sure LLM_MODEL and appropriate API key are set")
        logger.error("Example: export LLM_MODEL=openai/gpt-4o")
        return

      case Right(client) =>
        logger.info("✓ LLM client created successfully\n")

        // Create tool registry with all four Brave Search tools
        val tools = createResearchTools()
        logger.info("✓ Research tools initialized: {}", tools.map(_.name).mkString(", "))

        val registry = new ToolRegistry(tools)
        val agent    = new Agent(client)

        // Execute research workflow
        executeResearch(agent, registry)

        logger.info("\n" + "=" * 70)
        logger.info("🔬 === Research Complete ===")
    }
  }

  /**
   * Creates all four Brave Search tools using default configurations from reference.conf.
   * This demonstrates clean API usage with configuration loading.
   */
  private def createResearchTools() = Seq(
    BraveSearchTool.create(BraveSearchCategory.Web, BraveSearchConfig(count = 10)),
    BraveSearchTool.create(BraveSearchCategory.News, BraveSearchConfig(count = 10)),
    BraveSearchTool.create(BraveSearchCategory.Image, BraveSearchConfig(count = 2, safeSearch = SafeSearch.Strict)),
    BraveSearchTool.create(BraveSearchCategory.Video, BraveSearchConfig(count = 2, safeSearch = SafeSearch.Strict))
  )

  /**
   * Executes the multi-phase research workflow with the agent.
   */
  private def executeResearch(agent: Agent, registry: ToolRegistry): Unit = {
    val systemPrompt = createResearchSystemPrompt()

    val researchQuery =
      s"""Conduct comprehensive research on: "$RESEARCH_TOPIC"
         |
         |Follow this research workflow:
         |1. Use brave_web_search to gather foundational information and key facts
         |2. Use brave_news_search to find recent developments and current events
         |3. Use brave_image_search to find relevant visual references
         |4. Use brave_video_search to find explanatory videos or documentaries
         |
         |After gathering information from all sources, provide a comprehensive research summary that:
         |- Highlights key findings from each source type
         |- Identifies patterns and connections across different information modalities
         |- Presents the most important insights about the topic
         |
         |Format your final response with clear sections for Web Findings, News Updates, Visual References, Video Resources, and Research Summary.
       """.stripMargin

    logger.info("\n📊 PHASE 1: Research Planning & Execution\n")

    agent.run(researchQuery, registry, systemPromptAddition = Some(systemPrompt)) match {
      case Left(error) =>
        logger.error("Research failed: {}", error.formatted)

      case Right(state) =>
        // Display the research process
        // displayResearchProcess(state)

        // Extract and display the final research summary
        val finalResponse = state.conversation.messages
          .filter(_.role == org.llm4s.llmconnect.model.MessageRole.Assistant)
          .lastOption
          .map(_.content)
          .getOrElse("No research summary available")

        logger.info("\n" + "=" * 70)
        logger.info("📋 RESEARCH SUMMARY")
        logger.info("=" * 70)
        logger.info("\n{}\n", finalResponse)
    }
  }

  /**
   * Creates a system prompt that guides the agent's research behavior.
   */
  private def createResearchSystemPrompt(): String =
    """You are an expert research assistant with access to multiple search tools.
      |
      |Your role is to conduct thorough, multi-modal research on topics by:
      |- Using web search for foundational knowledge and comprehensive information
      |- Using news search for recent developments, current events, and trending topics
      |- Using image search to find visual references, diagrams, charts, and photographs
      |- Using video search to find explanatory content, documentaries, and presentations
      |
      |Research Guidelines:
      |1. Always use multiple search modalities to get a complete picture
      |2. Consider which type of search is most appropriate for different information needs
      |3. Synthesize findings from all sources into coherent insights
      |4. Highlight connections between different types of information
      |5. Present information in a clear, structured format
      |
      |Remember: Different search types provide complementary perspectives on the topic.
    """.stripMargin

}
