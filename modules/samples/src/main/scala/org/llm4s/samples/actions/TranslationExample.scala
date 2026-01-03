package org.llm4s.samples.actions

import org.llm4s.config.Llm4sConfig
import org.llm4s.llmconnect.LLMConnect
import org.llm4s.llmconnect.model._
import org.llm4s.llmconnect.LLMClient
import org.llm4s.types.Result

/**
 * Example demonstrating how to use LLM4S for multi-tonal text translation.
 *
 * This example shows how to:
 * 1. Create a reusable translation function with tonal control (Formal/Informal)
 * 2. Dynamically modify system prompts based on user context
 * 3. Handle translation results using type-safe Result patterns
 */
object TranslationExample {
  val textToTranslate = "Hello, how are you?"
  val language        = "French"
  val systemPrompt =
    """You are a helpful assistant specialized in translating text.
      |When given text, provide a translation that:
      |- Accurately conveys the meaning of the original text
      |- Maintains the original tone and style
      |- Sounds natural and idiomatic in the target language
      |
      |Return only the translated text without additional commentary.""".stripMargin
  enum Tone:
    case Informal
    case Formal
  def main(args: Array[String]): Unit = {
    println("Translation Example")
    println("-------------------------")
    println(s"Text to translate: $textToTranslate")
    println(s"Target Language: $language")
    println("-------------------------")

    val result = for {
      providerCfg <- Llm4sConfig.provider()
      client      <- LLMConnect.getClient(providerCfg)

      // Demonstrate both tones
      informal <- translate(textToTranslate, language, Tone.Informal)(client)
      formal   <- translate(textToTranslate, language, Tone.Formal)(client)
    } yield (informal, formal)

    result.fold(
      err => println(s"Error: ${err.formatted}"),
      (informal, formal) => {
        println(s"Informal ($language): $informal")
        println(s"Formal   ($language): $formal")
        println("-------------------------")
      }
    )
  }

  def translate(text: String, language: String, tone: Tone)(client: LLMClient): Result[String] = {
    val tonalInstruction = tone match {
      case Tone.Formal   => "Use formal language (e.g., 'vous' in French, 'Sie' in German)."
      case Tone.Informal => "Use informal/casual language (e.g., 'tu' in French, 'du' in German)."
    }

    val finalSystemPrompt =
      s"""$systemPrompt
         |
         |Specific Instruction: $tonalInstruction""".stripMargin

    val conversation = Conversation(
      Seq(
        SystemMessage(finalSystemPrompt),
        UserMessage(s"Translate to $language: $text")
      )
    )
    client.complete(conversation).map(_.message.content.trim())
  }
}
