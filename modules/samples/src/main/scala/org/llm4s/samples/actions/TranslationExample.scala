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
 *
 * // Run: sbt "samples/runMain org.llm4s.samples.actions.TranslationExample"
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

  sealed trait Tone
  object Tone {
    case object Informal extends Tone
    case object Formal   extends Tone
  }
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
      { case (informal, formal) =>
        println(s"Informal ($language): $informal")
        println(s"Formal   ($language): $formal")
        println("-------------------------")
      }
    )
  }

  /**
   * Translates text to the specified language with tonal control.
   *
   * @param text     The text to translate
   * @param language The target language
   * @param tone     The formality level (Formal/Informal)
   * @param client   The LLM client to use
   * @return Either an error or the translated text
   */
  def translate(text: String, language: String, tone: Tone)(client: LLMClient): Result[String] = {
    val tonalInstruction = tone match {
      case Tone.Formal   => "Use formal language (e.g., 'vous' in French, 'Sie' in German, 'usted' in Spanish)."
      case Tone.Informal => "Use informal/casual language (e.g., 'tu' in French, 'du' in German, 'tú' in Spanish)."
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
