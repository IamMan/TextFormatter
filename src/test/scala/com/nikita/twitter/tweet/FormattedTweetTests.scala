package com.nikita.twitter.tweet

import com.nikita.twitter.text.TextFormatter
import com.nikita.twitter.text.TextFormattersFactory
import org.scalatest.FunSuite
import org.scalatest.Matchers

class FormattedTweetTests extends FunSuite with Matchers {
  test("Example test") {
    val source = "Obama visited Facebook headquarters: http://bit.ly/xyz @elversatile"
    val formattersDescriptors = Seq(
      "positions 14 through 22 -> Entity",
      "positions 0 through 5 -> Entity",
      "positions 56 through 67 -> Twitter username",
      "positions 37 through 54 -> Link"
    )

    val formattersFactory = TextFormattersFactory()
    val formatters: Seq[TextFormatter] = formattersDescriptors.map(formattersFactory.parse)
    val formattedTweet = FormattedTweet(source, formatters)
    formattedTweet.tweet shouldBe """<strong>Obama</strong> visited <strong>Facebook</strong> headquarters: <a href="http://bit.ly/xyz">http://bit.ly/xyz</a> @<a href="http://twitter.com/elversatile">elversatile</a>"""
  }

  test("Enclosed decorators ") {
    val source = "italic link1 link2 @name"
    val formattersDescriptors = Seq(
      "positions 0 through 6 -> Entity",
      "positions 7 through 12 -> Link",
      "positions 13 through 18 -> Link",
      "positions 7 through 18 -> Entity",
      "positions 20 through 24 -> Twitter username",
    )

    val formattersFactory = TextFormattersFactory()
    val formatters: Seq[TextFormatter] = formattersDescriptors.map(formattersFactory.parse)
    val formattedTweet = FormattedTweet(source, formatters)
    formattedTweet.tweet shouldBe """<strong>italic</strong> <strong><a href="link1">link1</a> <a href="link2">link2</a></strong> @<a href="http://twitter.com/name">name</a>"""
  }

  test("Crossed decorators") {
    val source = "klasdjfkladsjfklasdjflfkldsakjf"
    val formattersDescriptors = Seq(
      "positions 10 through 20 -> Empty",
      "positions 11 through 19 -> Empty",
      "positions 12 through 15 -> Empty",
      "positions 13 through 16 -> Empty",
    )

    val formattersFactory = TextFormattersFactory()
    val formatters: Seq[TextFormatter] = formattersDescriptors.map(formattersFactory.parse)
    assertThrows[CrossDecoratorsException] {
      FormattedTweet(source, formatters)
    }
  }

}
