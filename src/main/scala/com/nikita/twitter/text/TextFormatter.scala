package com.nikita.twitter.text

import com.nikita.twitter.AsInt
import com.nikita.twitter.Range
import scala.util.matching.Regex

case class TextFormatter(range: Range, decorator: TextDecorator)

class TextFormattersFactory(val decoratorFactory: TextDecoratorsFactory) {
  private val textDecoratorRegex: Regex = "positions (\\d+) through (\\d+) -> (.*)".r

  def parse(arg: String): TextFormatter = arg match {
    case textDecoratorRegex(AsInt(left), AsInt(right), name) => TextFormatter(Range(left, right), decoratorFactory.create(name))
    case _ => throw TweetDecoratorFormatException(arg)
  }
}

object TextFormattersFactory {
  def apply(): TextFormattersFactory = new TextFormattersFactory(DefaultDecoratorFactory())
}

case class TweetDecoratorFormatException(arg: String) extends RuntimeException(s"Cannot parse: $arg")
