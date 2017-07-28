package com.nikita.twitter.text

trait TextDecorator {
  def decorate(source: String): String
}

case object EmptyTextDecorator extends TextDecorator {
  override def decorate(source: String): String = source
  val name: String = "Empty"
}

case object EntityTextDecorator extends TextDecorator {
  override def decorate(source: String): String = s"<strong>$source</strong>"
  val name: String = "Entity"
}

case object TwitterUsernameTextDecorator extends TextDecorator {
  override def decorate(source: String): String = s"""<a href="http://twitter.com/$source">$source</a>"""
  val name: String = "Twitter username"
}

case object LinkTextDecorator extends TextDecorator {
  override def decorate(source: String): String = s"""<a href="$source">$source</a>"""
  val name: String = "Link"
}
