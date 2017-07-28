package com.nikita.twitter.text

//Factory may have type parameter for change id type, but I think that this is out of scope now but can be easy extended in future
trait TextDecoratorsFactory {
  def create(id: String): TextDecorator
}

class DefaultDecoratorFactory(val handlers: scala.collection.mutable.Map[String, TextDecorator]) extends TextDecoratorsFactory {
  override def create(id: String): TextDecorator = handlers(id)
}

object DefaultDecoratorFactory {
  def apply(): TextDecoratorsFactory = new DefaultDecoratorFactory(
    scala.collection.mutable.Map(
      EmptyTextDecorator.name -> EmptyTextDecorator,
      EntityTextDecorator.name -> EntityTextDecorator,
      TwitterUsernameTextDecorator.name -> TwitterUsernameTextDecorator,
      LinkTextDecorator.name -> LinkTextDecorator))
}