package com.nikita.twitter.tweet

import com.nikita.twitter.Covers
import com.nikita.twitter.Inside
import com.nikita.twitter.None
import com.nikita.twitter.Partial
import com.nikita.twitter.Range
import com.nikita.twitter.text.EmptyTextDecorator
import com.nikita.twitter.text.TextDecorator
import com.nikita.twitter.text.TextFormatter
import scala.annotation.tailrec

case class FormattedTweet(source: String, formatters: Seq[TextFormatter]) {
  val tweet: String = makeTweet

  private def makeTweet: String = {
    val tree = TweetTree(formatters, source.length)
    TweetTree.decorate(tree, source)
  }
}

sealed trait TweetTree {
  def range: Range
  def decorator: TextDecorator
}

case class TweetNode(range: Range, decorator: TextDecorator, childs: List[TweetTree]) extends TweetTree
object TweetNode {
  def apply(decorator: TextFormatter, childs: List[TweetTree]): TweetNode = new TweetNode(decorator.range, decorator.decorator, childs)
}

case class TweetLeaf(range: Range, decorator: TextDecorator) extends TweetTree
object TweetLeaf {
  def apply(tweetDecorator: TextFormatter): TweetLeaf = new TweetLeaf(tweetDecorator.range, tweetDecorator.decorator)
}

object TweetTree {
  def apply(decorators: Seq[TextFormatter], length: Int): TweetTree = {
    //Make appropriate order for build tree: oreder by left border and reverse [1][[2][3][4]5][6]
    val orderedDecorators = decorators.sortWith(treeOrder)
    build(orderedDecorators, length)
  }

  def decorate(tree: TweetTree, source: String): String = tree match {
    case TweetNode(range, decorator, childs) =>
      //Decorate string which is a result of childs
      decorator.decorate(
        //For unification we adding EmptyLeaf between formatters
        childs.foldLeft(List[TweetTree]())
          ((trees: List[TweetTree], current: TweetTree) => current :: emptyLeaf(range, trees, current) :: trees).reverse
        //Decorate childs trees
        .map((tree: TweetTree) => decorate(tree, source))
        //Concat results
        .reduce(_ + _)
        //fix suffix
        + source.substring(childs.last.range.right, range.right)
      )
    case TweetLeaf(range, decorator) => decorator.decorate(source.substring(range.left, range.right))
  }

  private def emptyLeaf(range: Range, trees: List[TweetTree], current: TweetTree) =
    TweetLeaf(Range(lastOrLeft(trees, range), current.range.left), EmptyTextDecorator)
  private def lastOrLeft(trees: List[TweetTree], range: Range) =
    if (trees.nonEmpty) trees.head.range.right else range.left

  //Tree order is intervals are ordered by right end
  private def treeOrder(d1: TextFormatter, d2: TextFormatter): Boolean = {
    val diff = d1.range.right - d2.range.right
    if (diff < 0 ) true
    else if (diff > 0) false
    //if ends are equal we need smaller first
    else d1.range.left > d2.range.left
  }

  private def build(decorators: Seq[TextFormatter], sourceLength: Int): TweetTree = {

    @tailrec
    def combineLeafs(decorator: TextFormatter, acc: List[TweetTree], res: List[TweetTree]): List[TweetTree] = acc.head.range.intersect(decorator.range) match {
      case Inside => combineLeafs(decorator, acc.tail, acc.head :: res)
      case None => TweetNode(decorator, res) :: acc
      case Partial => throw CrossDecoratorsException()
      case Covers => throw new IllegalArgumentException("Decorators ordered wrong")
    }

    TweetNode(Range(0, sourceLength), EmptyTextDecorator, decorators.foldLeft(List[TweetTree]())((acc: List[TweetTree], decorator: TextFormatter) => acc match {
      case Nil => List(TweetLeaf(decorator))
      case head :: _ => head.range.intersect(decorator.range) match {
        case Inside => combineLeafs(decorator, acc, List())
        case None => TweetLeaf(decorator) :: acc
        case Partial => throw CrossDecoratorsException()
        case Covers => throw new IllegalArgumentException("Decorators ordered wrong")
      }
    }).reverse)
  }
}

case class CrossDecoratorsException() extends RuntimeException("Decorators should not cross")
