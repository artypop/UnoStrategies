package strategies

import entities.Colors.Color
import entities.{Card, Colors}
import services.PlayerInfos
import tools.CustomArray._

import scala.util.Random

trait Strategy {

  def choseCard(playerInfos: PlayerInfos): Option[Int]

  def choseColor(playerInfos: PlayerInfos): Color

  def choseColorRandomly(): Color = {

    val r = new Random()
    val colors = Array(Colors.Bleu, Colors.Vert, Colors.Rouge, Colors.Jaune)
    colors(r.nextInt(4))

  }

  def isSuperJokerPlayable(baseDecision: PlayerInfos): Boolean = {

    !baseDecision.allCards.exists(_.color == baseDecision.currentCard.color) |
      !baseDecision.allCards.exists(_.color == baseDecision.currentChosenColor)

  }

  def chooseColorFirstCard(setCards: Array[Card]): Color = {


    setCards.flatMap(_.color)
      .groupBy(identity)
      .mapValues(_.length)
      .toArray
      .maxOptionBy(_._2)
      .map(_._1)
      .getOrElse {
        val r = new Random()
        val colors = Array(Colors.Bleu, Colors.Vert, Colors.Rouge, Colors.Jaune)
        colors(r.nextInt(4))
      }

  }
}