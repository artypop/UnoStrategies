package strategies

import entities.{Card, Colors}
import entities.Colors.Color
import services.PlayerInfos
import tools.CustomArray._

import scala.util.Random

object OtherRandomStrategy extends Strategy {

  val r = new Random()

  override def choseCard(baseDecision: PlayerInfos): Option[Int] = {

    val playableCards: Array[(Card, Int)] = baseDecision.allCards.zipWithIndex.filter {

      card => card._1.isPlayableWith(baseDecision.currentCard, baseDecision.currentChosenColor)

    }

    if (playableCards.isEmpty) None

    else {

      if (isSuperJokerPlayable(baseDecision)) {
        val randomIndex: Int = r.nextInt(playableCards.length)

        Some(playableCards(randomIndex)._2)

      } else {

        val noJoker = playableCards.filter(_._1.cardValue.isSuperJoker == false)

        val randomIndex: Int = r.nextInt(noJoker.length)
        Some(noJoker(randomIndex)._2)

      }

    }
  }

  override def choseColor(baseDecision: PlayerInfos): Color = {


    baseDecision.allCards.flatMap(_.color)
      .groupBy(identity)
      .mapValues(_.length)
      .toArray
      .maxOptionBy(_._2)
      .map(_._1)
      .getOrElse {
        val colors = Array(Colors.Bleu, Colors.Vert, Colors.Rouge, Colors.Jaune)
        colors(r.nextInt(4))
      }
  }

}
