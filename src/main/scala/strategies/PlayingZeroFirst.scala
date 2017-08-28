package strategies

import entities.{Card, Colors}
import entities.Colors.Color
import services.PlayerInfos
import tools.CustomArray._

import scala.util.Random

object PlayingZeroFirst extends Strategy {

  val r = new Random()

  override def choseCard(baseDecision: PlayerInfos): Option[Int] = {

    val playableCards: Array[(Card, Int)] = baseDecision.allCards.zipWithIndex.filter {

      card => card._1.isPlayableWith(baseDecision.currentCard, baseDecision.currentChosenColor)

    }
    if (playableCards.isEmpty) None else {

      if (playableCards.exists(x => x._1.cardValue.isSuperJoker) & isSuperJokerPlayable(baseDecision)) {
        playableCards.maxOptionBy(_._1.cardValue.score).map(_._2)
      }
      else {

        if (playableCards.exists(x => x._1.cardValue.score == 0)) {

          val carteZero: Array[(Card, Int)] = playableCards.filter(_._1.cardValue.score == 0)
          Some(carteZero(0)._2)

        }
        else
          playableCards.filter(_._1.cardValue.isSuperJoker == false).maxOptionBy(_._1.cardValue.score).map(_._2)
      }
    }
  }

  override def choseColor(baseDecision: PlayerInfos): Color

  = {

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
