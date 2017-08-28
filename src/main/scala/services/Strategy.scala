package services

import entities.{Card, Colors}
import entities.Colors.Color

import scala.util.Random
import tools.CustomArray._

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

}


// Pure random strategy

object RandomStrategy extends Strategy {

  val r = new Random()

  override def choseCard(baseDecision: PlayerInfos): Option[Int] = {

    val playableCards = baseDecision.allCards.zipWithIndex.filter {

      card => card._1.isPlayableWith(baseDecision.currentCard, baseDecision.currentChosenColor)

    }

    if (playableCards.isEmpty) None

    else {

      val randomIndex: Int = r.nextInt(playableCards.length)
      Some(playableCards(randomIndex)._2)

    }
  }

  override def choseColor(baseDecision: PlayerInfos): Color = {

    val colors = Array(Colors.Bleu, Colors.Vert, Colors.Rouge, Colors.Jaune)

    colors(r.nextInt(4))

  }

}

// Better strategy : choose color with max number of cards

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

// Pick the strongest card on the hand & choose color with max number of cards

object StrongestStrategy extends Strategy {

  val r = new Random()

  override def choseCard(baseDecision: PlayerInfos): Option[Int] = {

    val playableCards: Array[(Card, Int)] = baseDecision.allCards.zipWithIndex.filter {

      card => card._1.isPlayableWith(baseDecision.currentCard, baseDecision.currentChosenColor)

    }

    if (playableCards.isEmpty) None
    else {

      if (isSuperJokerPlayable(baseDecision))
        playableCards.maxOptionBy(_._1.cardValue.score).map(_._2) else
        playableCards.filter(_._1.cardValue.isSuperJoker == false).maxOptionBy(_._1.cardValue.score).map(_._2)

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

// Pick the strongest card on the hand & choose color with max number of cards

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




