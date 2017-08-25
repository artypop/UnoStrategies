package services

import entities.{Card, Colors}
import entities.Colors.Color

import scala.reflect.ClassTag
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

      val randomIndex: Int = r.nextInt(playableCards.length)
      Some(playableCards(randomIndex)._2)

    }
  }

  override def choseColor(baseDecision: PlayerInfos): Color = {


    baseDecision.allCards.flatMap(_.color)
        .groupBy(identity)
        .mapValues(_.length)
        .toArray
        .maxOptionBy(_._2)
        .map(_._1)
          .getOrElse{
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

      var cardMax:Array[Int] = new Array[Int](playableCards.length)

      for (i <- playableCards.indices) {
      cardMax(i) = playableCards(i)._1.cardValue.score
      }

      val cardIndex = cardMax.indexOf(cardMax.max)

      // val randomIndex: Int = r.nextInt(playableCards.length)
      // Some(playableCards(randomIndex)._2)

      Some(playableCards(cardIndex)._2)

    }
  }

  override def choseColor(baseDecision: PlayerInfos): Color = {

    baseDecision.allCards.flatMap(_.color)
      .groupBy(identity)
      .mapValues(_.length)
      .toArray
      .maxOptionBy(_._2)
      .map(_._1)
      .getOrElse{
        val colors = Array(Colors.Bleu, Colors.Vert, Colors.Rouge, Colors.Jaune)
        colors(r.nextInt(4))
      }
  }

}




