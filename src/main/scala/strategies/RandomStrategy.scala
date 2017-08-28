package strategies

import entities.Colors
import entities.Colors.Color
import services.PlayerInfos

import scala.util.Random


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