package strategies

import entities.Colors.Color
import entities.Colors
import services.PlayerInfos

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

}


// Pure random strategy



// Better strategy : choose color with max number of cards



// Pick the strongest card on the hand & choose color with max number of cards



// Pick Zero as soon as it cans or else the strongest card on the hand & choose color with max number of cards





