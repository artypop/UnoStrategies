package services

import strategies.Strategy
import entities.{Card, CardTypes, Colors}
import entities.Colors.Color
import tools.CustomArray._


case class PlayerInfos(allCards: Array[Card], currentCard: Card, currentChosenColor: Option[Color], otherPLayerCardNumers: Int) {

}

case class GameState(firstPlayerPlaying: Boolean, stack: List[Card], currentPlayerHand: Array[Card],
                     otherPlayerHand: Array[Card], currentCard: Card, currentChosenColor: Option[Colors.Color]) {

  val playerInfos = PlayerInfos(currentPlayerHand, currentCard, currentChosenColor, otherPlayerHand.length)


  // Display
  override def toString: String =
    "*************************************" + "\n" + (
      if (firstPlayerPlaying)
        "1P*" + "   " + currentPlayerHand.mkString(" ") + "\n" +
          "2P " + "   " + otherPlayerHand.mkString(" ") + "\n"
      else
        "1P " + "   " + otherPlayerHand.mkString(" ") + "\n" +
          "2P*" + "   " + currentPlayerHand.mkString(" ") + "\n"
      ) + currentCard + Console.BLACK + " | CL:" + stack.size + " | Color:" + currentChosenColor.fold("None")(_.toString) +
      " | In deck:" + stack.head


  // Playing turn
  def playTurn(playerStrategy: Strategy): GameState = {

    val playedCardIndex: Option[Int] = playerStrategy.choseCard(playerInfos)

    // The player can play a card from his hand
    val (newCurrentPlayerHand, newOtherPlayerHand, newCurrentCard, newStack, newChosenColor, currentCardChange) = playedCardIndex.map { pci =>

      val newCurrentPlayerHand = currentPlayerHand.dropAtIndex(pci)
      val newCurrentCard = currentPlayerHand(pci)

      val (newOtherPlayerHand, newStack, newChosenColor) = newCurrentCard.cardValue.cardType match { // changed currentCard to newCurrentCard

        case CardTypes.plusDeux => (otherPlayerHand ++ stack.take(2), stack.drop(2) :+ currentCard, None)
        case CardTypes.superJoker => (otherPlayerHand ++ stack.take(4), stack.drop(4) :+ currentCard, Some(playerStrategy.choseColor(playerInfos)))
        case CardTypes.joker => (otherPlayerHand, stack :+ currentCard, Some(playerStrategy.choseColor(playerInfos)))
        case _ => (otherPlayerHand, stack :+ currentCard, None)

      }

      (
        newCurrentPlayerHand,
        newOtherPlayerHand,
        newCurrentCard,
        newStack,
        newChosenColor,
        true
      )

    }.getOrElse {

      // The player picks a card
      val pickedCard = stack.head
      val intermediaryStack = stack.tail
      val isPickedCardPlayable = pickedCard.isPlayableWith(currentCard, currentChosenColor)

      // val (newCurrentPlayerHand, newOtherPlayerHand, newStack, newChosenColor, currentCardChange) = {

      val (_, newOtherPlayerHand, newStack, newChosenColor, _) = {

        // The player can play the picked card
        if (isPickedCardPlayable) {

          val (newOtherPlayerHand, newStack, newChosenColor) = pickedCard.cardValue.cardType match {

            case CardTypes.plusDeux => (otherPlayerHand ++ intermediaryStack.take(2), intermediaryStack.drop(2) :+ currentCard, None)
            case CardTypes.superJoker => (otherPlayerHand ++ intermediaryStack.take(4), intermediaryStack.drop(4) :+ currentCard, Some(playerStrategy.choseColor(playerInfos)))
            case CardTypes.joker => (otherPlayerHand, intermediaryStack :+ currentCard, Some(playerStrategy.choseColor(playerInfos)))
            case _ => (otherPlayerHand, intermediaryStack :+ currentCard, None) // Changed currentChosenColor to None ?


          }
          (currentPlayerHand, newOtherPlayerHand, newStack, newChosenColor, true)


        }

        // The player can't play the picked card
        else (currentPlayerHand, otherPlayerHand, intermediaryStack, currentChosenColor, false)
      }

      (
        if (!isPickedCardPlayable) currentPlayerHand :+ pickedCard else currentPlayerHand,
        if (isPickedCardPlayable) newOtherPlayerHand else otherPlayerHand,
        if (isPickedCardPlayable) pickedCard else currentCard,
        // Changed intermediaryStack to newStack (else drops cards)
        newStack,
        if (isPickedCardPlayable) newChosenColor else currentChosenColor,
        isPickedCardPlayable)
    }

    if (currentCardChange & newCurrentCard.cardValue.makesSkipTurn)
      GameState(firstPlayerPlaying, newStack, newCurrentPlayerHand, newOtherPlayerHand, newCurrentCard, newChosenColor)
    else
      GameState(!firstPlayerPlaying, newStack, newOtherPlayerHand, newCurrentPlayerHand, newCurrentCard, newChosenColor)

  }

  def gameEnded(): Boolean = {
    otherPlayerHand.isEmpty | currentPlayerHand.isEmpty
  }


  def winner(): Boolean = {
    (firstPlayerPlaying | !currentPlayerHand.isEmpty) & !(firstPlayerPlaying & otherPlayerHand.isEmpty)
  }

  def calculateScore(player: Boolean): Int = { // True for P1, False for P2

    if (currentPlayerHand.isEmpty)
      scoreHand(otherPlayerHand) else
      scoreHand(currentPlayerHand)

  }

  def scoreHand(hand: Array[Card]): Int =
    (0 /: hand.map(_.cardValue.score)) (_ + _)
}
