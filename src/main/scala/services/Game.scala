package services

import strategies.Strategy
import entities.{Colors, Deck}

/**
  * Created by Simon on 20/08/2017.
  */

class Game(firstPlayerStrategy: Strategy, secondPlayerStrategy: Strategy) {

  def initialize(firstPlayerPlaying: Boolean): GameState = {
    val deck = Deck()

    val firstPlayerHand = deck.allCards.slice(0, 7).toArray
    val secondPlayerHand = deck.allCards.slice(7, 14).toArray
    val firstCard = deck.allCards(15)
    val stack = deck.allCards.drop(15)

    if (firstCard.color.isDefined)
    GameState(firstPlayerPlaying, stack, firstPlayerHand, secondPlayerHand, firstCard, None)
    else GameState(firstPlayerPlaying, stack, firstPlayerHand, secondPlayerHand, firstCard, Some(firstPlayerStrategy.choseColorRandomly()))

  }

}
