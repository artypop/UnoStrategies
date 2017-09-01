package services

import strategies.Strategy
import entities.{CardTypes, Colors, Deck}
import entities.Card
import entities.CardTypes._
import entities.Colors.Color

import scala.annotation.tailrec

/**
  * Created by Simon on 20/08/2017.
  */

case class Result(firstPlayerVictories: Int,
                  secondPlayerVictories: Int,
                  firstPlayerScore: Int,
                  secondPlayerScore: Int) {

  def +(anotherResult: Result): Result = {
    Result(
      this.firstPlayerVictories + anotherResult.firstPlayerVictories,
      this.secondPlayerVictories + anotherResult.secondPlayerVictories,
      this.firstPlayerScore + anotherResult.firstPlayerScore,
      this.secondPlayerScore + anotherResult.secondPlayerScore
    )
  }

}

class Game(firstPlayerStrategy: Strategy, secondPlayerStrategy: Strategy) {

  def initialize(firstPlayerPlaying: Boolean): GameState = {

    val deck = Deck()

    val firstPlayerHand = deck.allCards.slice(0, 7).toArray

    val secondPlayerHand = deck.allCards.slice(7, 14).toArray

    val firstCard = deck.allCards(14)

    val stack = deck.allCards.drop(15)

    if (firstCard.color.isDefined)
      GameState(firstPlayerPlaying, stack, firstPlayerHand, secondPlayerHand, firstCard, None)
    else GameState(firstPlayerPlaying, stack, firstPlayerHand, secondPlayerHand, firstCard, Some(firstPlayerStrategy.choseColorRandomly()))


  }

  def alterInitialize(firstPlayerPlaying: Boolean): GameState = {

    val deck = Deck()


    val firstCard = deck.allCards.head

    val gsInit: (Boolean, Array[Card], Array[Card], List[Card]) = firstCard.cardValue.cardType match {

      case `changeSens` | `passeTour` =>
        (!firstPlayerPlaying,
          deck.allCards.slice(1, 8).toArray,
          deck.allCards.slice(8, 15).toArray,
          deck.allCards.drop(15))

      case `plusDeux` =>
        (!firstPlayerPlaying,
          deck.allCards.slice(1, 10).toArray,
          deck.allCards.slice(10, 17).toArray,
          deck.allCards.drop(17))

      case `superJoker` =>
        (!firstPlayerPlaying,
          deck.allCards.slice(1, 12).toArray,
          deck.allCards.slice(12, 19).toArray,
          deck.allCards.drop(19))

      case _ =>
        (firstPlayerPlaying,
          deck.allCards.slice(1, 8).toArray,
          deck.allCards.slice(8, 15).toArray,
          deck.allCards.drop(15))

    }

    val colorCard: Option[Color] = if (firstCard.color.isDefined) None else {
      if (firstPlayerPlaying) Some(firstPlayerStrategy.chooseColorFirstCard(gsInit._3)) else
        Some(secondPlayerStrategy.chooseColorFirstCard(gsInit._3))
    }

    GameState(gsInit._1, gsInit._4, gsInit._3, gsInit._2, firstCard, colorCard)


  }

  def playOneSet(firstPlayerStarting: Boolean): Result = {

    var gameState = this.alterInitialize(firstPlayerStarting)

    while (!gameState.gameEnded()) {
      gameState = if (gameState.firstPlayerPlaying) {
        gameState.playTurn(firstPlayerStrategy)
      } else {
        gameState.playTurn(secondPlayerStrategy)
      }
    }

    if (gameState.winner()) {
      Result(1, 0, 0, gameState.calculateScore(true))
    }
    else {
      Result(0, 1, gameState.calculateScore(false), 0)
    }
  }

  def playOneSetRec(firstPlayerStarting: Boolean): Result = {

    val gameStateInit = this.alterInitialize(firstPlayerStarting)

    @tailrec def playUntilEnd(gameState: GameState): Result = {

      if (!gameState.gameEnded()) {
        if (gameState.firstPlayerPlaying)
          playUntilEnd(gameState.playTurn(firstPlayerStrategy))
        else
          playUntilEnd(gameState.playTurn(secondPlayerStrategy))
      }
      else {
        if (gameState.winner())
          Result(1, 0, 0, gameState.calculateScore(true))
        else
          Result(0, 1, gameState.calculateScore(false), 0)
      }
    }

    playUntilEnd(gameStateInit)

  }


  def playOneSetWithScore(firstPlayerStarting: Boolean, finalScore: Int): Result = {


    @tailrec def playUntilFinalScore(intermediaryResult: Result, playerPlaying: Boolean): Result = {


      if (intermediaryResult.firstPlayerScore < finalScore & intermediaryResult.secondPlayerScore < finalScore) {
        playUntilFinalScore(intermediaryResult + playOneSet(playerPlaying), !playerPlaying)
      } else {

        if (intermediaryResult.firstPlayerScore < intermediaryResult.secondPlayerScore)
          Result(1, 0, 0, 0)
        else
          Result(0, 1, 0, 0)

      }
    }

    playUntilFinalScore(Result(0, 0, 0, 0), firstPlayerStarting)

  }
}



