package services

import strategies.Strategy
import entities.{CardTypes, Colors, Deck}
import entities.Card
import entities.CardTypes._
import sun.awt.AWTIcon32_java_icon32_png

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


    val carteUn = deck.allCards.head

    val gsInit: Tuple5[Boolean, Array[Card], Array[Card], List[Card], Option[Colors.Color]] = carteUn.cardValue.cardType match {

      case `changeSens` | `passeTour` =>
        (!firstPlayerPlaying,
          deck.allCards.slice(1, 8).toArray,
          deck.allCards.slice(8, 15).toArray,
          deck.allCards.drop(15),
          None)

      case `plusDeux` =>
        (!firstPlayerPlaying,
          deck.allCards.slice(1, 10).toArray,
          deck.allCards.slice(10, 17).toArray,
          deck.allCards.drop(17), None)

      case `superJoker` =>
        (!firstPlayerPlaying,
          deck.allCards.slice(1, 12).toArray,
          deck.allCards.slice(12, 19).toArray,
          deck.allCards.drop(19),
          Some(firstPlayerStrategy.choseColorRandomly()))

      case _ =>
        (firstPlayerPlaying,
          deck.allCards.slice(1, 8).toArray,
          deck.allCards.slice(8, 15).toArray,
          deck.allCards.drop(15),
          if (carteUn.color.isDefined) None else Some(firstPlayerStrategy.choseColorRandomly()))

    }

    GameState(gsInit._1, gsInit._4, gsInit._3, gsInit._2, carteUn, gsInit._5)

  }

  def playOneSet(firstPlayerStarting: Boolean): Result = {

    var gameState = this.initialize(firstPlayerStarting)

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

    val gameStateInit = this.initialize(firstPlayerStarting)

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



