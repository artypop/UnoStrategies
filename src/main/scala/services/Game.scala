package services

import strategies.Strategy
import entities.{Colors, Deck}

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
    val firstCard = deck.allCards(15)
    val stack = deck.allCards.drop(15)

    if (firstCard.color.isDefined)
      GameState(firstPlayerPlaying, stack, firstPlayerHand, secondPlayerHand, firstCard, None)
    else GameState(firstPlayerPlaying, stack, firstPlayerHand, secondPlayerHand, firstCard, Some(firstPlayerStrategy.choseColorRandomly()))

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
      Result(1, 0, gameState.calculateScore(true), 0)
    }
    else {
      Result(0, 1, 0, gameState.calculateScore(false))
    }
  }

  def playOneSetWithScore(firstPlayerStarting: Boolean): Result = {


    var scorePlayerOne = 0
    var scorePlayerTwo = 0

    var numberOfGames = 0

    val finalScore = 500

    /* def playUntilFinalScore(intermediaryResult: Result): Result = {


      var gameState = this.initialize(firstPlayer)

      if (intermediaryResult.firstPlayerScore < finalScore & intermediaryResult.secondPlayerScore < finalScore) {
        firstPlayer = !firstPlayer
        playUntilFinalScore(playOneSet(firstPlayer))
      } else {
        if (intermediaryResult.firstPlayerScore < intermediaryResult.secondPlayerScore)
          Result(1, 0, 0, 0)
        else
          Result(0, 1, 0, 0)
      }
    }

    var resultat: Result = Result(0,0,0,0)


    resultat = playUntilFinalScore(resultat)

    resultat */

    while (scorePlayerOne <= finalScore & scorePlayerTwo <= finalScore) {

      val firstPlayer = numberOfGames % 2 == 0

      var gameState = this.initialize(firstPlayer)
      numberOfGames = numberOfGames + 1

      while (!gameState.gameEnded()) {
        gameState = if (gameState.firstPlayerPlaying) {
          gameState.playTurn(firstPlayerStrategy)
        } else {
          gameState.playTurn(secondPlayerStrategy)
        }
      }

      if (gameState.winner())
        scorePlayerTwo = scorePlayerTwo + gameState.calculateScore(true)
      else
        scorePlayerOne = scorePlayerOne + gameState.calculateScore(false)
    }

    if (scorePlayerOne > scorePlayerTwo)
      Result(1, 0, 0, 0)
    else
      Result(0, 1, 0, 0)
  }
}



