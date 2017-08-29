package test

import strategies._
import services._
// import test.ManyGames.{game, numberOfGames}

object ManyGamesWithScore extends App {

  // Define players strategies
  val playerOneStrategy = StrongestStrategy
  val playerTwoStrategy = StrongestStrategy

  // Number of games played
  val numberOfGames = 200000

  // Final Score to win one game
  val finalScore = 5

  // Initialisation
  val game = new Game(playerOneStrategy, playerTwoStrategy)

  var gameWinnedByPlayer1 = 0
  var gameWinnedByPlayer2 = 0

  /*
  // Games playing
  for (_ <- 0 to numberOfGames) {

    var n = 0

    var scorePlayerOne = 0
    var scorePlayerTwo = 0

    while (scorePlayerOne <= finalScore & scorePlayerTwo <= finalScore) {


      val firstPlayer = n % 2 == 0
      println(firstPlayer + "n : "+n)
      n=n+1


      var gameState = game.initialize(firstPlayer)

      println(gameState)

      while (!gameState.gameEnded()) {

        gameState = if (gameState.firstPlayerPlaying) {
          gameState.playTurn(playerOneStrategy)
        } else {
          gameState.playTurn(playerTwoStrategy)
        }

        println(gameState)
      }

      if (gameState.winner())
        scorePlayerTwo = scorePlayerTwo + gameState.calculateScore(true)
      else
        scorePlayerOne = scorePlayerOne + gameState.calculateScore(false)

      println(gameState.winner()+"Score P1 : "+scorePlayerOne+"Score P2 = "+ scorePlayerTwo)

    }

    if (scorePlayerOne > scorePlayerTwo)
      gameWinnedByPlayer2 = gameWinnedByPlayer2 + 1 else
      gameWinnedByPlayer1 = gameWinnedByPlayer1 + 1
  }

  println("* Number of games winned by Player 1: " + gameWinnedByPlayer1)
  println("* Number of games winned by Player 2: " + gameWinnedByPlayer2) */

  val finalResult: Result = 0.until(numberOfGames).par.map{
    setNumber  =>
      game.playOneSetWithScore(setNumber % 2 == 0)
  }.reduce(_ + _)

  println(finalResult)


}
