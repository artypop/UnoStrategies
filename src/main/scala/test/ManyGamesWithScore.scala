package test

import strategies._
import services._

object ManyGamesWithScore extends App {

  // Define players strategies
  val playerOneStrategy = PlayingZeroFirst
  val playerTwoStrategy = StrongestStrategy

  // Number of games played
  val numberOfGames = 100000

  // Final Score to win one game
  val finalScore = 500

  // Initialisation
  val game = new Game(playerOneStrategy, playerTwoStrategy)

  var gameWinnedByPlayer1 = 0
  var gameWinnedByPlayer2 = 0

  // Games playing
  for (_ <- 0 to numberOfGames) {

    var n = 0

    var scorePlayerOne = 0
    var scorePlayerTwo = 0

    while (scorePlayerOne <= finalScore & scorePlayerTwo <= finalScore) {

      val firstPlayer = n match {
        case j if j % 2 == 0 => false
        case _ => true
      }
      n = n + 1


      var gameState = game.initialize(firstPlayer)

      while (!gameState.gameEnded()) {

        gameState = if (gameState.firstPlayerPlaying) {
          gameState.playTurn(playerOneStrategy)
        } else {
          gameState.playTurn(playerTwoStrategy)
        }
      }

      if (gameState.winner()) {
        scorePlayerTwo = scorePlayerTwo + gameState.calculateScore(true)

      }
      else {
        scorePlayerOne = scorePlayerOne + gameState.calculateScore(false)

      }

    }

    if (scorePlayerOne > scorePlayerTwo)
      gameWinnedByPlayer2 = gameWinnedByPlayer2 + 1 else
      gameWinnedByPlayer1 = gameWinnedByPlayer1 + 1
  }

  println("* Number of games winned by Player 1: " + gameWinnedByPlayer1)
  println("* Number of games winned by Player 2: " + gameWinnedByPlayer2)

}
