package test

import strategies._
import services._

object GamesTesting extends App {

  val playerOneStrategy = StrongestStrategy
  val playerTwoStrategy = PlayingZeroFirst


  val game = new Game(playerOneStrategy, playerTwoStrategy)

  var gameWinnedByPlayer1 = 0
  var gameWinnedByPlayer2 = 0

  val numberOfGames = 200

  (1 to numberOfGames).foreach {
    i => {

      val firstPlayer = i match {
        case n if n % 2 == 0 => false
        case _ => true
      }

      var gameState = game.initialize(firstPlayer)

      println(gameState)

      while (!gameState.gameEnded()) {
        gameState = if (gameState.firstPlayerPlaying) {
          gameState.playTurn(playerOneStrategy)
        } else {
          gameState.playTurn(playerTwoStrategy)
        }
      }
      println(gameState)

      println("*************************************")

      if (gameState.winner()) {
        println("-> Player One Wins\n")
        gameWinnedByPlayer1 = gameWinnedByPlayer1 + 1
      }
      else {
        println("-> Player Two Wins\n")
        gameWinnedByPlayer2 = gameWinnedByPlayer2 + 1
      }

    }

  }

  println("Number of games winned by Player 1: " + gameWinnedByPlayer1)
  println("Number of games winned by Player 2: " + gameWinnedByPlayer2)

}
