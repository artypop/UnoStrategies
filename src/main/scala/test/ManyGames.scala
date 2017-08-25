package test

import services.{Game, OtherRandomStrategy, RandomStrategy, StrongestStrategy}

object ManyGames extends App {

  val playerOneStrategy = OtherRandomStrategy
  val playerTwoStrategy = StrongestStrategy

  val game = new Game(playerOneStrategy, playerTwoStrategy)

  var gameWinnedByPlayer1 = 0
  var gameWinnedByPlayer2 = 0


  val numberOfGames = 100000

  (1 to numberOfGames).foreach {
    i => {

      val firstPlayer = i match {
        case n if n % 2 == 0 => false
        case _ => true
      }


      var gameState = game.initialize(firstPlayer)

      while (!gameState.gameEnded()) {

        gameState = if (gameState.firstPlayerPlaying) {
          gameState.playTurn(playerOneStrategy)
        } else {
          gameState.playTurn(playerTwoStrategy)
        }
      }

      if (gameState.winner())
        gameWinnedByPlayer1 = gameWinnedByPlayer1 + 1
      else
        gameWinnedByPlayer2 = gameWinnedByPlayer2 + 1

    }

  }

  println("Number of games winned by Player 1: " + gameWinnedByPlayer1)
  println("Number of games winned by Player 2: " + gameWinnedByPlayer2)

}
