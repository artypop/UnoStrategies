package test

import strategies._
import services._

object ManyGames extends App {



  // Define players strategies
  val playerOneStrategy = PlayingZeroFirst
  val playerTwoStrategy = StrongestStrategy

  // Number of games played
  val numberOfGames = 50000

  // Initialisation
  val game = new Game(playerOneStrategy, playerTwoStrategy)


  val t3 = System.currentTimeMillis()

  var gameWinnedByPlayer1 = 0
  var gameWinnedByPlayer2 = 0


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

  val t4 = System.currentTimeMillis()

  println("Number of games winned by Player 1: " + gameWinnedByPlayer1)
  println("Number of games winned by Player 2: " + gameWinnedByPlayer2)

  println(t4 - t3)



  val t1= System.currentTimeMillis()

  val finalResult: Result = 0.until(numberOfGames).par.map{
    setNumber  =>
      game.playOneSet(setNumber % 2 == 0)
  }.reduce(_ + _)

  println(finalResult)

  val t2 = System.currentTimeMillis()
  println(t2-t1)

}
