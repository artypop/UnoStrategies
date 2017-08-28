package test

import services.{Game, RandomStrategy, OtherRandomStrategy}

object OneGameWithScore extends App {

  val playerOneStrategy = RandomStrategy
  val playerTwoStrategy = OtherRandomStrategy

  val finalScore = 500

  val game = new Game(playerOneStrategy, playerTwoStrategy)

  var gameWinnedByPlayer1 = 0
  var gameWinnedByPlayer2 = 0

  var scorePlayerOne = 0
  var scorePlayerTwo = 0

  var i = 2

  while (scorePlayerOne <= finalScore & scorePlayerTwo <= finalScore) {

    val firstPlayer = i match {
      case n if n % 2 == 0 => false
      case _ => true
    }

    i = i + 1

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
      scorePlayerTwo = scorePlayerTwo + gameState.calculateScore(true)
    }
    else {
      println("-> Player Two Wins\n")
      gameWinnedByPlayer2 = gameWinnedByPlayer2 + 1
      scorePlayerOne = scorePlayerOne + gameState.calculateScore(false)
    }
    println("Score Player 1: " + scorePlayerOne)
    println("Score Player 2: " + scorePlayerTwo + "\n\n")
  }

  if (scorePlayerOne > 500) println("** Player Two Wins\n") else println("** Player One Wins\n")

  println("Number of games winned by Player 1: " + gameWinnedByPlayer1)
  println("Number of games winned by Player 2: " + gameWinnedByPlayer2)

}