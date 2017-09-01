package test

import services.Game
import strategies.PlayingZeroFirst

object AlterInitializeTesting extends App {


  val playerOneStrategy = PlayingZeroFirst
  val playerTwoStrategy = PlayingZeroFirst

  val game = new Game(playerOneStrategy, playerTwoStrategy)

  var gameState = game.alterInitialize(false)
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
    println("-> Player One Wins")
  else
    println("-> Player Two Wins")


}
