package test

import entities.{Card, CardTypes, CardValue, Colors}
import services.{Game, RandomStrategy, OtherRandomStrategy, StrongestStrategy}
import tools.CustomArray._

object RulesTesting extends App {


/*  val card1 = Card(CardValue(CardTypes.cinq), Some(Colors.Bleu))
  val card2 = Card(CardValue(CardTypes.trois), Some(Colors.Jaune))
  val card3 = Card(CardValue(CardTypes.trois), Some(Colors.Vert))
  val card4 = Card(CardValue(CardTypes.passeTour), Some(Colors.Bleu))
  val card5 = Card(CardValue(CardTypes.joker), None) */

  val playerOneStrategy = OtherRandomStrategy
  val playerTwoStrategy = StrongestStrategy

  val game = new Game(playerOneStrategy, playerTwoStrategy)

  var gameState = game.initialize(true)
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
