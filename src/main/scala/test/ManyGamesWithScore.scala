package test

import strategies._
import services._
// import test.ManyGames.{game, numberOfGames}

object ManyGamesWithScore extends App {

  // Define players strategies
  val playerOneStrategy = OtherRandomStrategy
  val playerTwoStrategy = StrongestStrategy

  // Number of games played
  val numberOfGames = 2000

  // Final Score to win one game
  val finalScore = 500

  // Initialisation
  val game = new Game(playerOneStrategy, playerTwoStrategy)

  val finalResult: Result = 0.until(numberOfGames).par.map {
    setNumber =>
      game.playOneSetWithScore(setNumber % 2 == 0, finalScore)
  }.reduce(_ + _)

  println(finalResult)


}
