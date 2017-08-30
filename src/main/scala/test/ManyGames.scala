package test

import strategies._
import services._

object ManyGames extends App {

  // Define players strategies
  val playerOneStrategy = OtherRandomStrategy
  val playerTwoStrategy = StrongestStrategy

  // Number of games played
  val numberOfGames = 100000

  // Initialisation
  val game = new Game(playerOneStrategy, playerTwoStrategy)

  val finalResult: Result = 0.until(numberOfGames).par.map {
    setNumber =>
      game.playOneSet(setNumber % 2 == 0)
  }.reduce(_ + _)

  println(finalResult)

}
