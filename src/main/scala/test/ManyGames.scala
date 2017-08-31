package test

import strategies._
import services._

object ManyGames extends App {

  // Define players strategies
  val playerOneStrategy = StrongestStrategy
  val playerTwoStrategy = PlayingZeroFirst

  // Number of games played
  val numberOfGames = 50000

  // Initialisation
  val game = new Game(playerOneStrategy, playerTwoStrategy)

  val t1 = System.currentTimeMillis()

  val finalResultRec: Result = 0.until(numberOfGames).par.map {
    setNumber =>
      game.playOneSetRec(setNumber % 2 == 0)
  }.reduce(_ + _)

  val t2 = System.currentTimeMillis()

  println(finalResultRec)
  println(t2 - t1)


  val finalResult: Result = 0.until(numberOfGames).par.map {
    setNumber =>
      game.playOneSet(setNumber % 2 == 0)
  }.reduce(_ + _)

  val t3 = System.currentTimeMillis()

  println(finalResult)
  println(t3 - t2)


}
