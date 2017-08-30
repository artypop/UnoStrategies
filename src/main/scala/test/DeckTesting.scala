package test

import entities.Deck

object DeckTesting extends App {

  val deck = Deck()

  deck.allCards.foreach(println)

  val rouge = Console.RED + "red" + Console.BLACK
  println(Console.BLUE_B + "blue" + Console.BLACK)
  println(rouge)
  println("joker")

}
