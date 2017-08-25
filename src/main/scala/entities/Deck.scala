package entities

import Colors._

import scala.util.Random


case class Deck() {


  val allCards: List[Card] = {

    val r = new Random()

    val colors = List(Colors.Bleu, Colors.Vert, Colors.Rouge, Colors.Jaune)

    val coloredCardsTypes = List(
      CardTypes.un,
      CardTypes.deux,
      CardTypes.trois,
      CardTypes.quatre,
      CardTypes.cinq,
      CardTypes.six,
      CardTypes.sept,
      CardTypes.huit,
      CardTypes.neuf,
      CardTypes.passeTour,
      CardTypes.plusDeux,
      CardTypes.changeSens
    )

    val coloredCards = coloredCardsTypes.flatMap {
      digit =>
        colors.map {
          color => Card(CardValue(digit), Some(color))
        }
    }

    val zeros = colors.map {
      color => Card(CardValue(CardTypes.zero), Some(color))
    }

    val jokers = (0 to 3).flatMap(n => List(Card(CardValue(CardTypes.joker), None), Card(CardValue(CardTypes.superJoker), None))).toList

    r.shuffle(jokers ++ coloredCards ++ coloredCards ++ zeros)

    // r.shuffle(coloredCards ++ coloredCards ++ zeros)

  }

}
