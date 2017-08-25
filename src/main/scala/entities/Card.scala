package entities


object Colors extends Enumeration {
  type Color = Value
  val Rouge, Vert, Bleu, Jaune = Value
}

object CardTypes extends Enumeration {
  type CardType = Value
  val zero, un, deux, trois, quatre, cinq, six, sept, huit, neuf, changeSens, passeTour, plusDeux, joker, superJoker = Value
}

case class CardValue(cardType: CardTypes.CardType) {

  val score: Int = cardType match {
    case CardTypes.zero => 0
    case CardTypes.un => 1
    case CardTypes.deux => 2
    case CardTypes.trois => 3
    case CardTypes.quatre => 4
    case CardTypes.cinq => 5
    case CardTypes.six => 6
    case CardTypes.sept => 7
    case CardTypes.huit => 8
    case CardTypes.neuf => 9
    case (CardTypes.changeSens | CardTypes.passeTour | CardTypes.plusDeux) => 20
    case CardTypes.joker | CardTypes.superJoker => 50
  }

  override def toString: String = cardType match {
    case CardTypes.zero => "0"
    case CardTypes.un => "1"
    case CardTypes.deux => "2"
    case CardTypes.trois => "3"
    case CardTypes.quatre => "4"
    case CardTypes.cinq => "5"
    case CardTypes.six => "6"
    case CardTypes.sept => "7"
    case CardTypes.huit => "8"
    case CardTypes.neuf => "9"
    case CardTypes.changeSens => "<>"
    case CardTypes.passeTour => "/O/"
    case CardTypes.plusDeux => "+2"
    case CardTypes.joker => "JOKER"
    case CardTypes.superJoker => "JOKER+4"
  }

  val isJoker: Boolean = cardType == CardTypes.joker | cardType == CardTypes.superJoker
  val makesSkipTurn: Boolean = cardType == CardTypes.changeSens | cardType == CardTypes.passeTour | cardType == CardTypes.plusDeux | cardType == CardTypes.superJoker
  val isSuperJoker: Boolean = cardType == CardTypes.superJoker
}


case class Card(cardValue: CardValue, color: Option[Colors.Color]) extends Ordered[Card] {

  override def compare(that: Card): Int = cardValue.score compare that.cardValue.score

  def isPlayableWith(other: Card, declaredColor: Option[Colors.Color]): Boolean = {

    this.cardValue == other.cardValue |
      this.color.exists(p => other.color.contains(p)) |
      this.cardValue == other.cardValue |
      this.cardValue.isJoker |
      this.color.exists(p => declaredColor.contains(p))

  }

  override def toString: String = {
    val displayColor = color match {
      case Some(Colors.Bleu) => Console.BLUE
      case Some(Colors.Rouge) => Console.RED
      case Some(Colors.Vert) => Console.GREEN
      case Some(Colors.Jaune) => Console.YELLOW
      case None => Console.BLACK
    }

    val dispayValue = cardValue.cardType

    displayColor + cardValue.toString + Console.BLACK
  }


}
