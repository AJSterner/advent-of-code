import scala.collection.immutable.Queue
import scala.io.Source
import scala.annotation.tailrec

@main def crabCombat: Unit = 
  val input = Source.fromFile("input").getLines.to(Iterable)
  val game = parseDecks(input)
  println(pt1(game))
  println(pt2(game))

type Deck = Vector[Int]
type Game = Tuple2[Deck, Deck]

def parseDecks(input: Iterable[String]): Game =
  def toDeck(d: Iterable[String]): Deck = d.tail.map(_.toInt).to(Vector)
  input.span(_.nonEmpty) match
    case (d1, d2) => (toDeck(d1), toDeck(d2.tail))

@tailrec
def combat(game: Game): Deck = game match
  case (c1 +: d1, c2 +: d2) if (c1 > c2) => combat(d1 :+ c1 :+ c2, d2)
  case (c1 +: d1, c2 +: d2) if (c1 < c2) => combat(d1, d2 :+ c2 :+ c1)
  case (d1, d2) => if (d1.nonEmpty) d1 else d2

def score(d: Deck) = d.reverse.zipWithIndex.map({ case (c, i) => c * (i + 1) }).sum

def pt1(game: Game) = score(combat(game))

def recursiveCombat(game: Game): Deck = 
  case class Result(player: Int, deck: Deck)
  def rc(game: Game, prevGames: Set[Game]): Result = game match 
    case (d1, _) if (prevGames contains game) => Result(1, d1)
    case (c1 +: d1, c2 +: d2) if (c1 <= d1.size && c2 <= d2.size) =>
      rc((d1.take(c1), d2.take(c2)), Set[Game]()) match
        case Result(1, _) => rc((d1 :+ c1 :+ c2, d2), prevGames + game)
        case Result(2, _) => rc((d1, d2 :+ c2 :+ c1), prevGames + game)
    case (c1 +: d1, c2 +: d2) if (c1 > c2) => rc((d1 :+ c1 :+ c2, d2), prevGames + game)
    case (c1 +: d1, c2 +: d2) if (c1 < c2) => rc((d1, d2 :+ c2 :+ c1), prevGames + game)
    case (d1, d2) if (d2.isEmpty) => Result(1, d1)
    case (d1, d2) if (d1.isEmpty) => Result(2, d2)
    case (_, _) => throw new Exception("Malformed game")
  
  rc(game, Set[Game]()).deck

def pt2(game: Game) = score(recursiveCombat(game))
