import scala.io.Source

@main def hello: Unit = 
  val lines = Source.fromFile("input").getLines().toVector
  val foods = lines.map(parseFood)
  val aMap = buildAllergenMap(foods)
  println(pt1(aMap, foods))
  println(pt2(aMap))

type Food = Tuple2[Set[String], Set[String]] // (ingredients, allergens)
type AllergenMap = Map[String, Set[String]] // allergen -> possible ingredients

def parseFood(l: String) = l match
  case s"$xs (contains $ys)" => (xs.split(" ").toSet, ys.split(", ").toSet)

def buildAllergenMap(foods: Iterable[Food]): AllergenMap =
  foods.foldLeft(Map[String, Set[String]]())((aMap, food) => food match
    case (ingredients, allergens) => allergens.foldLeft(aMap)((aMap, allergen) =>
      aMap.updatedWith(allergen)(_ map { _ & ingredients } orElse Option(ingredients))))

def translateAllergens(aMap: AllergenMap): Map[String, String] = 
  def translate(aMap: AllergenMap, accum: Map[String, Set[String]]): Map[String, String] =
    if (aMap.isEmpty)
      accum.map { case (a, i) => (a, i.head) }
    else 
      val (translations, rest) = aMap.partition(_._2.size == 1)
      val tranlatedIngredients = translations.map(_._2.head)
      translate(rest.map { case (a, ings) => (a, ings -- tranlatedIngredients) }, translations ++ accum)

  translate(aMap, Map[String, Set[String]]())

def pt1(aMap: AllergenMap, foods: Iterable[Food]) =
  val possibleAllergens = aMap.foldLeft(aMap.head._2)((s, f) => s | f._2)
  foods.foldLeft(0)(_ + _._1.count(i => !possibleAllergens(i)))

def pt2(aMap: AllergenMap) = translateAllergens(aMap).toVector.sortBy(_._1).map(_._2).mkString(",")
