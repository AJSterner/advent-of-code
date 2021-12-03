import scala.io.Source
import scala.collection.immutable.HashMap
object Main extends App {
  val lines = Source.fromFile("input").getLines.toList
  
  def parsePassports(lines: List[String]): List[Map[String, String]] = {
    def passport(lines: List[String]): Map[String, String] =
      lines.flatMap(_.split(' '))
           .map({case s"$k:$v" => k -> v})
           .toMap
    
    lines.span(_.nonEmpty) match {
      case (xs, _ :: ys) => passport(xs) :: parsePassports(ys)
      case (xs, Nil) => passport(xs) :: Nil
    }
  }

  val passports = parsePassports(lines)
  val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def complete = (passport: Map[String, String]) => requiredFields.subsetOf(passport.keySet)

  def validField(field: Tuple2[String, String]): Boolean = {
      val year = raw"(\d{4})".r
      val hgtCm = raw"(\d+)cm".r
      val hgtIn = raw"(\d+)in".r
      val hcl = raw"#([0-9a-f]{6})".r
      val ecl = raw"(amb|blu|brn|gry|grn|hzl|oth)".r
      val pid = raw"(\d{9})".r
      def bt(x: Int, lo: Int, hi: Int) = lo <= x && x <= hi
      field match {
        case ("byr", year(value))  => bt(value.toInt, 1920, 2002)
        case ("iyr", year(value))  => bt(value.toInt, 2010, 2020)
        case ("eyr", year(value))  => bt(value.toInt, 2020, 2030)
        case ("hgt", hgtCm(value)) => bt(value.toInt, 150, 193)
        case ("hgt", hgtIn(value)) => bt(value.toInt, 59, 76)
        case ("hcl", hcl(_)) => true
        case ("ecl", ecl(_)) => true
        case ("pid", pid(_)) => true
        case (k, _) => !requiredFields(k)
    } 
  }

  def validStrict(passport: Map[String, String]) = complete(passport) && passport.forall(validField)
  println(passports.count(complete))
  println(passports.count(validStrict))
}
