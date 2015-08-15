package crossworder

case class Grid (words: Set[Word]) {
  lazy val acrossWords = words.collect{case word: AcrossWord => word}.toList.sorted.map(_.letters)
  lazy val downWords = words.collect{case word: DownWord => word}.toList.sorted.map(_.letters)
  lazy val maxX = words.map(_.maxX).max
  lazy val maxY = words.map(_.maxY).max
  def transpose = new Grid(words.map(_.transpose))
}