package crossworder

sealed abstract class Word(letters: String, x: Int, y: Int) {
  def maxX: Int
  def maxY: Int
  def length: Int = letters.length
  def transpose: Word
}

case class AcrossWord(letters: String, x: Int, y: Int) extends Word(letters: String, x: Int, y: Int) with Ordered[AcrossWord]{
  def maxX = x + length - 1
  def maxY = y
  def compare(that: AcrossWord) = that match {
    case AcrossWord(_,x2,y2) => P(x, y) compare P(x2, y2)
  }
  def transpose: DownWord = {
    DownWord(letters,y,x)
  }
}

case class DownWord(letters: String, x: Int, y: Int) extends Word(letters: String, x: Int, y: Int) with Ordered[DownWord] {
  def maxY = y + length - 1
  def maxX = x
  def compare(that: DownWord) = that match {
    case DownWord(_,x2,y2) => P(x, y) compare P(x2, y2)
  }
  def transpose: AcrossWord = {
    AcrossWord(letters,y,x)
  }
}