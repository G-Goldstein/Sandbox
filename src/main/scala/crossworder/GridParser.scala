package crossworder

object GridParser {
  
  val lineBreak = System.lineSeparator
  val blank = ' '
  def isBlank(char: Char): Boolean = char == blank
  
  def trimTrailingBlanks(string: String) = string.reverse.dropWhile(isBlank).reverse
  
  def toGrid(string: String) = Grid(GridString(string).wordLocations)
  
  case class GridString(string: String) {
    lazy val unpaddedRows = string.split(lineBreak).toList.map(trimTrailingBlanks)
    lazy val rowCount = unpaddedRows.length
    lazy val columnCount = unpaddedRows.map(_.length).max
    lazy val rows = unpaddedRows.map(_.padTo(columnCount, ' '))
    lazy val columns = rows.transpose.map(_.mkString)
    
    lazy val acrossWords = words(rows)
    lazy val downWords = words(columns)
    
    def words(lines: List[String]) = {
      for {
        line <- lines
        word <- line.split(blank).filter(_.length > 1)
      } yield word
    }
    
    def acrossWordLocations = {
      for {
        word <- acrossWords
        rowNumber <- (1 to rowCount)
        columnNumber = (rows(rowNumber-1) indexOfSlice word) + 1
        if columnNumber > 0
      } yield AcrossWord(word, columnNumber, rowNumber)
    }
    
    def downWordLocations = {
      for {
        word <- downWords
        columnNumber <- (1 to columnCount)
        rowNumber = (columns(columnNumber-1) indexOfSlice word) + 1
        if rowNumber > 0
      } yield DownWord(word, columnNumber, rowNumber)
    }
    
    def wordLocations: Set[Word] = acrossWordLocations.toSet union downWordLocations.toSet
  }
}