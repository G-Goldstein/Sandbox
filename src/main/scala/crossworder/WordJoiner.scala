package crossworder

object WordJoiner {
  
  def join(words: List[String]): Set[Grid] = {
    require(words.length == 2)
    join(words.head, words.tail.head)
  }
  
  def join(a: String, b: String): Set[Grid] = {
    for {
      ai <- (0 to a.length-1)
      bi <- (0 to b.length-1)
      if a(ai) == b(bi)
      across = AcrossWord(a, 1, bi+1)
      down = DownWord(b, ai+1, 1)
      grid = Grid(Set(across, down))
      result <- Set(grid, grid.transpose)
    } yield result
  }.toSet
 
  def quad(words: List[String]): Set[Grid] = {
    require(words.size == 4)
    val firstPairs = for {
      word <- words.tail
    } yield List(words.head, word)
    for {
      firstPair <- firstPairs
      secondPair = words.filterNot(firstPair.contains(_))
      
    } yield Grid(Set())
  }.toSet
}