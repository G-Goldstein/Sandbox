package crossworder

object WordJoiner {
  
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
 
}