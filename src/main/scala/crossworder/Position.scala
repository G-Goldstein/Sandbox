package crossworder

case class P(x: Int, y: Int) extends Ordered[P] {
  def compare(that: P) = {
    val yDiff = this.y - that.y
    val xDiff = this.x - that.x
    if (yDiff != 0) yDiff else xDiff
  }
}