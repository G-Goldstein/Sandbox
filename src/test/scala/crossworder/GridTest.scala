package crossworder

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class GridTest extends FlatSpec with Matchers {
  
  // cooper
  // a    o
  // rat  u
  // r r  t
  // orange
  // t p
  // s
  
  val carrots = DownWord("carrots", 1, 1)
  val rat = AcrossWord("rat", 1, 3)
  val orange = AcrossWord("orange", 1, 5)
  val trap = DownWord("trap", 3, 3)
  val cooper = AcrossWord("cooper", 1, 1)
  val route = DownWord("route", 6, 1)
  
  
  
  val rattrap = new Grid(Set(orange, carrots, rat, trap, cooper, route))
  val rattrap2 = new Grid(Set(orange, carrots, rat, trap, cooper, route))
  val nearlyrattrap = new Grid(Set(orange, carrots, trap, cooper, route))
  
  "rattrap grid" should "contain AcrossWords cooper, rat and orange" in {
    rattrap.acrossWords shouldBe List("cooper", "rat", "orange")
  }
  it should "contain DownWords carrot, route and trap" in {
    rattrap.downWords shouldBe List("carrots", "route", "trap")
  }
  it should "have maxX of 6" in {
    rattrap.maxX shouldBe 6
  }
  it should "have maxY of 7" in {
    rattrap.maxY shouldBe 7
  }
  
  "equals" should "match on contents of the grid" in {
    rattrap shouldBe rattrap2
  }
  it should "not match different contents" in {
    nearlyrattrap should not be rattrap
  }
  
  "transpose" should "transpose rattrap" in {
    rattrap.transpose shouldBe new Grid(Set(orange.transpose, carrots.transpose, rat.transpose, trap.transpose, cooper.transpose, route.transpose))
  }
  
  
}