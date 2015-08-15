package crossworder

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class WordTest extends FlatSpec with Matchers {
  
  val cat = AcrossWord("cat",1,1)
  "across word cat" should "have a length of 3" in {
    cat.length shouldBe 3
  }
  it should "have an x of 1" in {
    cat.x shouldBe 1
  }
  it should "have a y of 1" in {
    cat.y shouldBe 1
  }
  it should "have a maxX of 3" in {
    cat.maxX shouldBe 3
  }
  it should "have a maxY of 1" in {
    cat.maxY shouldBe 1
  }
  it should "have letters 'cat'" in {
    cat.letters shouldBe "cat"
  }
  
  "transpose" should "transpose cat to down word cat" in {
    cat.transpose shouldBe DownWord("cat",1,1)
  }
  it should "transpose down word trampoline" in {
    DownWord("trampoline",5,3).transpose shouldBe AcrossWord("trampoline",3,5) 
  }
  
}