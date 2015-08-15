package crossworder

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import GridParser._

class GridParserTest extends FlatSpec with Matchers {
  
  val zooString = """|     a   
                     |    ox
                     |     o   
                     |buffalo  
                     |a e  o   
                     |tarantula
                     |  r  l           
                     |  e      
                     |  tortoise""".stripMargin
                                    
  val zooGridString = GridString(zooString)
  
  "zooGridString" should "have 9 rows" in {
    zooGridString.rowCount shouldBe 9
  }
  it should "have 10 columns" in {
    zooGridString.columnCount shouldBe 10
  }
  it should "have a fourth row of 'buffalo', padded to the grid's column count" in {
    zooGridString.rows(3) shouldBe "buffalo".padTo(zooGridString.columnCount, blank)
  }
  it should "have a third column of 'ferret' with initial padding" in {
    zooGridString.columns(2) shouldBe "   ferret"
  }
  it should "contain the across word tortoise" in {
    zooGridString.acrossWords contains "tortoise" shouldBe true
  }
  it should "not contain the single-character across word e" in {
    zooGridString.acrossWords contains "e" shouldBe false
  }
  it should "contain the down word bat" in {
    zooGridString.downWords contains "bat" shouldBe true
  }
  it should "contain 4 across words" in {
    zooGridString.acrossWords.length shouldBe 4
  }
  it should "contain 3 down words" in {
    zooGridString.downWords.length shouldBe 3
  }
  it should "contain AcrossWord('ox', 5, 2)" in {
    zooGridString.acrossWordLocations contains AcrossWord("ox", 5, 2) shouldBe true
  }
  it should "contain DownWord('bat', 1, 4)" in {
    zooGridString.downWordLocations contains DownWord("bat", 1, 4) shouldBe true
  }
  it should "contain 7 WordLocations" in {
    zooGridString.wordLocations.size shouldBe 7
  }
  
  val zooGrid = toGrid(zooString)
  
  "toGrid should result in zooGrid, which" should "have a maxX of 10" in {
    zooGrid.maxX shouldBe 10
  }
  it should "have a maxY of 9" in {
    zooGrid.maxY shouldBe 9
  }
  it should "contain AcrossWord('tarantula', 1, 6)" in {
    val tarantula = AcrossWord("tarantula", 1, 6)
    zooGrid.words.contains(tarantula) shouldBe true
  }
  it should "contain 4 across words" in {
    zooGrid.acrossWords.size shouldBe 4
  }
  it should "contain 3 down words" in {
    zooGrid.downWords.size shouldBe 3
  }
  
}