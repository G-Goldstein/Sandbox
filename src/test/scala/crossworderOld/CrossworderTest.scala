package crossworderOld

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import Crossworder._

class CrossworderTest extends FlatSpec with Matchers {
  
  val Across = true
  // CATAPULT
  // A###A###
  // TIGER###
  // ##O#R###
  // #LAGOON#
  // ##T#T###
  
  val catapult = WordLocation("CATAPULT", Across, Location(1,1))
  val cat = WordLocation("CAT", Down, Location(1,1))
  val tiger = WordLocation("TIGER", Across, Location(1, 3))
  val parrot = WordLocation("PARROT", Down, Location(5,1))
  val goat = WordLocation("GOAT", Down, Location(3,3))
  val lagoon = WordLocation("LAGOON", Across, Location(2,5))
  
  // BAT
  // RAT
  
  val bat = WordLocation("BAT", Across, Location(1,1))
  val rat = WordLocation("RAT", Across, Location(1,2))
  
  // MONKEY
  // #D####
  // #O####
  // #G####
  
  val monkey = WordLocation("MONKEY", Across, Location(1,1))
  val dog = WordLocation("DOG", Down, Location(2,2))
   
  "string element comparison" should "be true for first letters in cat and catapult" in {
    catapult.word(cat.start.x - catapult.start.x) == cat.word(catapult.start.y - cat.start.y)
  }
  
  "Across" should "be true" in {
    Across shouldBe true
  }
  "Down" should "be false" in {
    Down shouldBe false
  } 
  
  "catapult" should "have start.x of 1" in {
    catapult.start.x shouldBe 1
  }
  it should "have maxX of 8" in {
    catapult.maxX shouldBe 8
  }
  it should "have start.y of 1" in {
    catapult.start.y shouldBe 1
  }
  it should "have maxY of 1" in {
    catapult.maxY shouldBe 1
  }
  it should "be across" in {
    catapult.across shouldBe true
  }
  "cat" should "have start.x of 1" in {
    cat.start.x shouldBe 1
  }
  it should "have maxX of 1" in {
    cat.maxX shouldBe 1
  }
  it should "have start.y of 1" in {
    cat.start.y shouldBe 1
  }
  it should "have maxY of 3" in {
    cat.maxY shouldBe 3
  }
  it should "be down" in {
    cat.across shouldBe false
  }
  
  "canCoexistWith" should "be true for overlapping words cat and catapult" in {
    catapult canCoexistWith cat shouldBe true
  }
  it should "be true for distant vertical words cat and parrot" in {
    cat canCoexistWith parrot shouldBe true
  }
  it should "be false for adjacent words rat and bat" in {
    rat canCoexistWith bat shouldBe false
  }
  it should "be false for touching words monkey and dog" in {
    monkey canCoexistWith dog shouldBe false
  }
  // C###
  // OPOT
  // G###
  
  val cog = WordLocation("COG", Down, Location(1,1))
  val pot = WordLocation("POT", Across, Location(2,2))
  it should "be false for touching words cog and pot" in {
    cog canCoexistWith pot shouldBe false
  }
  
  // #FEZ
  // H###
  // A###
  // T###
  
  val fez = WordLocation("FEZ", Across, Location(2,1))
  val hat = WordLocation("HAT", Down, Location(1,2))
  it should "be true for corner-touching words fez and hat" in {
    fez canCoexistWith hat shouldBe true
  }
  
  val allWords = Set(catapult, cat, tiger, parrot, goat, lagoon, bat, rat, monkey, dog, cog, pot, fez, hat)
  
  for {
    wordA <- allWords
    wordB <- allWords
  } it should "be commutative for " + wordA + " and " + wordB in {
    (wordA canCoexistWith wordB) shouldBe (wordB canCoexistWith wordA)
  }
  
  
  
}