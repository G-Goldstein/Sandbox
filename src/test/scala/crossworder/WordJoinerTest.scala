package crossworder

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import WordJoiner._
import GridParser._

class WordJoinerTest extends FlatSpec with Matchers {
  
  val hammet1 = """|hammer
                   |  a
                   |  l
                   |  l
                   |  e
                   |  t""".stripMargin
                   
  val hammet2 = """|hammer
                   |   a
                   |   l
                   |   l
                   |   e
                   |   t""".stripMargin
                   
  val hammet3 = """| m
                   |hammer
                   | l
                   | l
                   | e
                   | t """.stripMargin
                   
  val hammet4 = """|    m
                   |    a
                   |    l
                   |    l
                   |hammer
                   |    t""".stripMargin
                   
  val possibleJoins = Set(toGrid(hammet1), toGrid(hammet1).transpose, 
                          toGrid(hammet2), toGrid(hammet2).transpose,
                          toGrid(hammet3), toGrid(hammet3).transpose,
                          toGrid(hammet4), toGrid(hammet4).transpose)
  
  "join" should "return all possible joins between 'hammer' and 'mallet'" in {
    join("hammer", "mallet") shouldBe possibleJoins
  }
  it should "return all possible joins between 'mallet' and 'hammer'" in {
    join("mallet", "hammer") shouldBe possibleJoins
  }
  val hammet = List("hammer", "mallet")
  val maller = List("mallet", "hammer")
  it should "return all possible joins between the list of 'mallet' and 'hammer'" in {
    join(maller) shouldBe possibleJoins
  }
  it should "return all possible joins between the list of 'hammer' and 'mallet'" in {
    join(hammet) shouldBe possibleJoins
  }
  it should "return the empty set for possible joins between 'cat' and 'dog'" in {
    join("cat", "dog") shouldBe Set[Grid]()
  }
  
  // goat, gold, torn, darn
  
  val quad1 = """|goat
                 |o  o
                 |l  r
                 |darn""".stripMargin
                 
  val quad2 = """| g 
                 |goat
                 | l o
                 | darn
                 |   n""".stripMargin
                 
  val possibleQuads = Set(toGrid(quad1), toGrid(quad1).transpose,
                          toGrid(quad2), toGrid(quad2).transpose)
  
  "quad" should "not accept a set of size 0" in {
    intercept[IllegalArgumentException] {
      quad(Set[String]()) 
    }
  }
  it should "not accept a set of size 3" in {
    intercept[IllegalArgumentException] {
      quad(Set("a", "b", "c")) 
    }
  }
  it should "not accept a set of size 5" in {
    intercept[IllegalArgumentException] {
      quad(Set("a", "b", "c", "d", "e")) 
    }
  }
  it should "return all possible quads between 'goat', 'gold', 'torn' and 'darn'" in {
    quad(Set("goat", "gold", "torn", "darn")) shouldBe possibleQuads
  }
  
}