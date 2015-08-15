package crossworder

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import WordJoiner._
import GridParser._

class WordJoinerTest extends FlatSpec with Matchers {
  
  val hammer = "hammer"
  val mallet = "mallet"
  
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
  
  "join" should "return all possible joins between hammer and mallet" in {
    join(hammer, mallet) shouldBe possibleJoins
  }
}