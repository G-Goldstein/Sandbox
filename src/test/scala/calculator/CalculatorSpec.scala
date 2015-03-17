package calculator

import org.scalatest._

class CalculatorSpec extends FlatSpec with Matchers {
  
  "Sum" should "correctly sum up two numbers" in {
    Calculator.sum(3,4) should be (7)
  }
   
}