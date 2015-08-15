package calculator

object Functions extends App {
  
  def allBinaryFunctionInputs[T](set: Set[T]) = {
    for {
      x <- set
      y <- set
    } yield (x, y)
  }
  
  val map = Map(1 -> 2, 3 -> 4)
  
  def allFunctions[T](set: Set[T]): Set[Map[(T, T), T]] = {
    def allFunctionsAcc(remainingInputs: Set[(T, T)]): Set[Map[(T, T), T]] = {
      if (remainingInputs.isEmpty) Set()
      else if (remainingInputs.tail.isEmpty)
        for {
          x <- set
        } yield Map(remainingInputs.head -> x)
      else 
      for {
        x <- set
        partialFunction <- allFunctionsAcc(remainingInputs.tail)
      } yield partialFunction + (remainingInputs.head -> x)
    }
    allFunctionsAcc(allBinaryFunctionInputs(set))
  }

  allFunctions(Set(1, 2, 3)).foreach(println)

}