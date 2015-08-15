package crossworderOld

object Crossworder extends App {
  
  val Down = false
  
  val words = Set(
               "rpg",
               "java",
               "scala",
               "delphi",
               "sql",
               "portfolio",
               "design",
               "development",
               "testanalyst",
               "executiononly",
               "figaro",
               "tracey",
               "jhc",
               "element",
               "neon",
               "carbon",
               "zinc",
               "technology",
               "database")
  
  def intersections(words: Set[String]): Set[((String, Int), (String, Int))] = {
    for {
      a <- words
      b <- words
      if (a != b)
      i <- (0 to a.length - 1)
      j <- (0 to b.length - 1)
      if (a(i) == b(j))
    } yield ((a, i+1), (b, j+1))
  }
  
  case class Location(x: Int, y: Int) {
    def move(right: Int, down: Int) = Location(x + right, y + down)
    def transpose = Location(y, x)
    def adjacentTo(that: Location) = {
      val verticalDiff = (this.y - that.y).abs
      val horizontalDiff = (this.x - that.x).abs
      verticalDiff + horizontalDiff == 1
    }
  }
  
  case class WordLocation(word: String, across: Boolean, start: Location) {
    val maxX = start.x + {if (across) (word.length - 1) else 0}
    val maxY = start.y + {if (!across) (word.length - 1) else 0}
    val length = word.length
    val locations = if (across) for {
      pos <- (0 to word.length - 1)
    } yield Location(start.x + pos, start.y)
    else for {
      pos <- (0 to word.length - 1)
    } yield Location(start.x, start.y + pos)
    def move(y: Int, x: Int) = {
      new WordLocation(word, across, start.move(y, x))
    }
    def transpose = {
      new WordLocation(word, !across, start.transpose)
    }
    
    def isSameWordAs(that: WordLocation): Boolean = {
      this.word == that.word
    }
    def isSameOrientationAs(that: WordLocation): Boolean = {
      this.across == that.across
    }
    
    def ==(that: WordLocation): Boolean = {
      this.word == that.word && this.across == that.across && this.start == that.start
    }
    
    def canCoexistWith(that: WordLocation): Boolean = {
      if (this == that) true
      else
      if (this.across == false)
        this.transpose.canCoexistWith(that.transpose)
      else {
        if (that.across == false) {
          // No overlap, down word is far to left or right of across word
          (that.start.x < this.start.x - 1) || (that.start.x > this.maxX + 1) ||
          // Down word is just one column away from across word, so must avoid touching the end.
          ((that.start.x == this.start.x - 1) || (that.start.x == this.maxX + 1)) && ((that.maxY < this.start.y) || (that.start.y > this.start.y)) ||
          // Down word is on same column as across word, so must be completely above or below...
          (that.start.x >= this.start.x && that.start.x <= this.maxX) && ((that.start.y > this.start.y + 1) || (that.maxY < this.start.y - 1)) ||
          // Down word properly crosses the across word and the common letter matches
          ((that.start.x >= this.start.x) && (that.start.x <= this.maxX)) && ((that.start.y <= this.start.y) && (that.maxY >= this.start.y)) && (this.word(that.start.x - this.start.x) == that.word(this.start.y - that.start.y))
        }
        else {
          if (that.start.y == this.start.y) {
            (this.maxX < that.start.x - 1) || (that.maxX < this.start.x - 1)
          }
          else {
            if ((that.start.y - this.start.y).abs == 1) {
              (this.maxX < that.start.x) || (that.maxX < this.start.x)
            }
            else true
          }
        }
      }
    }
  }
  
  class PrintGrid(lines: List[List[Char]]) {
    override def toString = {
      lines.map(line => line.mkString).mkString("\n")
    }
    
    def addWordLocation(wordLocation: WordLocation) = {
      val changePairs = wordLocation.locations zip wordLocation.word
      changePairs.foldLeft(this)((acc, nextPair) => acc.addCharacter(nextPair._1, nextPair._2))
    } 
    
    def addCharacter(location: Location, char: Char) = {
      new PrintGrid(lines.updated(location.y - 1, lines(location.y - 1).updated(location.x - 1, char)))
    }
  }
  
  def emptyGrid(height: Int, width: Int) = {
    val lines = (1 to height).toList.map(y => (1 to width).toList.map(_ => '.'))
    new PrintGrid(lines)
  }
  
  class CrosswordGrid(val words: Set[WordLocation]) {
    val startX = words.map(_.start.x).min
    val startY = words.map(_.start.y).min
    val width = 1 + words.map(_.maxX).max - startX
    val height = 1 + words.map(_.maxY).max - startY
    
    def move(y: Int, x: Int) = {
      new CrosswordGrid(words.map(_.move(y,x)))
    }
    
    def +(that: CrosswordGrid) = {
      new CrosswordGrid(this.words union that.words)
    }
    
    def adjustedWords = words.map(x => x.move(1 - startX, 1 - startY))
    
    override def toString = {
      adjustedWords.foldLeft(emptyGrid(height, width))((acc, nextWord) => acc.addWordLocation(nextWord)).toString
    }
    
    def isValid = {
      words.forall(a => words.forall(b => a canCoexistWith b))
    }
    
    def sharesAWordOrientationWith(that: CrosswordGrid) = {
      this.words.exists(thisWord => that.words.exists(thatWord => (thisWord isSameWordAs thatWord) && (thisWord isSameOrientationAs thatWord)))
    }
    
    def hasAllWordsIn(that: CrosswordGrid) = {
      def hasAllWordsIn(source: Set[WordLocation], checkWords: Set[WordLocation]): Boolean = {
        if (checkWords.isEmpty) true
        else
          if (source.exists(sWord => sWord isSameWordAs checkWords.head)) hasAllWordsIn(source, checkWords.tail)
          else
            false
      }
      hasAllWordsIn(this.words, that.words)
    }
    
    def joinWith(that: CrosswordGrid) = {
      val joins = for {
        a <- this.words
        b <- that.words
        if ((a isSameWordAs b) && (a isSameOrientationAs b))
      } yield (a, b)
      joins.headOption match {
        case None => throw new Error("Can't join")
        case Some((thisWord, thatWord)) => {
          val thatRelocated = that.move(thisWord.start.y - thatWord.start.y, thisWord.start.x - thatWord.start.x)
          this + thatRelocated
        }
      }
    }
  }
  
  // val quadWords = Set("cat", "dog", "dock", "gator")
  
  // dog
  // o a
  // cat
  // k o
  //   r
  
  def quads(words: Set[String]) = {
    for {
      firstAcross <- words
      firstDown <- words
      if (firstAcross != firstDown)
      fAFDI <- (0 to firstAcross.length - 1)
      fDFAI <- (0 to firstDown.length - 1)
      if (firstAcross(fAFDI) == firstDown(fDFAI))
      secondDown <- words
      if (secondDown != firstAcross && secondDown != firstDown)
      fASDI <- (fAFDI + 2 to firstAcross.length - 1)
      sDFAI <- (0 to secondDown.length - 1)
      if (firstAcross(fASDI) == secondDown(sDFAI))
      secondAcross <- words
      if (secondAcross != firstAcross && secondAcross != firstDown && secondAcross != secondDown)
      val minDownLength = Set(firstDown.length, secondDown.length).min
      val horizontalGap = fASDI - fAFDI
      fDSAI <- (fDFAI + 2 to minDownLength - 1)
      val sDSAI = fDSAI + sDFAI - fDFAI
      if (sDSAI <= secondDown.length - 1)
      sAFDI <- (0 to secondAcross.length - 1)
      val sASDI = sAFDI + horizontalGap
      if (sASDI <= secondAcross.length - 1)
      if (secondAcross(sAFDI) == firstDown(fDSAI) && secondAcross(sASDI) == secondDown(sDSAI))
      val firstAcrossPosition = Location(0, 0)
      val firstDownPosition = Location(fAFDI, -fDFAI)
      val secondAcrossPosition = Location(fAFDI - sAFDI, -fDFAI + fDSAI)
      val secondDownPosition = Location(fASDI, -sDFAI)
    } yield new CrosswordGrid(Set(
        WordLocation(firstAcross, true, firstAcrossPosition),
        WordLocation(firstDown, false, firstDownPosition),
        WordLocation(secondAcross, true, secondAcrossPosition),
        WordLocation(secondDown, false, secondDownPosition)))
  }
  
  val topCorner = Location(-1, -1)
  val nextAcross = Location(-1, 1)
  val dog = WordLocation("dog", true, topCorner)
  val cat = WordLocation("cat", true, nextAcross)
  val dock = WordLocation("dock", false, topCorner)
  val grid = new CrosswordGrid(Set(dog, cat, dock))
  
  lazy val quadGrids = quads(words)
  
  val a = quadGrids.head
  
  lazy val twoGrids = for {
    b <- quadGrids
    if !(a hasAllWordsIn b)
    if (a sharesAWordOrientationWith b)
  } yield a joinWith b
  
  //println(quadGrids.size)
  println(twoGrids.size)
  println(twoGrids.mkString("\n\n\n"))
}