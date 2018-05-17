import scala.io.Source

object WordCount{

  trait Monoid[A]{
    def op(x: A, y: A): A
    def zero: A
  }

  object WordCounter{
    //     I need this method to be static (belong to a class not an object of a class)
    def initWordCounter(text: String): WordCounter = {
      lazy val startsWithSpace = text.startsWith(" ")
      lazy val endsWithSpace = text.endsWith(" ")

      val words = text.split("\\s+").toList

      println(words.head, words.length - 2, words.last)

      if (startsWithSpace && endsWithSpace) {
        FullWords(words.head, words.length - 2, words.last)
      }
      else if (startsWithSpace){
        RightUnfinished(words.head, words.length - 2, words.last)
      }
      else if (endsWithSpace){
        LeftUnfinished(words.head, words.length - 2, words.last)
      }
      else BothUnfinished(words.head, words.length - 2, words.last)



//      new FullWords(words.head, words.length - 2, words.last)
    }
  }

  sealed trait WordCounter
  case class FullWords(leftPart: String, countWords: Int, rightPart: String) extends WordCounter
  case class LeftUnfinished(leftPart: String, countWords: Int, rightPart: String) extends WordCounter
  case class RightUnfinished(leftPart: String, countWords: Int, rightPart: String) extends WordCounter
  case class BothUnfinished(leftPart: String, countWords: Int, rightPart: String) extends WordCounter
  case class Part(chars: String) extends WordCounter

  val wordCountMonoid =  new Monoid[WordCounter] {
    override def op(x: WordCounter, y: WordCounter): WordCounter = (x, y) match {
      case ( FullWords(xLeftPart, xWordCount, xRightPart),
      FullWords(yLeftPart, yWordCount, yRightPart)) =>
        FullWords(xLeftPart, xWordCount + yWordCount + 2, yRightPart)

      case ( RightUnfinished(xLeftPart, xWordCount, xRightPart),
      LeftUnfinished(yLeftPart, yWordCount, yRightPart)) =>
        FullWords(xLeftPart, xWordCount + yWordCount + 1, yRightPart)

      case ( BothUnfinished(xLeftPart, xWordCount, xRightPart),
      BothUnfinished(yLeftPart, yWordCount, yRightPart)) =>
        FullWords(xLeftPart, xWordCount + yWordCount + 1, yRightPart)

//      case (Part(leftPart), Part(rightPart)) =>
//        Part(leftPart + rightPart)
//
//      case (Part(leftPart), FullWord("", wordCount, rightPart)) =>
//        FullWord(leftPart, wordCount, rightPart)
//
//      case (FullWord(leftPart, wordCount, ""), Part(rightPart)) =>
//        FullWord(leftPart, wordCount, rightPart)
//
//      case (Part(NonEmptySpaceString(_)), FullWord(_, wordCount, rightPart)) =>
//        FullWord("", wordCount + 1, rightPart)
//
//      case (FullWord(leftPart, wordCount, _), Part(NonEmptySpaceString(_))) =>
//        FullWord(leftPart, wordCount + 1, "")
//
//      case (Part(leftLeftPart), FullWord(leftRightPart, wordCount, rightPart)) =>
//        FullWord(leftLeftPart + leftRightPart, wordCount, rightPart)
//
//      case (FullWord(leftPart, wordCount, rightLeftPart), Part(rightRightPart)) =>
//        FullWord(leftPart, wordCount, rightLeftPart + rightRightPart)
//
//      case (FullWord(leftPart, leftWordCount, centreLeftPart), FullWord(centreRightPart, rightWordCount, rightPart)) =>
//        FullWord(
//          leftPart,
//          leftWordCount + rightWordCount + (if ((centreLeftPart + centreRightPart) == "") 0 else 1),
//          rightPart
//        )
    }

    //      WordCounter(x.leftPart, x.countWords + y.countWords + 1, y.rightPart)
    override def zero: WordCounter = FullWords("", 0, "")
  }


  def splitLineAndCountWords(text: String)(implicit maxSignsThreshold:Int) : Int = {
    def iter(text: String): WordCounter = {
      if (text.length < maxSignsThreshold)
        WordCounter.initWordCounter(text)
      else {
        val (left, right) = text.splitAt(text.length / 2)
        wordCountMonoid.op(WordCounter.initWordCounter(left),WordCounter.initWordCounter(right))
      }
    }

//    val result = iter(text)
    // add 2 as we have words in left and right parts
    iter(text) match{
      case FullWords(_, wordsCount, _) =>
        wordsCount + 2
    }
//    result.countWords + 2
  }

  def getLinesFromFile(filename: String) = {
    val lines = Source.fromFile(filename).getLines.mkString(" ")
    //    println(s"The file $filename contains ${lines.length} lines.")
    lines
  }



  def main(args: Array[String]): Unit = {
    val text = getLinesFromFile("src/main/scala/WordCount/small.txt")
    implicit val threshold: Int = 15
    println(splitLineAndCountWords(text))
  }

}