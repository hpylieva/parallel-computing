import scala.io.Source

object WordCount{

  trait Monoid[A]{
    def op(x: A, y: A): A
    def zero: A
  }

  object WordCounter{
//     I need this method to be static (belong to a class not an object of a class)
    def initWordCounter(text: String): WordCounter = {
      val words = text.split("\\W+").toList
//      val firstPart = text.takeWhile(_ != ' ')
//      val lastPart = text.split(" ").last
//      val middleWordCount = text.split(" ").length - 2
//      println(firstPart,middleWordCount, lastPart)
//      new WordCounter(firstPart, middleWordCount, lastPart)
      println(words.head, words.length - 2, words.last)
      new WordCounter(words.head, words.length - 2, words.last)
    }
  }

  case class WordCounter(leftPart: String, countWords: Int, rightPart: String)

  val wordCountMonoid =  new Monoid[WordCounter] {
    override def op(x: WordCounter, y: WordCounter): WordCounter =
      WordCounter(x.leftPart, x.countWords + y.countWords + 1, y.rightPart)
    override def zero: WordCounter = WordCounter("",0,"")
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

    val result = iter(text)
    // add 2 as we have words in left and right parts
    result.countWords + 2
  }

  def getLinesFromFile(filename: String) = {
    val lines = Source.fromFile(filename).getLines.mkString("\n")
//    println(s"The file $filename contains ${lines.length} lines.")
    lines
  }



  def main(args: Array[String]): Unit = {
    val text = getLinesFromFile("src/main/scala/WordCount/small.txt")
    implicit val threshold: Int = 10
    println(splitLineAndCountWords(text))
  }

}