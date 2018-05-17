import scala.io.Source

object WordCount{

  trait Monoid[A]{
    def op(x: A, y: A): A
    def zero: A
  }

  case class WordCounter(leftPart: String, countWords: Int, rightPart: String)

  val wordCountMonoid =  new Monoid[WordCounter] {
    override def op(x: WordCounter, y: WordCounter): WordCounter =
      WordCounter(x.leftPart, x.countWords+y.countWords +1, y.rightPart)
    override def zero: WordCounter = WordCounter("",0,"")
  }

  def initWordCounter(text: String): WordCounter = {
    val firstPart = text.takeWhile(_ != ' ')
    val lastPart = text.split(" ").last
    val middleWordCount = text.split(" ").length - 2
    new WordCounter(firstPart, middleWordCount, lastPart)
  }

  def splitLineAndCountWords(text: String)(implicit threshold:Int) : Int = {

    def iter(text: String): WordCounter = {
      if (text.length < threshold)
        initWordCounter(text)
      else {
        val (left, right) = text.splitAt(text.length / 2)
        wordCountMonoid.op(new WordCounter(left, 0, right))

      }
    }
  }

  def getLinesFromFile(filename: String) = {
    val lines = Source.fromFile(filename).getLines.mkString
    println(s"The file $filename contains ${lines.length} lines.")
    lines
  }

  implicit val threshold: Int = 1

  def main(args: Array[String]): Unit = {
    print(getLinesFromFile("src/main/scala/WordCount/small.txt"))
  }

}