import scala.io.Source

object ProcessData{

  def parse(line: String): List[(Int,String)] = {

  val index = line.indexOf(",")
  //if (index<0) println(line)
  val classification = line.substring(0,index) match {
    case "ham" => 0
    case "spam" => 1
  }
  val text  = line.takeRight(line.length-index)
  List((classification, text))

  }


  def parseA(fileName : String): List[(Int,String)] ={
    val bufferedSource = Source.fromFile(fileName).getLines().toList
    def Acc(acc: List[String]): List[(Int,String)] = {
      if (acc.tail.isEmpty) parse(acc.head)
      else parse(acc.head) ::: Acc(acc.tail)
    }
    Acc(bufferedSource)
  }



}
