import java.io.{BufferedWriter, File, FileWriter}

import sun.nio.cs.ISO_8859_2

import scala.io.Source

object ProcessData{

  // Saves a List of integers to target path
  def saveToFileInt(pathSet: String, targetSet: List[Int])={
    val file = new File(pathSet)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- targetSet){
      bw.write(line + " ")
    }
    bw.close()
  }
  // Saves a List of strings to target path
  def saveToFile(pathSet: String, targetSet: List[String])={
    val file = new File(pathSet)
    val bw = new BufferedWriter(new FileWriter(file))
      for (line <- targetSet){
      bw.write(line + "\n")
    }
    bw.close()
  }
  //This function was used to split the spam.dat into three different groups, then save those groups into 3 different sets:
  //1 - Training Set, containing 60% of the data
  //2 - Cross-Validation Set, containing 20% of the data
  //3 - Test Set, containing 20% of the data
  def splitA(fileName : String) ={
    val bSource = Source.fromFile(fileName).getLines().toList
    val bShuffle = scala.util.Random.shuffle(bSource)
    val sizeBuff: Int =bShuffle.size/10
    val trainingSet = bShuffle.slice(0, sizeBuff*6)
    val crossValidation = bShuffle.slice(sizeBuff*6, sizeBuff*8)
    val testSet = bShuffle.slice(sizeBuff*8, sizeBuff*10)
    saveToFile("src\\main\\resources\\spamdata\\trainingset.dat",trainingSet)
    saveToFile("src\\main\\resources\\spamdata\\crossvalidation.dat",crossValidation)
    saveToFile("src\\main\\resources\\spamdata\\testset.dat",testSet)
  }
  // Separates line into classification and message
  def parse(line: String): List[(Int,String)] = {
  //finds index of first comma
  val index = line.indexOf(",")
  //if (index<0) println(line)

  // convert spam into 1 and ham into 0, separating by first comma
  val classification = line.substring(0,index) match {
    case "ham" => 0
    case "spam" => 1
  }
  // separate the text message
  val text  = line.takeRight(line.length-index)
  List((classification, text))
  }
  // Separates all lines from target file into classification and message
  def parseA(fileName : String): List[(Int,String)] ={
    val bufferedSource = Source.fromFile(fileName).getLines().toList
    def Acc(acc: List[String]): List[(Int,String)] = {
      if (acc.tail.isEmpty) parse(acc.head)
      else parse(acc.head) ::: Acc(acc.tail)
    }
    Acc(bufferedSource)
  }
  // Counts the lenght of every message
  def countLength(allSet: List[(Int, String)]):List[(Int, Int)]={
    allSet.map(x => (x._1, x._2.split(" ").length))
  }

}
