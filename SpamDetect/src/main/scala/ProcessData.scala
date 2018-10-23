import java.io.{BufferedWriter, File, FileWriter}

import breeze.linalg.{DenseMatrix, DenseVector}
import shapeless._
import sun.nio.cs.ISO_8859_2

import scala.io.Source

object ProcessData{

  // Saves a List of integers to target path
  /*def saveToFileInt(pathSet: String, targetSet: List[Int])={
    val file = new File(pathSet)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- targetSet){
      bw.write(line + " ")
    }
    bw.close()
  }*/

  // Saves a List of something to target path
  def saveToFile[T](pathSet: String, targetSet: List[T]): Unit ={
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
    //Dividing the data into 10 parts in order to be easier to split it into the different sets
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

  // Convert spam into 1 and ham into 0, separating by first comma
  val classification = line.substring(0,index) match {
    case "ham" => 0
    case "spam" => 1
  }

  // separate the text message
  val text  = line.takeRight(line.length-index-1)
  List((classification, text))
  }

  //Reads from target path name, return a list of strings
  def readFromFile(fileName: String): List[String]={
    Source.fromFile(fileName).getLines().toList
  }

  // Separates all lines from target file into classification and message
  def parseA(fileName : String, targetSet: List[String]): List[(Int,String)] ={
    val bufferedSource = targetSet
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

  //Delete all the punctuation presented in every text message (apostrophe and hyphens are maintained)
  def takePunctuation(targetSet:List[(Int,String)]):List[(Int,String)]=
    targetSet.map(x=> (x._1,x._2.replaceAll("\\p{Punct}", " ")))

  //Separates each sentence by the words that compose it in different strings
  def tokenization(targetMessage:String): List[String] = {
    targetMessage.split(" ").toList
  }

  //Remove all words that match with the words contained in stopWords list
  def takeStopWords(stopWords: List[String], targetSet:List[(Int,String)]):List[(Int,String)]={
    targetSet.map(x=> (x._1,tokenization(x._2).filterNot(stopWords.contains(_)).mkString(" ")))//.filter(stopWordsList))
  }

  //Turn all upper characters to lower
  def uppertoLower(targetSet:List[(Int,String)]):List[(Int,String)]={
    targetSet.map(x=> (x._1,x._2.toLowerCase()))
  }

  //Converts words using Porter stemmer
  def applyStemmer(targetSet:List[String]):List[String]={
    targetSet.map(x => tokenization(x).map(y=>Stemmer.stem(y)).mkString(" "))
  }

  //Simplify data by grouping in a *word* (Strings must be lower case)
  //ex: Replace 910000000(digits of mobile number) -> "*phonenumber*"
  def replaceOverall(targetSet:List[(Int,String)]):List[(Int,String)]={
      targetSet.map(x => (x._1,x._2
        .replaceAll("(\\S*www\\.\\S*)|(\\S*\\.com\\S*)", " WEBSITE ")
        .replaceAll("(:\\p{Punct}+)|(:\\w\\s+)"," SMILE ")
        .replaceAll("\\.{3}"," TRIPLEDOT ")
        .replaceAll("\\d{5,}", " PHONENUMBER ")
        .replaceAll("\\w{1,4}\\/\\w{1,4}"," PER ")
        .replaceAll("(\\p{Punct}+|\\W)((mon)|(monday)|(tue)|(tuesday)|(wed)|(wednesday)|(thu)|(thursday)|(friday)|(saturday)|(sunday))(\\p{Punct}+|\\W)"," WEEKDAY ")
        .replaceAll("(\\p{Punct}+|\\W)((jan)|(january)|(feb)|(february)|(mar)|(march)|(apr)|(april)|(may)|(jun)|(june)|(jul)|(july)" +
          "|(aug)|(august)|(sep)|(september)|(oct)|(october)|(nov)|(november)|(dec)|(december))(\\p{Punct}+|\\W)"," MONTH ")
        .replaceAll("(\\d+\\W*pound\\w*)|(\\d+\\W*dollar\\w*)|(\\d+\\W*cash\\w*)|(\\d+\\W*euro\\w*)|(\\d+\\W*p\\W)", " MONEY ")
        .replaceAll("(?:(?:31(\\/|-|\\.)(?:0?[13578]|1[02]))\\1|(?:(?:29|30)(\\/|-|\\.)" +
          "(?:0?[1,3-9]|1[0-2])\\2))(?:(?:1[6-9]|[2-9]\\d)?\\d{2})|(?:29(\\/|-|\\.)0?2\\3(?:(?:" +
          "(?:1[6-9]|[2-9]\\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))" +
          "|(?:0?[1-9]|1\\d|2[0-8])(\\/|-|\\.)(?:(?:0?[1-9])|(?:1[0-2]))\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})", " DATE ")
        .replaceAll("\\d{1,4}", " NUMBER ")

      )
    )
  }

  //Make term frequency matrix from target set
  def makeTFMatrix(targetSet : List[(Int,String)]): DenseMatrix[Float] = {

    //List of sentences from target set, without empty strings
    val listOfSentences = targetSet.map(x=> tokenization(x._2).filterNot(x=> x.equals("")))

    //List of words of every sentence without repetitions, without empty strings
    val listOfWords= targetSet.foldLeft(List(""))((s,x)=> tokenization(x._2) ++ s ).distinct.sorted.filterNot(x=> x.equals(""))

    //Converted words into a map pointing to 0
    val mappedLisfOfWords : Map[String,Float]= listOfWords.map(x=> x->0f).toMap

    //Every words is atributted the value of converted sentence into a map
    // Where each vector maps the proportion of the word presented in a specific sentence(Term Frequency)
    val convertedVectorList : List[DenseVector[Float]] = listOfSentences.map(x=>
                  DenseVector((mappedLisfOfWords ++ x.foldLeft(Map.empty[String, Float]){
                      (count, word) => count + (word -> (count.getOrElse(word, 0f) + (1f/x.length)))
                    }).values.toArray))

    //Restructure a list of vectors into a matrix
    val matrix = DenseMatrix(convertedVectorList:_*)

    //transpose matrix
    matrix.t
  }

}
