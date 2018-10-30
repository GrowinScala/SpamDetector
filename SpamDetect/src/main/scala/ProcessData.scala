import java.io.{BufferedWriter, File, FileWriter}

import breeze.linalg
import breeze.linalg._
import breeze.linalg.support.CanSlice2
import breeze.numerics._
import sun.nio.cs.ISO_8859_2
import scala.io.Source

 object ProcessData {
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
  def saveToFile[T](pathName: String, targetSet: List[T]): Unit ={
    val file = new File(pathName)
    val bw = new BufferedWriter(new FileWriter(file))
      for (line <- targetSet){
      bw.write(line + "\n")
    }
    bw.close()
  }

  // Saves a List of something to target path
  def saveToFile(pathName: String, matrix: DenseMatrix[Double]): Unit ={
    breeze.linalg.csvwrite(new File(pathName), matrix, separator = ' ')
  }

  //This function was used to split the spam.dat into three different groups, then save those groups into 3 different sets:
  //1 - Training Set, containing 60% of the data
  //2 - Cross-Validation Set, containing 20% of the data
  //3 - Test Set, containing 20% of the data
  def splitA(fileName : String):Unit ={
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
  def readListFromFile(pathName: String): List[String]={
    Source.fromFile(pathName).getLines().toList
  }

  //Reads from target path name, return a list of strings
  def readMatrixFromFile(pathName: String): DenseMatrix[Double]={
    breeze.linalg.csvread(new File(pathName), separator = ' ')
  }

  // Separates all lines from target file into classification and message
  def parseA(targetSet: List[String]): List[(Int,String)] ={
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
        .replaceAll("(\\p{Punct}+|\\W)((mon)|(monday)|(tue)|(tuesday)|(wed)|(wednesday)|(thu)|(thursday)|(friday)|(saturday)|(sunday))(\\p{Punct}+|\\W)"," ")
        .replaceAll("(\\p{Punct}+|\\W)((jan)|(january)|(feb)|(february)|(mar)|(march)|(apr)|(april)|(may)|(jun)|(june)|(jul)|(july)" +
          "|(aug)|(august)|(sep)|(september)|(oct)|(october)|(nov)|(november)|(dec)|(december))(\\p{Punct}+|\\W)"," ")
        .replaceAll("(\\d+\\W*pound\\w*)|(\\d+\\W*dollar\\w*)|(\\d+\\W*cash\\w*)|(\\d+\\W*euro\\w*)|(\\d+\\W*p\\W)", " MONEY ")
        .replaceAll("(?:(?:31(\\/|-|\\.)(?:0?[13578]|1[02]))\\1|(?:(?:29|30)(\\/|-|\\.)" +
          "(?:0?[1,3-9]|1[0-2])\\2))(?:(?:1[6-9]|[2-9]\\d)?\\d{2})|(?:29(\\/|-|\\.)0?2\\3(?:(?:" +
          "(?:1[6-9]|[2-9]\\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))" +
          "|(?:0?[1-9]|1\\d|2[0-8])(\\/|-|\\.)(?:(?:0?[1-9])|(?:1[0-2]))\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})", " ")
        .replaceAll("\\d{1,4}", " ")
        .replaceAll("([a-z])\\1{2,}"," REPETITION ")
      )
    )
  }

   def listOfWordsF(targetSet : List[(Int,String)]): List[String]= {
     //List of sentences from target set, without empty strings
     val listOfSentences = targetSet.map(x=> tokenization(x._2).filterNot(x=> x.equals("")))
     //List of words of every sentence without repetitions, without empty strings
     val listOfWords= targetSet.foldLeft(List(""))((s,x)=> tokenization(x._2) ++ s ).distinct.sorted.filterNot(x=> x.equals(""))
     listOfWords
   }


  //Make term frequency matrix from target set
  def makeTFMatrix(targetSet : List[(Int,String)]): DenseMatrix[Double] = {

    //List of sentences from target set, without empty strings
    val listOfSentences = targetSet.map(x=> tokenization(x._2).filterNot(x=> x.equals("")))

    //List of words of every sentence without repetitions, without empty strings
    val listOfWords= targetSet.foldLeft(List(""))((s,x)=> tokenization(x._2) ++ s ).distinct.sorted.filterNot(x=> x.equals(""))

    //Converted words into a map pointing to 0
    val mappedLisfOfWords : Map[String,Double]= listOfWords.map(x=> x->0.0).toMap
    saveToFile("src\\main\\resources\\spamdata\\listOfWords.dat", mappedLisfOfWords.keys.toList)

    //Every words is atributted the value of converted sentence into a map
    // Where each vector maps the proportion of the words presented in a specific sentence(Term Frequency)
    val convertedVectorList : List[DenseVector[Double]] = listOfSentences.map(x =>
                  DenseVector((mappedLisfOfWords ++ x.foldLeft(Map.empty[String, Double]){
                      (count, word) => count + (word -> (count.getOrElse(word, 0.0) + 1.0/x.length))
                    }).values.toArray))
    //Restructure a list of vectors into a matrix
    val matrix = DenseMatrix(convertedVectorList:_*)

    //transpose matrix
    matrix.t
  }


   //Makes tf * idf matrix
  def makeTFIDFMatrix(TFMatrix: DenseMatrix[Double]) :DenseMatrix[Double]={

    val TFMatrixCols = TFMatrix.cols
    //Maps each row of a matrix to is value times log( 1+ total number of columns/ number of documents that contains the term at least once)
    val TFIDFMatrix = TFMatrix(*,::).map(row=> {
    //val countRow = row.foldLeft(0.0)((count, element) => count + (if (element!=0) 1.0 else 0.0))
    val countRow = row.findAll(x => x!=0.0).length
      row.map(x=> if(x!= 0.0) x* sigmoid(TFMatrixCols.toDouble/countRow) else x)
    })

    TFIDFMatrix
  }

   /*
   * This method takes 2 equal length arrays of doubles
   * It returns a double representing similarity of the 2 arrays
   * 0.9925 would be 99.25% similar
   * (x dot y)/||X|| ||Y||
   */
   def cosineSimilarity(x: Array[Double], y: Array[Double]): Double = {
       /*
      * Return the dot product of the 2 arrays
      * e.g. (a[0]*b[0])+(a[1]*a[2])
      */
       def dotProduct(x: Array[Double], y: Array[Double]): Double = {
         (for ((a, b) <- x zip y) yield a * b) sum
       }

       /*
        * Return the magnitude of an array
        * We multiply each element, sum it, then square root the result.
        */
       def magnitude(x: Array[Double]): Double = {
         math.sqrt(x map (i => i * i) sum)
       }
       require(x.length == y.length)

     val magMultiplication = magnitude(x) * magnitude(y)
     if (magMultiplication !=0.0) dotProduct(x, y) / magMultiplication
     else 0
   }




   def f1Score(cvCategories: List[Int], catPositions: List[Int]): Double = {
     def auxScore(cvCat: List[Int], catPos: List[Int]): List[Int] = {
       if (cvCat.tail.isEmpty)
         cvCat.head match {
           case 0 => if (catPos.head==0) List(0, 0, 0)
           else List(0, 0, 1)
           case 1 => if (catPos.head==0) List(0, 1, 0)
           else List(1, 0, 0)
         }
       else
         cvCat.head match {
           case 0 => if (catPos.head==0) auxScore(cvCat.tail, catPos.tail)
           else (List(0, 0, 1),auxScore(cvCat.tail, catPos.tail)).zipped.map(_ + _)
           case 1 => if (catPos.head==0) (List(0, 1, 0),auxScore(cvCat.tail, catPos.tail)).zipped.map(_ + _)
           else (List(1, 0, 0),auxScore(cvCat.tail, catPos.tail)).zipped.map(_+_)
         }
     }
     //List(TruePositive,FalseNegative,FalsePositive)
     val Score: List[Int] = auxScore(cvCategories, catPositions)
     val truePos: Double = Score.head.toDouble
     val falseNeg:Double = Score.tail.head.toDouble
     val falsePos:Double = Score.tail.tail.head.toDouble
     2*truePos/(2*truePos+falseNeg+falsePos)
   }

   def values(cosineVector : List[DenseVector[Double]], numberMax : Int ): List[Seq[Double]] = {

     def auxValues(auxVector : DenseVector[Double], numberMax : Int ) : Seq[Double]= {

       val indexMax = argmax(auxVector)

       if(numberMax == 1) auxVector(indexMax) +: Seq()
       else auxVector(indexMax) +: auxValues(DenseVector.vertcat(auxVector.slice(0,indexMax),auxVector.slice(indexMax+1,auxVector.length)),numberMax-1)

     }
     cosineVector.map(x => auxValues(x,numberMax))
   }

   def ponderationValues(values : List[Seq[Double]], positions : List[IndexedSeq[Int]], trainingSet : List[(Int,String)]): List[Int] = {

       def associateValPos (values : List[Seq[Double]], positions : List[IndexedSeq[Int]]) : List[Seq[(Int,Double)]] = {

         if(values.tail.isEmpty) positions.head.zip(values.head) :: Nil
         else positions.head.zip(values.head) +: associateValPos(values.tail,positions.tail)

       }

     val ponderation =  associateValPos(values,positions).map(x=> x.foldLeft(0.0)((count,y)=> if(trainingSet.drop(y._1).head._1 == 0) y._2 + count else count - y._2))
     ponderation.map(x=> if(x > 0) 0 else 1)

   }

   def positions(cosineVector : List[DenseVector[Double]], numberMax : Int ): List[IndexedSeq[Int]] = {

     cosineVector.map(x => argtopk(x,numberMax))

   }

   def seeMajority( positions : List[IndexedSeq[Int]], trainingSet: List[(Int,String)]): List[Int] ={

     val majority = positions.map(x => x.foldLeft(0)((count,y) => trainingSet.drop(y).head._1 + count ))
     majority.map(x=> if( x<= 1) 0 else 1)

   }

}
