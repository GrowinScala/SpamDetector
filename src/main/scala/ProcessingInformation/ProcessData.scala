package ProcessingInformation

import java.io.{ BufferedWriter, File, FileWriter }

import DefinedStrings.{ FilesName, Regex, SpecificWords }
import DefinedValues.ThresholdValues
import breeze.linalg._
import breeze.numerics._

import scala.io.Source

class ProcessData {
  val fileName = new FilesName()
  val regex = new Regex()
  val specificWords = new SpecificWords()

  /**
   * Saves a List of something to target path
   * @param pathName
   * @param targetSet
   * @tparam T
   */
  def saveToFile[T](pathName: String, targetSet: List[T]): Unit = {
    val file = new File(pathName)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- targetSet) {
      bw.write(line + "\n")
    }
    bw.close()
  }

  /**
   * Saves a List of something to target path
   * @param pathName
   * @param matrix
   */
  def saveToFile(pathName: String, matrix: DenseMatrix[Double]): Unit = {
    breeze.linalg.csvwrite(new File(pathName), matrix, separator = ' ')
  }

  val threshV = new ThresholdValues()

  /**
   * Function used to split the spam.dat into three different groups, then save those groups into 3 different sets:
   * 1 - Training Set, containing 60% of the data
   * 2 - Cross-Validation Set, containing 20% of the data
   * 3 - Test Set, containing 20% of the data
   * @param fileName
   * @param trainingSetPath
   * @param crossValidationSetPath
   * @param testSetPath
   */
  def splitA(fileName: String, trainingSetPath: String, crossValidationSetPath: String, testSetPath: String): Unit = {
    val bSource = Source.fromFile(fileName).getLines().toList
    val bShuffle = scala.util.Random.shuffle(bSource)
    /**
     * Dividing the data into 10 parts in order to be easier to split it into the different sets
     */
    val sizeBuff: Int = bShuffle.size / threshV.tenParts
    val trainingSet = bShuffle.slice(threshV.zeroParts, sizeBuff * threshV.sixParts)
    val crossValidation = bShuffle.slice(sizeBuff * threshV.sixParts, sizeBuff * threshV.eightParts)
    val testSet = bShuffle.slice(sizeBuff * threshV.eightParts, sizeBuff * threshV.tenParts)
    saveToFile(trainingSetPath, trainingSet)
    saveToFile(crossValidationSetPath, crossValidation)
    saveToFile(testSetPath, testSet)
  }

  /**
   * Separates line into classification and message
   * @param line
   * @return
   */
  def parse(line: String): List[(Int, String)] = {
    /**
     * Finds index of first comma
     */
    val index = line.indexOf(",")
    /**
     * Convert spam into 1 and ham into 0, separated by first comma
     */
    val classification = line.substring(0, index) match {
      case "ham" => threshV.categorizeHam
      case "spam" => threshV.categorizeSpam
    }
    /**
     * Separate the text message
     */
    val text = line.takeRight(line.length - index - 1)
    List((classification, text))
  }

  /**
   * Reads from target path name, return a list of strings
   * @param pathName
   * @return
   */
  def readListFromFile(pathName: String): List[String] = {
    Source.fromFile(pathName).getLines().toList
  }

  /**
   * Reads from target path name, return a list of strings
   * @param pathName
   * @return
   */
  def readMatrixFromFile(pathName: String): DenseMatrix[Double] = {
    breeze.linalg.csvread(new File(pathName), separator = ' ')
  }

  /**
   * Separates all lines from target file into classification and message
   * @param targetSet
   * @return
   */
  def parseA(targetSet: List[String]): List[(Int, String)] = {
    val bufferedSource = targetSet
    def Acc(acc: List[String]): List[(Int, String)] = {
      if (acc.tail.isEmpty) parse(acc.head)
      else parse(acc.head) ::: Acc(acc.tail)
    }
    Acc(bufferedSource)
  }

  // Counts the lenght of every message
  def countLength(allSet: String):  Int = {
    allSet.split(" ").length
  }

  /**
   * Delete all the punctuation presented in every text message (apostrophe and hyphens are maintained)
   *
   * @param targetSet
   * @return
   */
  def takePunctuation(targetSet: String): String =
    targetSet.replaceAll(regex.regexPunctuation, " ")

  /**
   * Separates each sentence by the words that compose it in different strings
   * @param targetMessage
   * @return
   */
  def tokenization(targetMessage: String): List[String] = {
    targetMessage.split(" ").toList
  }

  /**
   * Remove all words that match with the words contained in stopWords list
   * @param stopWords
   * @param targetSet
   * @return
   */
  def takeStopWords(stopWords: List[String], targetSet: String): String = {
    tokenization(targetSet).filterNot(stopWords.contains(_)).mkString(" ") //.filter(stopWordsList))
  }

  /**
   * Turn all upper characters to lower
   * @param targetSet
   * @return
   */
  def uppertoLower(targetSet: String): String = {
    targetSet.toLowerCase()
  }

  /**
   *
   * @param targetSet
   * Converts words using Porter stemmer
   * @return
   */
  def applyStemmer(targetSet: String): String = {
    tokenization(targetSet).map(y => Stemmer.Stemmer.stem(y)).mkString(" ")
  }

  /**
   * Simplify data by grouping in a *word* (Strings must be lower case)
   * Example:
   * Input: 910000000
   * Output: " PHONENUMBER "
   * @param targetSet
   * @return
   */
  def replaceOverall(targetSet: String): String = {
    targetSet
      .replaceAll(regex.regexWebsite, " " + specificWords.WEBSITE + " ")
      .replaceAll(regex.regexSmile, " " + specificWords.SMILE + " ")
      .replaceAll(regex.regexTripleDot, " " + specificWords.TRIPLEDOT + " ")
      .replaceAll(regex.regexPhoneNumber, " " + specificWords.PHONENUMBER + " ")
      .replaceAll(regex.regexPer, " " + specificWords.PER + " ")
      .replaceAll(regex.regexWeekDays, " ")
      .replaceAll(regex.regexMonth, " ")
      .replaceAll(regex.regexMoney, " " + specificWords.MONEY + " ")
      .replaceAll(regex.regexDate, " ")
      .replaceAll(regex.regexNumber, " ")
      .replaceAll(regex.regexRepetition, " " + specificWords.REPETITION + " ")
  }

  def listOfWordsF(targetSet: List[(Int, String)]): List[String] = {
    /**
     * List of sentences from target set, without empty strings
     */
    val listOfSentences = targetSet.map(x => tokenization(x._2).filterNot(x => x.equals("")))
    /**
     * List of words of every sentence without repetitions, without empty strings
     */
    val listOfWords = targetSet.foldLeft(List(""))((s, x) => tokenization(x._2) ++ s).distinct.sorted.filterNot(x => x.equals(""))
    listOfWords
  }

  /**
   * Make term frequency matrix from target set
   * @param targetSet
   * @return
   */
  def makeTFMatrix(targetSet: List[(Int, String)],listOfWordsPath : String): DenseMatrix[Double] = {
    /**
     * List of sentences from target set, without empty strings
     */
    val listOfSentences = targetSet.map(x => tokenization(x._2).filterNot(x => x.equals("")))
    /**
     * List of words of every sentence without repetitions, without empty strings
     */
    val listOfWords = targetSet.foldLeft(List(""))((s, x) => tokenization(x._2) ++ s).distinct.sorted.filterNot(x => x.equals(""))
    /**
     * Converted words into a map pointing to 0
     */
    val mappedLisfOfWords: Map[String, Double] = listOfWords.map(x => x -> 0.0).toMap
    saveToFile(listOfWordsPath, mappedLisfOfWords.keys.toList)
    /**
     * Every words is atributted the value of converted sentence into a map
     * Where each vector maps the proportion of the words presented in a specific sentence(Term Frequency)
     */
    val convertedVectorList: List[DenseVector[Double]] = listOfSentences.map(x =>
      DenseVector((mappedLisfOfWords ++ x.foldLeft(Map.empty[String, Double]) {
        (count, word) => count + (word -> (count.getOrElse(word, 0.0) + 1.0))
      }).values.toArray))
    /**
     * Restructure a list of vectors into a matrix
     */
    val matrix = DenseMatrix(convertedVectorList: _*)
    /**
     * Transpose matrix
     */
    matrix.t
  }

  /**
   * Calculates TF*IDF matrix
   * @param TFMatrix
   * @return
   */
  def makeTFIDFMatrix(TFMatrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    val TFMatrixCols = TFMatrix.cols
    /**
     * Maps each row of a matrix to is value times log( 1+ total number of columns/ number of documents
     * that contains the term at least once)
     */
    val TFIDFMatrix = TFMatrix(*, ::).map(row => {
      val countRow = row.findAll(x => x != 0.0).length
      row.map(x => if (x != 0.0) x * sigmoid(TFMatrixCols.toDouble / countRow) else x)
    })
    TFIDFMatrix
  }

  /**
   * This function will calculate the cosine similarity between the convertedMatrix and TFIDF Matrix
   * It will return a matrix where each row represents the different values between a string j of cross
   * validation and the various strings of the training set
   * @param TFIDFMatrix
   * @param convertedMatrix
   * @return
   */
  def cosineVector(TFIDFMatrix: DenseMatrix[Double], convertedMatrix: DenseMatrix[Double]): DenseMatrix[Double] =
    convertedMatrix(::, *).map(cols1 => TFIDFMatrix(::, *).map(cols2 => cosineSimilarity(cols1, cols2)).inner).toDenseMatrix.t

  /**
   * This method takes 2 equal length arrays of doubles
   * It returns a double representing similarity of the 2 arrays
   * 0.9925 would be 99.25% similar
   * (x dot y)/||X|| ||Y||
   * @param x
   * @param y
   * @return
   */
  def cosineSimilarity(x: DenseVector[Double], y: DenseVector[Double]): Double = {
    /**
     * Return the dot product of the 2 arrays
     * e.g. (a[0]*b[0])+(a[1]*a[2])
     * @param x
     * @param y
     * @return
     */
    def dotProduct(x: DenseVector[Double], y: DenseVector[Double]): Double = {
      x dot y
    }
    /*
     * Return the magnitude of an array
     * We multiply each element, sum it, then square root the result.
     */
    def magnitude(x: DenseVector[Double]): Double = sqrt(x dot x)
    val magMultiplication = magnitude(x) * magnitude(y)
    if (magMultiplication != 0.0) dotProduct(x, y) / magMultiplication
    else 0
  }

  def convertedMatrixHead(listAux: List[String] ,mappedLisfOfWords: Map[String, Double]): DenseMatrix[Double] = {
    DenseMatrix((mappedLisfOfWords ++ listAux.foldLeft(Map.empty[String, Double]) {
      (count, word) => count + (word -> (count.getOrElse(word, 0.0) + 1.0))
    }).values.toArray)
  }
  /**
   * Every words is attributed the value of converted sentence into a map where each vector
   * maps the proportion of the words presented in a specific sentence (Term Frequency)
   * @param listOfIntersected
   * @param mappedLisfOfWords
   * @return
   */
  def convertedMatrixList(listOfIntersected: List[List[String]], mappedLisfOfWords: Map[String, Double]): DenseMatrix[Double] = {
    def convertedAux(listAux: List[List[String]], mappedListAux: Map[String, Double]): DenseMatrix[Double] = {
      if (listAux.tail.isEmpty) convertedMatrixHead(listAux.head ,mappedLisfOfWords)
      else
        DenseMatrix.vertcat(DenseMatrix((mappedLisfOfWords ++ listAux.head.foldLeft(Map.empty[String, Double]) {
          (count, word) => count + (word -> (count.getOrElse(word, 0.0) + 1.0))
        }).values.toArray), convertedAux(listAux.tail, mappedListAux))
    }
    convertedAux(listOfIntersected, mappedLisfOfWords).t
  }

  /**
   * Function that calculates the Euclidean distance between two vectors
   * @param x
   * @param y
   * @return
   */
  def euclidianDistance(x: DenseVector[Double], y: DenseVector[Double]): Double = {
    def dotProduct(x: DenseVector[Double], y: DenseVector[Double]): Double = {
      x dot y
    }
    val xy = x - y
    sqrt(dotProduct(xy, xy))
  }

  /**
   * Applies Euclidean distance to every collumn of both matrices
   * @param TFIDFMatrix
   * @param convertedMatrix
   * @return
   */
  def distanceVector(TFIDFMatrix: DenseMatrix[Double], convertedMatrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    convertedMatrix(::, *).map(cols1 => TFIDFMatrix(::, *).map(cols2 => euclidianDistance(cols1, cols2)).inner).toDenseMatrix.t
  }

  def evaluationMetrics(categories: DenseVector[Int], catPositions: DenseVector[Int]): Unit = {
    /**
     * The vector Categories is multiplied by 1 and summed with the vector catPositions that is multiplied by 2
     * This will help us to distinct the different cases (0,0), (0,1), (1, 0) and (1, 1)
     */
    val scoreVector = categories + catPositions * 2
    val falsePosIndex = (0 until scoreVector.length).map(i => if (scoreVector.data(i) == 2) i).toSet
    val falseNegIndex = (0 until scoreVector.length).map(i => if (scoreVector.data(i) == 1) i).toSet
    val truePosIndex = (0 until scoreVector.length).map(i => if (scoreVector.data(i) == 3) i).toSet
    val trueNegIndex = (0 until scoreVector.length).map(i => if (scoreVector.data(i) == 0) i).toSet
    val truePos: Double = (scoreVector :== 3).activeSize
    val falseNeg: Double = (scoreVector :== 1).activeSize
    val falsePos: Double = (scoreVector :== 2).activeSize
    val trueNeg: Double = (scoreVector :== 0).activeSize
    println(s"False Positive %: ${falsePos / (falsePos + trueNeg)}")
    println(s"False Positive: $falsePos")
    println(s"False Positive indexes: $falsePosIndex")
    println(s"False Negative indexes: $falseNegIndex")
    println(s"True Positive indexes: $truePosIndex")
    println(s"True Negative indexes: $trueNegIndex")
    println(s"Accuracy (% of predictions that were correct): ${(truePos + trueNeg) / categories.activeSize}")
    println(s"Precision (% of emails classified as spam that were actually spam): ${truePos / (truePos + falsePos)}")
    println(s"Recall (% of spam emails that were predicted correctly): ${truePos / (truePos + falseNeg)}")
    println(s"F1-score: ${2 * truePos / (2 * truePos + falseNeg + falsePos)}")
  }

  /**
   * Functions that verifies if the intersection of two lists of strings is greater or equal than 3
   * @param stringList
   * @param targetStringList
   * @return
   */
  def containsString(stringList: List[String], targetStringList: List[String]): Boolean = {
    stringList.intersect(targetStringList).length >= threshV.string3
  }

  /**
   * Function that categorize the list of messages as ham or spam through the decision tree constructed
   * @param categorizePositionsE
   * @param listOfIntersected
   * @param specificKeywords
   * @return
   */
  def decisionTree(categorizePositionsE: DenseVector[Int], listOfIntersected: List[List[String]], specificKeywords: List[String]): DenseVector[Int] = {
    val categorizedWithList = categorizePositionsE.data.zip(listOfIntersected)
    val stemmedSpecificKeywords = specificKeywords.map(applyStemmer(_))
    val newCategorization = categorizedWithList.map(x => if (x._1 == threshV.categorizeHam && containsString(x._2, stemmedSpecificKeywords)) threshV.categorizeSpam else x._1)
    DenseVector(newCategorization)
  }

  /**
   * Verifies if the intersection of two Lists of strings is equal or greater than 3
   * @param thresh
   * @param targetString
   * @param specificKeywords
   * @return
   */
  def containsMoreString(thresh: Int, targetString: List[String], specificKeywords: List[String]): Boolean = {
    val stemmedSpecificKeywords = specificKeywords.map(applyStemmer(_))
    targetString.intersect(stemmedSpecificKeywords).length >= thresh
  }

  /**
   * Verifies if the intersection of two Lists of strings is equal or less than 3
   * @param thresh
   * @param targetString
   * @param specificKeywords
   * @return
   */
  def containsLessString(thresh: Int, targetString: List[String], specificKeywords: List[String]): Boolean = {
    val stemmedSpecificKeywords = specificKeywords.map(applyStemmer(_))
    targetString.intersect(stemmedSpecificKeywords).length <= thresh
  }
}
