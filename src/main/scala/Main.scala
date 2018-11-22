import DecisionTrees.{CosineTree, EuclideanTree, DecisionTree}
import DefinedStrings.{FilesName, SpecificWords}
import ProcessingInformation.{ProcessData, ProcessSet}
import breeze.linalg._

class Main(SMS: String) {

  /**
   * Classes FilesName(), ProcessData(), SpecificWords()
   */
  val fileName = new FilesName()
  val dataProcess = new ProcessData()
  val specificWords = new SpecificWords()

  /**
   * Class that processes data set
   */
  val allSetProcessed = new ProcessSet(fileName.fileStopWords, fileName.fileTrainingSet)

  /**
   * Creates matrix TF
   */
  val TFmatrix = dataProcess.makeTFMatrix(allSetProcessed.setProcessString,fileName.fileListOfWords)
  /**
   * Creates TFIDF matrix through TF matrix
   */
  val TFIDFMatrix = dataProcess.makeTFIDFMatrix(TFmatrix)
  /**
   * Saves in csv file the matrix TFIDF
   */
  dataProcess.saveToFile(fileName.dataSetfileMatrixTFIDF, TFIDFMatrix)

  /**
   * Read the matrix created in advance and save it in a function
   */
  val dataSetTFIDFMatrixCV = dataProcess.readMatrixFromFile(fileName.fileMatrixTFIDF)

  /**
    * Process target SMS
    * Example:
    * Input: "When did you get to the library"
    * Output: "librari"
    */
  val processedSMS = allSetProcessed.processString(SMS)

  /**
    * Applying the stemmer function to some specific words that strongly indicate spam messages
    */
  val specificKeywords = specificWords.commonSpamWords.map(dataProcess.applyStemmer(_))


  /**
    * Function that applies a decision tree through several features
    * (p.e length, number of specific word, upper cased words, ...)
    */

  val decisionT = new DecisionTree(processedSMS, specificKeywords.map(dataProcess.applyStemmer(_)))
    .decisionTreeTargetString

  /**
    * Read list of words that were achieved in the training set data after filtered
    */
  val listOfWords = dataProcess.readListFromFile(fileName.fileListOfWords)

  /**
    * Maps each word with the value 0
    */
  val mappedLisfOfWords: Map[String, Double] = listOfWords.map(x => x -> 0.0).toMap

  /**
    * List of words of every sentence without repetitions, without empty strings
    */
  val listOfSentences = dataProcess.tokenization(processedSMS).filterNot(x => x.equals(""))

  /**
    * Filter, from the cross validation set, the sentences that are not considered
    * in the list of Words created from the training set
    */
  val listOfIntersected: List[String] = listOfSentences.map(x =>
    x.filter(y => listOfWords.contains(y)))

  /**
    * Every words is attributed the value of converted sentence into a map where each vector
    * maps the proportion of the words presented in a specific sentence (Term Frequency)
    */
  val convertedMatrix: DenseMatrix[Double] =
    dataProcess.convertedMatrixList(List(listOfIntersected), mappedLisfOfWords)

  /**
    * Class that processes the cosine tree
    */
  val cosineTree = new CosineTree(dataSetTFIDFMatrixCV, convertedMatrix, List(listOfIntersected), allSetProcessed)


  /**
    * Class that processes the euclidean tree
    */
  val euclideanTree = new EuclideanTree(dataSetTFIDFMatrixCV, convertedMatrix, List(listOfIntersected), allSetProcessed)


  /**
    * Matching the three trees calculated applying Random Forest "algorithm"
    */
  val trueCategorization = (cosineTree.finalCategorizationC + euclideanTree.finalCategorizationE + decisionT).map(
    {
      case 0 => 0
      case 1 => 0
      case 2 => 1
      case 3 => 1
    }
  )

  /**
    * Only used for the API response
    */
  val convertToResponse = trueCategorization(0) match {
    case 0  => "not spam"
    case 1 => "spam"
  }

}