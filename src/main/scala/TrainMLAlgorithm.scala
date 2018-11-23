import DecisionTrees.{CosineTree, EuclideanTree, DecisionTree}
import DefinedStrings.{FilesName, SpecificWords}
import ProcessingInformation.{ProcessData, ProcessSet}
import breeze.linalg._

object TrainMLAlgorithm {

  /**
    * Set timer
    */
  val inicialTime: Long = System.currentTimeMillis

  /**
    * Classes FilesName(), ProcessData(), SpecificWords()
    */
  val fileName = new FilesName()
  val dataProcess = new ProcessData()
  val specificWords = new SpecificWords()

  /**
    * Class that processes the training set
    */
  val trainingSet = new ProcessSet(fileName.fileStopWords, fileName.fileTrainingSet)

  /*
/**
  * Creates matrix TF
  */
val TFmatrix = dataProcess.makeTFMatrix(trainingSet.setStopWords)
/**
  * Creates TFIDF matrix through TF matrix
  */
val TFIDFMatrix = dataProcess.makeTFIDFMatrix(TFmatrix,file.fileListOfWords)
/**
  * Saves in csv file the matrix TFIDF
  */
dataProcess.saveToFile(fileName.fileMatrixTFIDF, TFIDFMatrix)
*/

  /**
    * Read the matrix created in advance and save it in a function
    */
  val TFIDFMatrixCV = dataProcess.readMatrixFromFile(fileName.fileMatrixTFIDF)

  /**
    * Class that processes the cross-validation set
    */
  val crossValidationSet = new ProcessSet(fileName.fileStopWords, fileName.fileCrossValidation)

  /**
    * Processing the cross-validation set
    * Example:
    * Input: (ham,"When did you get to the library")
    * Output: (0,librari)
    */
  val cvSetStopWords = crossValidationSet.setProcessString

  /**
    * Applying the stemmer function to some specific words that strongly indicate spam messages
    */
  val specificKeywords = specificWords.commonSpamWords.map(dataProcess.applyStemmer(_))

  /**
    * Function that applies a decision tree through several features
    * (p.e length, number of specific word, upper cased words, ...)
    */
  val decisionT = DenseVector(cvSetStopWords.map(x => new DecisionTree(
    x._2,
    specificKeywords.map(dataProcess.applyStemmer(_))
  ).decisionTreeTargetString).toArray)

  /**
    * Evaluation of results achieved with the decision tree
    * (False Positive, False Negative, True Positive, True Negative,
    * Accuracy, Precision, Recall, F1-Score)
    */
  dataProcess.evaluationMetrics(crossValidationSet.setVector, decisionT)

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
  val listOfCVSentences = cvSetStopWords.map(x =>
    dataProcess.tokenization(x._2).filterNot(x => x.equals("")))

  /**
    * Filter, from the cross validation set, the sentences that are not considered
    * in the list of Words created from the training set
    */
  val listOfCVintersected: List[List[String]] = listOfCVSentences.map(x =>
    x.filter(y => listOfWords.contains(y)))

  /**
    * Every words is attributed the value of converted sentence into a map where each vector
    * maps the proportion of the words presented in a specific sentence (Term Frequency)
    */
  val convertedMatrix: DenseMatrix[Double] =
    dataProcess.convertedMatrixList(listOfCVintersected, mappedLisfOfWords)

  /**
    * Class that processes the cosine tree
    */
  val cosineTree = new CosineTree(TFIDFMatrixCV, convertedMatrix, listOfCVintersected, trainingSet)

  /**
    * Evaluation of results achieved with the cosine tree
    * (False Positive, False Negative, True Positive, True Negative,
    * Accuracy, Precision, Recall, F1-Score)
    */
  dataProcess.evaluationMetrics(crossValidationSet.setVector, cosineTree.finalCategorizationC)

  /**
    * Class that processes the euclidean tree
    */
  val euclideanTree = new EuclideanTree(TFIDFMatrixCV, convertedMatrix, listOfCVintersected, trainingSet)

  /**
    * Evaluation of results achieved with the euclidean tree
    * (False Positive, False Negative, True Positive, True Negative,
    * Accuracy, Precision, Recall, F1-Score)
    */
  dataProcess.evaluationMetrics(crossValidationSet.setVector, euclideanTree.finalCategorizationE)

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

  println("FINAL METRICS")
  /**
    * Evaluation of results achieved with the euclidean tree
    * (False Positive, False Negative, True Positive, True Negative,
    * Accuracy, Precision, Recall, F1-Score)
    */
  dataProcess.evaluationMetrics(crossValidationSet.setVector, trueCategorization)


  /**
    * Only used for the API response
    */
  val convertToResponse = trueCategorization(0) match {
    case 0 => "not spam"
    case 1 => "spam"
  }

  /**
    * Running time in seconds
    */
  val finalTime = System.currentTimeMillis
  val timeRunning = (finalTime - inicialTime).toDouble / 1000 + " seconds"

}
