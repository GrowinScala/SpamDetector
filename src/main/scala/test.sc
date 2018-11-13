
import DecisionTrees.{CosineTree, EuclideanTree}
import DefinedStrings.{FilesName, SpecificWords}
import ProcessingInformation.{ProcessData, ProcessSet}
import breeze.linalg._
//import ProcessingInformation.ProcessData._
import DecisionTrees.DecisionTree._

val inicialTime: Long = System.currentTimeMillis

val fileName = new FilesName()
val dataProcess = new ProcessData()
val specificWords = new SpecificWords()

val trainingSet = new ProcessSet(fileName.fileStopWords, fileName.fileTrainingSet)

val TFmatrix = dataProcess.makeTFMatrix(trainingSet.setStopWords)
//Creates TFIDF matrix through TF matrix
val TFIDFMatrix = dataProcess.makeTFIDFMatrix(TFmatrix)
//Saves in csv file the matrix TFIDF
dataProcess.saveToFile(fileName.fileMatrixTFIDF, TFIDFMatrix)

//Read the matrix created and saved in function
lazy val TFIDFMatrixCV = dataProcess.readMatrixFromFile(fileName.fileMatrixTFIDF)

val crossValidationSet = new ProcessSet(fileName.fileStopWords, fileName.fileCrossValidation)

val cvSetStopWords = dataProcess.takeStopWords(crossValidationSet.stemmedStopWords, crossValidationSet.stemmedSet)
val specificKeywords = dataProcess.applyStemmer(specificWords.commonSpamWords)

val decisionT = DenseVector(cvSetStopWords.map(x => decisionTreeTargetString(x._2, dataProcess.applyStemmer(specificKeywords))).toArray)
dataProcess.evaluationMetrics(crossValidationSet.setVector, decisionT)


//Read list of Words that were achieved in the training set data
val listOfWords = dataProcess.readListFromFile(fileName.fileListOfWords)

//maps each word with the value 0
val mappedLisfOfWords: Map[String, Double] = listOfWords.map(x => x -> 0.0).toMap

//List of words of every sentence without repetitions, without empty strings
val listOfCVSentences = cvSetStopWords.map(x => dataProcess.tokenization(x._2).filterNot(x => x.equals("")))


//Filter, from the cross validation set, the sentences that are not considered in the list of Words
//created from the training set
val listOfCVintersected: List[List[String]] = listOfCVSentences.map(x => x.filter(y => listOfWords.contains(y)))

//Every words is attributed the value of converted sentence into a map where each vector
//maps the proportion of the words presented in a specific sentence (Term Frequency)
val convertedMatrix: DenseMatrix[Double] = dataProcess.convertedMatrixList(listOfCVintersected, mappedLisfOfWords)

val cosineTree = new CosineTree(TFIDFMatrixCV, convertedMatrix, listOfCVintersected, trainingSet)
dataProcess.evaluationMetrics(crossValidationSet.setVector, cosineTree.finalCategorizationC)

val euclideanTree = new EuclideanTree(TFIDFMatrixCV, convertedMatrix, listOfCVintersected, trainingSet)
dataProcess.evaluationMetrics(crossValidationSet.setVector, euclideanTree.finalCategorizationE)


//dataProcess.evaluationMetrics(crossValidationSet.setVector, finalCategorizationE)

val trueCategorization = (cosineTree.finalCategorizationC + euclideanTree.finalCategorizationE + decisionT).map(x =>
  x match {
    case 0 => 0
    case 1 => 0
    case 2 => 1
    case 3 => 1
  }
)

println("FINAL METRICS")
dataProcess.evaluationMetrics(crossValidationSet.setVector, trueCategorization)

//Running time in seconds
val finalTime = System.currentTimeMillis
val timeRunning = (finalTime - inicialTime).toDouble / 1000 + " seconds"