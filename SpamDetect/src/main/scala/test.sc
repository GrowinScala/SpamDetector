
import breeze.linalg._
//import ProcessData._
import DecisionTree._

val inicialTime: Long = System.currentTimeMillis

val fileName = new FilesName()
val dataProcess = new ProcessData()

val trainingSet = new ProcessSet(fileName.fileStopWords, fileName.fileTrainingSet)

val TFmatrix = dataProcess.makeTFMatrix(trainingSet.setStopWords)
//Creates TFIDF matrix through TF matrix
val TFIDFMatrix = dataProcess.makeTFIDFMatrix(TFmatrix)
//Saves in csv file the matrix TFIDF
dataProcess.saveToFile(fileName.fileMatrixTFIDF , TFIDFMatrix)

//Read the matrix created and saved in function
 lazy val TFIDFMatrixCV = dataProcess.readMatrixFromFile(fileName.fileMatrixTFIDF )

  val crossValidationSet = new ProcessSet(fileName.fileStopWords,fileName.fileCrossValidation)

  val cvSetStopWords = dataProcess.takeStopWords(crossValidationSet.stemmedStopWords, crossValidationSet.stemmedSet)
  val specificKeywords = dataProcess.applyStemmer(List("WEBSITE", "horny", "member","download" ,"mobile", "delivery","delivered","PHONENUMBER" ,"MONEY","PER", "reply", "text", "send","sent","ringtone","free", "freemsg", "click","chat","offer", "won","service","lottery","cash","congrats","win","claim","prize","subscribe", "unsubscribe", "order", "call", "dial", "buy","link","sale","store","visit","poly","credit"))

  val decisionT = DenseVector(cvSetStopWords.map(x=> decisionTreeTargetString(x._2,dataProcess.applyStemmer(specificKeywords))).toArray)
  dataProcess.evaluationMetrics(crossValidationSet.setVector, decisionT)


  //Read list of Words that were achieved in the training set data
  val listOfWords = dataProcess.readListFromFile(fileName.fileListOfWords)

  //maps each word with the value 0
  val mappedLisfOfWords : Map[String,Double]= listOfWords.map(x=> x->0.0).toMap

  //List of words of every sentence without repetitions, without empty strings
  val listOfCVSentences = cvSetStopWords.map(x=> dataProcess.tokenization(x._2).filterNot(x=> x.equals("")))


  //Filter, from the cross validation set, the sentences that are not considered in the list of Words
  //created from the training set
  val listOfCVintersected: List[List[String]] = listOfCVSentences.map(x=> x.filter(y => listOfWords.contains(y)))

  //Every words is attributed the value of converted sentence into a map where each vector
  //maps the proportion of the words presented in a specific sentence (Term Frequency)
  val convertedMatrix: DenseMatrix[Double] = dataProcess.convertedMatrixList(listOfCVintersected, mappedLisfOfWords)

  //This function will calculate the cosine similarity between the convertedMatrix and TFIDF Matrix
  //It will return a matrix where each row represents the different values between a string j of cross validation and the
  //various strings of the training set
  val cosineMatrix = dataProcess.cosineVector(TFIDFMatrixCV, convertedMatrix)

  //For every vector of the cosineVector list, it is calculated the position of the maximum value.
  //This position corresponds to the most similar string of training data with the string of CV data considered
  println("COSINE SIMILARITY")
  val positionsC :DenseVector[Int] = cosineMatrix(*, ::).map(row => argmax(row))
  val valuesC :DenseVector[Double] = cosineMatrix(*, ::).map(row => max(row))
  val cvLength = dataProcess.countLength(cvSetStopWords).map(x=> x._2)
  val categorizePositionsC = positionsC.map(x => trainingSet.setVector(x))

  val finalCategorizationC =  DenseVector((0 until valuesC.length).map(i=>

  if(categorizePositionsC.data(i) == 0 && valuesC.data(i) < 0.45 && dataProcess.containsMoreString(3, listOfCVintersected.drop(i).head, specificKeywords)) 1
  else if(categorizePositionsC.data(i) == 0 && valuesC.data(i) < 0.70 && dataProcess.containsMoreString(4, listOfCVintersected.drop(i).head, specificKeywords)) 1
  else if (categorizePositionsC.data(i) == 1 && valuesC.data(i) < 0.65 && dataProcess.containsLessString(0, listOfCVintersected.drop(i).head, specificKeywords)) 0
  else categorizePositionsC.data(i)).toArray)


/*
  val categorizePositions =  DenseVector((0 until valuesC.length).map(i=>

  if((valuesC.data(i) < 0.40 && ( cvLength.drop(i).head < 8) ) || cvLength.drop(i).head <2 ) 0
  else trainingSetVector.data(positionsC.data(i))).toArray)
 // val specificKeywords = List("WEBSITE","delivery","delivered","PHONENUMBER" ,"MONEY","PER", "reply", "text", "send","sent","ringtone","free", "freemsg", "click","chat","offer", "won","service","lottery","cash","congrats","win","claim","prize","subscribe", "unsubscribe", "order", "call", "dial", "buy","link","sale","store","visit")

  val finalCategorizationC = decisionTree(categorizePositions,listOfCVintersected,specificKeywords)
*/

dataProcess.evaluationMetrics(crossValidationSet.setVector, finalCategorizationC)


  println("EUCLIDEAN SIMILARITY")
  val distanceMatrix = dataProcess.distanceVector(TFIDFMatrixCV, convertedMatrix)
  val positionsE = distanceMatrix(*, ::).map(row => argmin(row))
  val valuesE :DenseVector[Double] = distanceMatrix(*, ::).map(row => min(row))

  val categorizePositionsE =  DenseVector((0 until valuesE.length).map(i=>

  if((valuesE.data(i) > 0.60 && ( cvLength.drop(i).head < 8) ) || cvLength.drop(i).head <2 ) 0
  else trainingSet.setVector.data(positionsE.data(i))).toArray)

  val finalCategorizationE = dataProcess.decisionTree(categorizePositionsE,listOfCVintersected,specificKeywords)
  //listOfCVintersected
  // == 0 && trainingSetStopWords.drop(x).head._2.contains("WEBSITE"))
  dataProcess.evaluationMetrics(crossValidationSet.setVector, finalCategorizationE)

/*
println("EUCLIDEAN SIMILARITY")
val distanceMatrix = distanceVector(TFIDFMatrixCV, convertedMatrix)
val positionsE = distanceMatrix(*, ::).map(row => argmin(row))
val valuesE :DenseVector[Double] = distanceMatrix(*, ::).map(row => min(row))
val categorizePositionsE = positionsE.map(x => trainingSetVector(x))

val finalCategorizationE = DenseVector((0 until positionsE.length).map(i=>
  if(categorizePositionsE.data(i) == 0 && valuesE.data(i)>3 && containsMoreString(3, listOfCVintersected.drop(i).head, specificKeywords)) 1
  else if (categorizePositionsE.data(i) == 1 && valuesE.data(i)>3 && containsLessString(0, listOfCVintersected.drop(i).head, specificKeywords)) 0
  else categorizePositionsE.data(i)).toArray)

*/
   dataProcess.evaluationMetrics(crossValidationSet.setVector, finalCategorizationE)

  val superFinalCategorization = (finalCategorizationC + finalCategorizationE + decisionT ).map(x=>
    x match {
      case 0 => 0
      case 1 => 0
      case 2 => 1
      case 3 => 1
    }
  )

 println("FINAL METRICS")
 dataProcess.evaluationMetrics(crossValidationSet.setVector,superFinalCategorization)




  //Running time in seconds
  val finalTime = System.currentTimeMillis
  val timeRunning = (finalTime - inicialTime).toDouble/1000 + " seconds"