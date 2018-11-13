package DecisionTrees

import ProcessingInformation.ProcessData
import breeze.linalg.{*, DenseVector, argmax, max}

object CosineTree {

  val dataProcess = new ProcessData()

  //This function will calculate the cosine similarity between the convertedMatrix and TFIDF Matrix
  //It will return a matrix where each row represents the different values between a string j of cross validation and the
  //various strings of the training set
  val cosineMatrix = dataProcess.cosineVector(TFIDFMatrixCV, convertedMatrix)
  val positionsC :DenseVector[Int] = cosineMatrix(*, ::).map(row => argmax(row))
  val valuesC :DenseVector[Double] = cosineMatrix(*, ::).map(row => max(row))
  val cvLength = dataProcess.countLength(cvSetStopWords).map(x=> x._2)
  val categorizePositionsC = positionsC.map(x => trainingSet.setVector(x))

  val finalCategorizationC =  DenseVector((0 until valuesC.length).map(i=>

    if(categorizePositionsC.data(i) == 0 && valuesC.data(i) < 0.45 && dataProcess.containsMoreString(3, listOfCVintersected.drop(i).head, specificKeywords)) 1
    else if(categorizePositionsC.data(i) == 0 && valuesC.data(i) < 0.70 && dataProcess.containsMoreString(4, listOfCVintersected.drop(i).head, specificKeywords)) 1
    else if (categorizePositionsC.data(i) == 1 && valuesC.data(i) < 0.65 && dataProcess.containsLessString(0, listOfCVintersected.drop(i).head, specificKeywords)) 0
    else categorizePositionsC.data(i)).toArray)

}
