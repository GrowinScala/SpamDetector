package DecisionTrees

import DefinedStrings.SpecificWords
import DefinedValues.ThresholdValues
import ProcessingInformation.{ProcessData, ProcessSet}
import breeze.linalg.{*, DenseMatrix, DenseVector, argmax, max}
import scalariform.formatter.preferences._

class CosineTree(TFIDFMatrixCV: DenseMatrix[Double], convertedMatrix: DenseMatrix[Double], listOfCVintersected: List[List[String]], trainingSet: ProcessSet) {

  val dataProcess = new ProcessData()
         val specificWords = new SpecificWords()
  val threshV = new ThresholdValues()

  val specificKeywords = dataProcess.applyStemmer(specificWords.commonSpamWords)

  //For every vector of the cosineVector list, it is calculated the position of the maximum value.
  //This position corresponds to the most similar string of training data with the string of CV data considered


  //This function will calculate the cosine similarity between the convertedMatrix and TFIDF Matrix
  //It will return a matrix where each row represents the different values between a string j of cross validation and the
  //various strings of the training set
  val cosineMatrix = dataProcess.cosineVector(TFIDFMatrixCV, convertedMatrix)
  val positionsC: DenseVector[Int] = cosineMatrix(*, ::).map(row => argmax(row))
  val valuesC: DenseVector[Double] = cosineMatrix(*, ::).map(row => max(row))
  val cvLength = dataProcess.countLength(trainingSet.setStopWords).map(x => x._2)
  val categorizePositionsC = positionsC.map(x => trainingSet.setVector(x))

  val finalCategorizationC = DenseVector((0 until valuesC.length).map(i =>

    if (categorizePositionsC.data(i) == 0 && valuesC.data(i) < 0.45 && dataProcess.containsMoreString(3, listOfCVintersected.drop(i).head, specificKeywords)) 1
    else if (categorizePositionsC.data(i) == 0 && valuesC.data(i) < 0.70 && dataProcess.containsMoreString(4, listOfCVintersected.drop(i).head, specificKeywords)) 1
    else if (categorizePositionsC.data(i) == 1 && valuesC.data(i) < 0.65 && dataProcess.containsLessString(0, listOfCVintersected.drop(i).head, specificKeywords)) 0
    else categorizePositionsC.data(i)).toArray)


}
