package DecisionTrees

import DefinedStrings.SpecificWords
import DefinedValues.ThresholdValues
import ProcessingInformation.{ ProcessData, ProcessSet }
import breeze.linalg.{ *, DenseMatrix, DenseVector, argmax, max }

class CosineTree(TFIDFMatrixCV: DenseMatrix[Double], convertedMatrix: DenseMatrix[Double], listOfCVintersected: List[List[String]], trainingSet: ProcessSet) {

  val dataProcess = new ProcessData()
  val specificWords = new SpecificWords()
  val threshV = new ThresholdValues()

  val specificKeywords = dataProcess.applyStemmer(specificWords.commonSpamWords)

  /**
    * This function will calculate the cosine similarity between the convertedMatrix and TFIDF Matrix
    * It will return a matrix where each row represents the different values between a string j of cross
    * validation and the various strings of the training set
    */
  val cosineMatrix = dataProcess.cosineVector(TFIDFMatrixCV, convertedMatrix)

  /**
    * For every vector of the cosineVector list, it is calculated the position of the maximum value.
    * This position corresponds to the most similar string of training data with the string of CV data considered
    */
  val positionsC: DenseVector[Int] = cosineMatrix(*, ::).map(row => argmax(row))
  val valuesC: DenseVector[Double] = cosineMatrix(*, ::).map(row => max(row))
  val cvLength = dataProcess.countLength(trainingSet.setStopWords).map(x => x._2)
  val categorizePositionsC = positionsC.map(x => trainingSet.setVector(x))

  /**
    * Tree node that takes into account the cosine value and the number of specific words that a message contains,
    * in order to reclassify it or not
    */
  val finalCategorizationC = DenseVector((0 until valuesC.length).map(i =>
    if (categorizePositionsC.data(i) == threshV.categorizeHam && valuesC.data(i) < threshV.cosine045 && dataProcess.containsMoreString(threshV.string3, listOfCVintersected.drop(i).head, specificKeywords)) threshV.categorizeSpam
    else if (categorizePositionsC.data(i) == threshV.categorizeHam && valuesC.data(i) < threshV.cosine070 && dataProcess.containsMoreString(threshV.string4, listOfCVintersected.drop(i).head, specificKeywords)) threshV.categorizeSpam
    else if (categorizePositionsC.data(i) == threshV.categorizeSpam && valuesC.data(i) < threshV.cosine065 && dataProcess.containsLessString(threshV.string0, listOfCVintersected.drop(i).head, specificKeywords)) threshV.categorizeHam
    else categorizePositionsC.data(i)).toArray)
}
