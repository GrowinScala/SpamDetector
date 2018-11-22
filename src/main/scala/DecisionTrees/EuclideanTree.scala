package DecisionTrees

import DefinedStrings.SpecificWords
import DefinedValues.ThresholdValues
import ProcessingInformation.{ ProcessData, ProcessSet }
import breeze.linalg.{ *, DenseMatrix, DenseVector, argmin, min }

class EuclideanTree(TFIDFMatrixCV: DenseMatrix[Double], convertedMatrix: DenseMatrix[Double], listOfCVintersected: List[List[String]], set: ProcessSet) {

  val dataProcess = new ProcessData()
  val specificWords = new SpecificWords()
  val threshV = new ThresholdValues()

  /**
    * Apply stemmer to the specific spam words considered
    */
  val specificKeywords = specificWords.commonSpamWords.map(dataProcess.applyStemmer(_))


  /**
    * Matrix that calculates the euclidean distances between the vectors (collumns) of the TFIDF matrix and the
    * vectors obtained with the messages that are being classified. The minimum value for each message is considered and
    * the classification attributed
    */
  val distanceMatrix = dataProcess.distanceVector(TFIDFMatrixCV, convertedMatrix)
  val positionsE = distanceMatrix(*, ::).map(row => argmin(row))
  val valuesE: DenseVector[Double] = distanceMatrix(*, ::).map(row => min(row))

  /**
    * Creates a list of integers with the length of each message after being processed
    */
  val cvLength = set.setProcessString.map(x=>dataProcess.countLength(x._2))

  /**
    * Tree node that takes into account the euclidean value and the length of each message
    * in order to reclassify it or not
    */
  val categorizePositionsE = DenseVector((0 until valuesE.length).map(i =>
    if ((valuesE.data(i) > 0.60 && (cvLength.drop(i).head < 8)) || cvLength.drop(i).head < 2) 0
    else set.setVector.data(positionsE.data(i))).toArray)

  /**
    * Function that categorizate the final values (0 or 1) for each string/message
    */
  val finalCategorizationE = dataProcess.decisionTree(categorizePositionsE, listOfCVintersected, specificKeywords)
}
