package DecisionTrees

import DefinedStrings.SpecificWords
import DefinedValues.ThresholdValues
import ProcessingInformation.{ProcessData, ProcessSet}
import breeze.linalg.{*, DenseMatrix, DenseVector, argmin, min}

class EuclideanTree(TFIDFMatrixCV: DenseMatrix[Double], convertedMatrix: DenseMatrix[Double], listOfCVintersected: List[List[String]], trainingSet: ProcessSet) {

  val dataProcess = new ProcessData()
  val specificWords = new SpecificWords()
  val threshV = new ThresholdValues()

  val specificKeywords = dataProcess.applyStemmer(specificWords.commonSpamWords)

  val distanceMatrix = dataProcess.distanceVector(TFIDFMatrixCV, convertedMatrix)
  val positionsE = distanceMatrix(*, ::).map(row => argmin(row))
  val valuesE: DenseVector[Double] = distanceMatrix(*, ::).map(row => min(row))
  val cvLength = dataProcess.countLength(trainingSet.setStopWords).map(x => x._2)

  val categorizePositionsE = DenseVector((0 until valuesE.length).map(i =>

    if ((valuesE.data(i) > 0.60 && (cvLength.drop(i).head < 8)) || cvLength.drop(i).head < 2) 0
    else trainingSet.setVector.data(positionsE.data(i))).toArray)

  val finalCategorizationE = dataProcess.decisionTree(categorizePositionsE, listOfCVintersected, specificKeywords)


}
