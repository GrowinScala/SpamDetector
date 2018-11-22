package DefinedStrings
import java.net.URLDecoder
class FilesName {

  /**
   * Paths to the data files
   */
  val spamDataPath = URLDecoder.decode(getClass.getResource("/spamdata").getPath)
  val fileCrossValidation = spamDataPath + "/crossvalidation.dat"
  val fileListOfWords = spamDataPath + "/listOfWords.dat"
  val dataSetfileListOfWords = spamDataPath + "/dataSetListOfWords.dat"
  val fileMatrixTFIDF = spamDataPath + "/matrixTFIDF.csv"
  val dataSetfileMatrixTFIDF = spamDataPath + "/dataSetMatrixTFIDF.csv"
  val fileAllSet = spamDataPath + "/spam.dat"
  val fileStopWords = spamDataPath + "/stopWords.txt"
  val fileTestSet = spamDataPath + "/testset.dat"
  val fileTrainingSet = spamDataPath + "/trainingset.dat"
}
