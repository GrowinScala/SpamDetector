package DefinedStrings
import java.net.URLDecoder
class FilesName {

  val spamDataPath = URLDecoder.decode(getClass.getResource("/spamdata").getPath)
  val fileCrossValidation = spamDataPath + "/crossvalidation.dat"
  val fileListOfWords = spamDataPath + "/listOfWords.dat"
  val fileMatrixTFIDF = spamDataPath + "/matrixTFIDF.csv"
  val fileAllSet = spamDataPath + "/spam.dat"
  val fileStopWords = spamDataPath + "/stopWords.txt"
  val fileTestSet = spamDataPath + "/testset.dat"
  val fileTrainingSet = spamDataPath + "/trainingset.dat"
}
