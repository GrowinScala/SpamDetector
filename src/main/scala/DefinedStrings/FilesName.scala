package DefinedStrings
import java.net.URLDecoder
class FilesName {

  val spamDataPath = URLDecoder.decode(getClass.getResource("/spamdata").getPath)
    //.replaceAll("%20", " ")
  val fileCrossValidation = spamDataPath + "/crossvalidation.dat"
  val fileListOfWords = spamDataPath + "/listOfWords.dat"
  val fileMatrixTFIDF = "C:/Users/PedroLuis/Desktop/Scala/SpamDetector/src/main/resources/spamdata/matrixTFIDF.csv"
  val fileAllSet = spamDataPath + "/spam.dat"
  val fileStopWords = spamDataPath + "/stopWords.txt"
  val fileTestSet = spamDataPath + "/testset.dat"
  val fileTrainingSet = spamDataPath + "/trainingset.dat"
  val useless = ""
}
