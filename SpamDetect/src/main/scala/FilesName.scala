class FilesName {

  val spamDataPath =  getClass.getResource("/spamdata").getPath.replaceAll("%20", " ")
  val fileCrossValidation = spamDataPath +"/crossvalidation.dat"
  val fileListOfWords = spamDataPath + "/listOfWords.dat"
  val fileMatrixTFIDF = spamDataPath + "/matrixTFIDF.csv"
  val fileAllSet = spamDataPath + "/spam.dat"
  val fileStopWords = spamDataPath + "/stopWords.txt"
  val fileTestSet = spamDataPath + "/testset.dat"
  val fileTrainingSet = spamDataPath + "/trainingset.dat"

}
