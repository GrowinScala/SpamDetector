import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import ProcessData._
/*
//splitA("src\\main\\resources\\spamdata\\spam.dat")
lazy val trainingSetLoaded = readListFromFile("src\\main\\re" +
  "sources\\spamdata\\trainingset.dat")
lazy val trainingSet = parseA("src\\main\\resources\\spamdata\\trainingset.dat",trainingSetLoaded)
lazy val stopWordsList = readListFromFile("src\\main\\resources\\spamdata\\stopWords.txt")
val stemmedStopWords = applyStemmer(stopWordsList).distinct
//saveToFile("src\\main\\resources\\spamdata\\trainingset.dat", trainingSet)
val trainingSetLower = uppertoLower(trainingSet)
val trainingReplace = replaceOverall(trainingSetLower)
val trainingSetPonctuation = takePunctuation(trainingReplace)
val trainingStemmed = trainingSetPonctuation.map(x=> x._1).zip(applyStemmer(trainingSetPonctuation.map(x=> x._2)))
val trainingSetStopWords = takeStopWords(stemmedStopWords, trainingStemmed)
//val matrix = makeTFMatrix(trainingSetStopWords)
//saveToFile("src\\main\\resources\\spamdata\\matrixTF.csv", matrix)
*/
val TFMatrix = readMatrixFromFile("src\\main\\resources\\spamdata\\matrixTF.csv")

//val TFMatrixCols = TFMatrix.cols
/*val TFIDFMatrix = TFMatrix(*,::).map(row=> {
  val countRow = row.foldLeft(0.0)((count, element) => count + (if (element!=0) 1.0 else 0.0))
  row.map(x=> if(x!= 0.0) x* log1p(TFMatrixCols.toDouble/countRow) else x)
})*/
val TFIDFMatrix = makeTFIDFMatrix(TFMatrix)



//replaceOverall(List((1,"Message:some text missing*  Missing* *Number Missing *Sent:date missing *Missing U a lot thats y everything is missing sent via fullonsms.com")))