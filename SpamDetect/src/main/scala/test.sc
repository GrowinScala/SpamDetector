import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import ProcessData._

//splitA("src\\main\\resources\\spamdata\\spam.dat")
lazy val trainingSetLoaded = readFromFile("src\\main\\re" +
  "sources\\spamdata\\trainingset.dat")
lazy val trainingSet = parseA("src\\main\\resources\\spamdata\\trainingset.dat",trainingSetLoaded)
lazy val stopWordsList = readFromFile("src\\main\\resources\\spamdata\\stopWords.txt")
lazy val stemmedStopWords = applyStemmer(stopWordsList).distinct
//saveToFile("src\\main\\resources\\spamdata\\trainingset.dat", trainingSet)
//val trainingSetcounted = countLength(trainingSet)
//val testing = List((1,"knowledge Message:some text :( mis:/sing* Sender:Name Missing*/ *Number Missing *Sent:Date missing *Missing U a lot thats y everything is missing sent via fullonsms.com"))
val trainingSetLower = uppertoLower(trainingSet)
val trainingReplace = replaceOverall(trainingSetLower)
val trainingSetPonctuation = takePunctuation(trainingReplace)
val trainingStemmed = trainingSetPonctuation.map(x=> x._1).zip(applyStemmer(trainingSetPonctuation.map(x=> x._2)))
val trainingSetStopWords = takeStopWords(stemmedStopWords, trainingStemmed)

val matrix = makeTFMatrix(trainingSetStopWords)


saveToFile("src\\main\\resources\\spamdata\\output.dat", trainingSetStopWords)



//replaceOverall(List((1,"Message:some text missing*  Missing* *Number Missing *Sent:date missing *Missing U a lot thats y everything is missing sent via fullonsms.com")))