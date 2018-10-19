import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import ProcessData._

//splitA("src\\main\\resources\\spamdata\\spam.dat")
lazy val trainingSetLoaded = readFromFile("src\\main\\resources\\spamdata\\trainingset.dat")
lazy val trainingSet = parseA("src\\main\\resources\\spamdata\\trainingset.dat",trainingSetLoaded)
lazy val stopWordsList = readFromFile("src\\main\\resources\\spamdata\\stopWords.txt")
//saveToFile("src\\main\\resources\\spamdata\\trainingset.dat", trainingSet)
//val trainingSetcounted = countLength(trainingSet)
val trainingSetLower = uppertoLower(trainingSet)

val trainingSetPonctuation = takePonctuation(trainingSetLower)

val trainingSetStopWords = takeStopWords(stopWordsList, trainingSetPonctuation)

val trainingReplace = replaceOverall(trainingSetStopWords)






