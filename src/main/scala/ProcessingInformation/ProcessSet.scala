package ProcessingInformation

import DefinedStrings.FilesName
import breeze.linalg.DenseVector

class ProcessSet(stopWordsFileName: String, targetSetFileName: String) {

  val fileName = new FilesName
  val dataProcess = new ProcessData

  //import list of Stop Words provided early
  val stopWordsList = dataProcess.readListFromFile(stopWordsFileName)
  //apply stemmer to the list of stop Words and take the common words
  val stemmedStopWords = dataProcess.applyStemmer(stopWordsList).distinct

  //splitA("src\\main\\resources\\spamdata\\spam.dat")
  lazy val setLoaded = dataProcess.readListFromFile(targetSetFileName)
  lazy val setParsed = dataProcess.parseA(setLoaded)
  val setVector = DenseVector(setParsed.map(x => x._1).toArray)

  //saveToFile("src\\main\\resources\\spamdata\\trainingset.dat", trainingSet)

  val setLower = dataProcess.uppertoLower(setParsed)
  val replacedSet = dataProcess.replaceOverall(setLower)
  val setPonctuation = dataProcess.takePunctuation(replacedSet)
  val stemmedSet = setPonctuation.map(x => x._1)
    .zip(dataProcess.applyStemmer(setPonctuation.map(x => x._2)))
  val setStopWords = dataProcess.takeStopWords(stemmedStopWords, stemmedSet)
  val setLength = dataProcess.countLength(setStopWords).map(x => x._2)
  //

}
