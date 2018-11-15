package ProcessingInformation

import DefinedStrings.FilesName
import breeze.linalg.DenseVector

class ProcessSet(stopWordsFileName: String, targetSetFileName: String) {

  val fileName = new FilesName
  val dataProcess = new ProcessData

  /**
    * Import list of Stop Words provided previously
    */
  val stopWordsList = dataProcess.readListFromFile(stopWordsFileName)
  /**
    * Apply stemmer to the list of stop Words and take the common words
    */
  val stemmedStopWords = dataProcess.applyStemmer(stopWordsList).distinct
  /**
    * Apply parse function to every string of the targetSetFile name
    * Example:
    * Input: (ham,"Why are u up so early?")
    * Output: (0, "Why are u up so early?")
    */
  lazy val setLoaded = dataProcess.readListFromFile(targetSetFileName)
  lazy val setParsed = dataProcess.parseA(setLoaded)
  /**
    * Selects the value 0 or 1 (ham or spam) from every string and creates a vector
    */
  val setVector = DenseVector(setParsed.map(x => x._1).toArray)
  /**
    * Apply several changes to the messages considered:
    * - Every upper letters to lower
    * - Generalize some words of strings (p.e. all phone numbers are replaced by: "PHONENUMBER")
    * - Remove unnecessary punctuation characters
    * - Remove some worthless words (articles, determinants, pronouns, ...
    * )
    */
  val setLower = dataProcess.uppertoLower(setParsed)
  val replacedSet = dataProcess.replaceOverall(setLower)
  val setPonctuation = dataProcess.takePunctuation(replacedSet)
  val stemmedSet = setPonctuation.map(x => x._1)
    .zip(dataProcess.applyStemmer(setPonctuation.map(x => x._2)))
  val setStopWords = dataProcess.takeStopWords(stemmedStopWords, stemmedSet)
  /**
    * Counts the number of words of every message, after all the changes creating a List of integers
    */
  val setLength = dataProcess.countLength(setStopWords).map(x => x._2)
}
