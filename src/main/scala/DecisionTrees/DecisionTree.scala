package DecisionTrees

import DefinedStrings.{Regex, SpecificWords}
import DefinedValues.ThresholdValues

object DecisionTree {

  val regex = new Regex()
  val specificWords = new SpecificWords()
  val thresholdV = new ThresholdValues()

  /**
    *
    * @param targetSentence
    * @param specificKeywords
    * @return
    */
  def decisionTreeTargetString(targetSentence: String, specificKeywords: List[String]): Int = {
    val sentence = targetSentence.split(" ")
    if (sentence.intersect(specificKeywords).length == thresholdV.noIntersection) thresholdV.returnHam else {
      if (sentence.intersect(specificWords.decisionTreeIntersect).length > thresholdV.noIntersection) if (sentence.intersect(specificKeywords).length >= thresholdV.intersection3) thresholdV.returnSpam else {
        if (targetSentence.replaceAll(regex.regexSmile, " " + specificWords.SMILE + " ").contains(specificWords.SMILE)) thresholdV.returnHam else {
          if (sentence.length < thresholdV.intersection2) thresholdV.returnHam else {
            if (targetSentence.replaceAll(regex.regexUpper, " " + specificWords.UPPER + " ").contains(specificWords.UPPER)) thresholdV.returnSpam
            else thresholdV.returnHam
          }
        }
      } else {
        if (sentence.intersect(specificKeywords).length >= thresholdV.intersection4) thresholdV.returnSpam else {
          if (targetSentence.replaceAll(regex.regexUpper, " " + specificWords.UPPER + " ").contains(specificWords.UPPER)
            && (sentence.intersect(specificKeywords).length >= thresholdV.intersection2)) thresholdV.returnSpam
          else thresholdV.returnHam
        }
      }
    }
  }

}
