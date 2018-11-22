package DecisionTrees

import DefinedStrings.{ Regex, SpecificWords }
import DefinedValues.ThresholdValues

class DecisionTree(targetSentence: String, specificKeywords: List[String]) {

  val regex = new Regex()
  val specificWords = new SpecificWords()
  val thresholdV = new ThresholdValues()

  /**
   * Function that contains all the nodes of the decision tree
   * @param targetSentence
   * @param specificKeywords
   * @return
   */
  def decisionTreeTargetString: Int = {
    val sentence = targetSentence.split(" ")
    if (sentence.intersect(specificKeywords).length == thresholdV.noIntersection) thresholdV.categorizeHam else {
      if (sentence.intersect(specificWords.decisionTreeIntersect).length > thresholdV.noIntersection) if (sentence.intersect(specificKeywords).length >= thresholdV.intersection3) thresholdV.categorizeSpam else {
        if (targetSentence.replaceAll(regex.regexSmile, " " + specificWords.SMILE + " ").contains(specificWords.SMILE)) thresholdV.categorizeHam else {
          if (sentence.length < thresholdV.intersection2) thresholdV.categorizeHam else {
            if (targetSentence.replaceAll(regex.regexUpper, " " + specificWords.UPPER + " ").contains(specificWords.UPPER)) thresholdV.categorizeSpam
            else thresholdV.categorizeHam
          }
        }
      }
      else {
        if (sentence.intersect(specificKeywords).length >= thresholdV.intersection4) thresholdV.categorizeSpam else {
          if (targetSentence.replaceAll(regex.regexUpper, " " + specificWords.UPPER + " ").contains(specificWords.UPPER)
            && (sentence.intersect(specificKeywords).length >= thresholdV.intersection2)) thresholdV.categorizeSpam
          else thresholdV.categorizeHam
        }
      }
    }
  }

}
