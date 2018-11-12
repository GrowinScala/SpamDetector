object DecisionTree {
/*
  def hasMathSymbols(targetString: String): Int = {

    val mathSymbols = targetString.toLowerCase.replaceAll("[\\+-<>\\/\\^]"," MATH ")
    if(mathSymbols.contains("MATH"))1 else 0
  }

  def hasURLS(targetString: String): Int = {

    val urls = targetString.toLowerCase.replaceAll("(\\S*www\\.\\S*)|(\\S*\\.com\\S*)|(\\S+\\/\\S+\\/\\S+\\/\\S+)"," WEBSITE ")
    if(urls.contains("WEBSITE"))1 else 0
  }

  def hasTripleDots(targetString: String): Int = {

    val tripleDot = targetString.toLowerCase.replaceAll("\\.{3}"," TRIPLEDOT ")
    if(tripleDot.contains("TRIPLEDOT"))0 else 1
  }


  def hasSpecialSymbols(targetString: String): Int = {

    val symbols = targetString.toLowerCase.replaceAll("[$!*#~&]"," SYMBOLS ")
    if(symbols.contains("SYMBOLS"))1 else 0
  }

  def hasEmotion(targetString: String): Int = {
    val emotions = targetString.toLowerCase.replaceAll("(:\\p{Punct}+)|(:\\w\\s+)"," EMOTIONS ")
    if(emotions.contains("EMOTIONS"))0 else 1
  }

  def hasUpper(targetString: String): Int = {

    val upper = targetString.replaceAll("(^(\\p{Upper}+)\\s+)|(\\s+(\\p{Upper}+)$)|(\\s+\\p{Upper}+\\s+)"," UPPER ")
    if(upper.contains("UPPER"))1 else 0
  }
  def hasPhoneNumber(targetString: String): Int = {

    val phoneNumber = targetString.replaceAll("\\d{5,}"," PHONENUMBER ")
    if(phoneNumber.contains("PHONENUMBER"))1 else 0
  }

  def hasNoSpecificWord(targetString : String, specificKeywords : List[String]): Int={

    if(targetString.split(" ").intersect(specificKeywords).length==0) 0 else 1

  }

  def hasSpecificWords(targetString: String, specificKeywords: List[String]): Int = {

    if(targetString.split(" ").intersect(specificKeywords).length==1) 0 else 1
  }
  def has2SpecificWords(targetString: String, specificKeywords: List[String]): Int = {

    if(targetString.split(" ").intersect(specificKeywords).length>=2) 1 else 0
  }
  def has3SpecificWords(targetString: String, specificKeywords: List[String]): Int = {

    if(targetString.split(" ").intersect(specificKeywords).length>=3) 1 else 0
  }

  def hasLessThanThresholdChar(targetString: String, charThreshold: Int): Int = {

    if(targetString.length < charThreshold) 0 else 1
  }

  def hasLessThanThresholdWords(targetString: String, wordThreshold: Int): Int = {

    if(targetString.split(" ").length < wordThreshold) 0 else 1
  }

  def seeMajority(decisions: List[Int]): Int = {

    val lenght = decisions.length
    if((lenght - decisions.count(x=> x.equals(1))) < lenght.toDouble/2) 1 else 0

  }

  def runDecisionTrees(targetString : String, specificKeywords: List[String], charThreshold: Int, wordThreshold: Int): Int = {

    //val decision : List[Int] = hasNoSpecificWord(targetString,specificKeywords) :: Nil
    hasNoSpecificWord(targetString,specificKeywords)
  }*/


  /**
    *
    * @param targetSentence
    * @param specificKeywords
    * @return
    */
  def decisionTreeTargetString(targetSentence : String, specificKeywords: List[String]): Int ={
    val sentence = targetSentence.split(" ")
    if(sentence.intersect(specificKeywords).length==0) 0 else{
      if(sentence.intersect(List("WEBSITE","PHONENUMBER","MONEY")).length>0) if(sentence.intersect(specificKeywords).length>=3)  1 else{
        if(targetSentence.replaceAll("(:\\p{Punct}+)|(:\\w\\s+)"," EMOTIONS ").contains("EMOTIONS"))0 else{
          if(sentence.length<2)0 else{
            if(targetSentence.replaceAll("(^(\\p{Upper}+)\\s+)|(\\s+(\\p{Upper}+)$)|(\\s+\\p{Upper}+\\s+)"," UPPER ").contains("UPPER"))1
            else  0
          }
        }
      } else {
        if(sentence.intersect(specificKeywords).length>=4)1 else{
          if(targetSentence.replaceAll("(^(\\p{Upper}+)\\s+)|(\\s+(\\p{Upper}+)$)|(\\s+\\p{Upper}+\\s+)"," UPPER ").contains("UPPER")
            && (sentence.intersect(specificKeywords).length>=2)) 1
          else
          0
        }
      }
    }
  }

}
