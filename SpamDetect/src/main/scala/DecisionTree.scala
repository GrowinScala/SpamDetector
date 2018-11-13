object DecisionTree{

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
