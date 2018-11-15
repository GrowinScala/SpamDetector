package DefinedStrings

class Regex {

  /**
    * Regex functions to identify some characteristics components of a message
    */
  val regexPunctuation = "\\p{Punct}"
  val regexWebsite = "(\\S*www\\.\\S*)|(\\S*\\.com\\S*)|(\\S+\\/\\S+\\/\\S+\\/\\S+)"
  val regexSmile = "(:" + regexPunctuation + "+)|(:\\w\\s+)"
  val regexTripleDot = "\\.{3}"
  val regexPhoneNumber = "\\d{5,}|(\\d\\s*){11}"
  val regexPer = "\\w{1,4}\\/\\w{1,4}"

  val regexWeekDays = "(" + regexPunctuation + "+|\\W)((mon)|(monday)|(tue)|(tuesday)|(wed)" +
    "|(wednesday)|(thu)|(thursday)|(friday)|(saturday)|(sunday))(" + regexPunctuation + "+|\\W)"

  val regexMonth = "(" + regexPunctuation + "+|\\W)((jan)|(january)|(feb)|(february)|(mar)|(march)" +
    "|(apr)|(april)|(may)|(jun)|(june)|(jul)|(july)|(aug)|(august)|(sep)|(september)|(oct)|" +
    "(october)|(nov)|(november)|(dec)|(december))(" + regexPunctuation + "+|\\W)"

  val regexMoney = "(\\d+\\W*pence\\w*)|(\\d+\\W*pound\\w*)|(\\d+\\W*dollar\\w*)|(\\d+\\W*cash\\w*)" +
    "|(\\d+\\W*euro\\w*)|(\\d+\\W*p(\\s|$)|(\\d+pp))|(\\?\\d+|[\\.,\\,]\\d+)"

  val regexDate = "(?:(?:31(\\/|-|\\.)(?:0?[13578]|1[02]))\\1|(?:(?:29|30)(\\/|-|\\.)" +
    "(?:0?[1,3-9]|1[0-2])\\2))(?:(?:1[6-9]|[2-9]\\d)?\\d{2})|(?:29(\\/|-|\\.)0?2\\3(?:(?:" +
    "(?:1[6-9]|[2-9]\\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))" +
    "|(?:0?[1-9]|1\\d|2[0-8])(\\/|-|\\.)(?:(?:0?[1-9])|(?:1[0-2]))\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})"

  val regexNumber = "\\d{1,4}"
  val regexRepetition = "([a-z])\\1{2,}"
  val regexUpper = "(^(\\p{Upper}+)\\s+)|(\\s+(\\p{Upper}+)$)|(\\s+\\p{Upper}+\\s+)"

}
