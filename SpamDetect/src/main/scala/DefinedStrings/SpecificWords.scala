package DefinedStrings

class SpecificWords {

  val WEBSITE = "WEBSITE"
  val SMILE = "SMILE"
  val TRIPLEDOT = "TRIPLEDOT"
  val PHONENUMBER = "PHONENUMBER"
  val PER = "PER"
  val WEEKDAYS = "WEEKDAYS"
  val MONTHS = "MONTHS"
  val MONEY  = "MONEY"
  val DATE = "DATE"
  val NUMBER = "NUMBER"
  val REPETITION = "REPETITION"
  val UPPER = "UPPER"
  val decisionTreeIntersect = List(WEBSITE,PHONENUMBER,MONEY)

  val commonSpamWords = decisionTreeIntersect ++ List(PER, "horny",
    "member","download" ,"mobile", "delivery","delivered","reply", "text",
    "send","sent","ringtone","free", "freemsg", "click", "chat","offer",
    "won","service","lottery","cash","congrats","win","claim","prize",
    "subscribe", "unsubscribe", "order", "call", "dial", "buy","link",
    "sale","store","visit","poly","credit")

}
