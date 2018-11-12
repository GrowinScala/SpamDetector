
import breeze.linalg._
import ProcessData._
import DecisionTree._

val inicialTime: Long = System.currentTimeMillis

val spamDataPath =  getClass.getResource("/spamdata").getPath.replaceAll("%20", " ")

//import list of Stop Words provided early
lazy val stopWordsList = readListFromFile(spamDataPath + "/stopWords.txt")
//apply stemmer to the list of stop Words and take the common words
val stemmedStopWords = applyStemmer(stopWordsList).distinct


  //splitA("src\\main\\resources\\spamdata\\spam.dat")
  lazy val trainingSetLoaded = readListFromFile(spamDataPath + "/trainingset.dat")
  lazy val trainingSet = parseA(trainingSetLoaded)
  val trainingSetVector =  DenseVector(trainingSet.map(x => x._1).toArray)
/*
  //saveToFile("src\\main\\resources\\spamdata\\trainingset.dat", trainingSet)

  val trainingSetLower = uppertoLower(trainingSet)
  val trainingReplace = replaceOverall(trainingSetLower)
  val trainingSetPonctuation = takePunctuation(trainingReplace)
  val trainingStemmed = trainingSetPonctuation.map(x=> x._1)
                        .zip(applyStemmer(trainingSetPonctuation.map(x=> x._2)))
  val trainingSetStopWords = takeStopWords(stemmedStopWords, trainingStemmed)
  val trainingSetLength = countLength(trainingSetStopWords).map(x=> x._2)
  //
  val TFmatrix = makeTFMatrix(trainingSetStopWords)
  //Creates TFIDF matrix through TF matrix
  val TFIDFMatrix = makeTFIDFMatrix(TFmatrix)
  //Saves in csv file the matrix TFIDF
  saveToFile(spamDataPath +"/matrixTFIDF.csv", TFIDFMatrix)
*/

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////CROSS-VALIDATION///////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Read the matrix created and saved in function
 lazy val TFIDFMatrixCV = readMatrixFromFile(spamDataPath +"/matrixTFIDF.csv")

// trial of a list the would be the cross-validation Set
/*
val cvSet = List(
    (0,"It doesnt make sense to take it there unless its free. If you need to know more, wikipedia.com")
)
*/

  val cvList = readListFromFile(spamDataPath + "/crossvalidation.dat")
  val cvSet = parseA(cvList)

/*
  val cvSet = List(
    "ham,Ok. I only ask abt e movie. U wan ktv oso?",
      "ham,Today iZ Yellow rose day. If u love my frndship give me 1 misscall &amp; send this to ur frndZ &amp; See how many miss calls u get. If u get 6missed U marry ur Lover.",
      "ham,Are you sure you don't mean get here",
      "spam,Our dating service has been asked 2 contact U by someone shy! CALL 09058091870 NOW all will be revealed. POBox84, M26 3UZ 150p",
      "ham,Shuhui has bought ron's present it's a swatch watch...",
      "ham,I'm reading the text i just sent you. Its meant to be a joke. So read it in that light",
      "spam,Hi 07734396839 IBH Customer Loyalty Offer: The NEW NOKIA6600 Mobile from ONLY ?10 at TXTAUCTION!Txt word:START to No:81151 & get Yours Now!4T&",
      "ham,Do u noe how 2 send files between 2 computers?",
      "ham,Don't make life too stressfull.. Always find time to Laugh.. It may not add years to your Life! But surely adds more life to ur years!! Gud ni8..swt dreams..",
      "spam,Ringtone Club: Gr8 new polys direct to your mobile every week !",
      "spam,I don't know u and u don't know me. Send CHAT to 86688 now and let's find each other! Only 150p/Msg rcvd. HG/Suite342/2Lands/Row/W1J6HL LDN. 18 years or over.",
      "spam,XMAS iscoming & ur awarded either ?500 CD gift vouchers & free entry 2 r ?100 weekly draw txt MUSIC to 87066 TnC www.Ldew.com1win150ppmx3age16subscription ",
      "ham,Thx. All will be well in a few months",
      "ham,Hey. What happened? U switch off ur cell d whole day. This isnt good. Now if u do care, give me a call tomorrow.",
      "ham,WHAT TIME U WRKIN?")
   */
  //Turn the characters to lower case
  val cvSetLower = cvSet
  //uppertoLower(cvSet.flatMap(parse(_)))

  //Generalize data into a specific group of words
  val cvReplace = replaceOverall(cvSetLower)

  //Remove all punctuation
  val cvSetPonctuation = takePunctuation(cvReplace)

  //Creates an ordered list of the categories (spam->1 or ham ->0)(int) for each string
  val cvCategories = cvSetPonctuation.map(x=> x._1)
  val cvCategoriesVector = DenseVector(cvCategories.toArray)
  //Apply stemmer to the list of sentences (string) and merge it with the correspondent category (ham -> 0 or spam ->1)
  val cvStemmed = cvCategories.zip(applyStemmer(cvSetPonctuation.map(x=> x._2)))

  //Remove stopwords from training set
  val cvSetStopWords = takeStopWords(stemmedStopWords, cvStemmed)
  val specificKeywords = applyStemmer(List("WEBSITE", "horny", "member","download" ,"mobile", "delivery","delivered","PHONENUMBER" ,"MONEY","PER", "reply", "text", "send","sent","ringtone","free", "freemsg", "click","chat","offer", "won","service","lottery","cash","congrats","win","claim","prize","subscribe", "unsubscribe", "order", "call", "dial", "buy","link","sale","store","visit","poly","credit"))

  /*val treeCategorization = cvSetStopWords.map(x=> runDecisionTrees(x._2,specificKeywords,48,2))
  evaluationMetrics(cvCategoriesVector, DenseVector(treeCategorization.toArray))
  */
  val decisionT = DenseVector(cvSetStopWords.map(x=> decisionTreeTargetString(x._2,applyStemmer(specificKeywords))).toArray)
  evaluationMetrics(cvCategoriesVector, decisionT)


  //Read list of Words that were achieved in the training set data
  val listOfWords = readListFromFile(spamDataPath + "/listOfWords.dat")

  //maps each word with the value 0
  val mappedLisfOfWords : Map[String,Double]= listOfWords.map(x=> x->0.0).toMap

  //List of words of every sentence without repetitions, without empty strings
  val listOfCVSentences = cvSetStopWords.map(x=> tokenization(x._2).filterNot(x=> x.equals("")))


  //Filter, from the cross validation set, the sentences that are not considered in the list of Words
  //created from the training set
  val listOfCVintersected: List[List[String]] = listOfCVSentences.map(x=> x.filter(y => listOfWords.contains(y)))

  //Every words is attributed the value of converted sentence into a map where each vector
  //maps the proportion of the words presented in a specific sentence (Term Frequency)
  val convertedMatrix: DenseMatrix[Double] = convertedMatrixList(listOfCVintersected, mappedLisfOfWords)

  //This function will calculate the cosine similarity between the convertedMatrix and TFIDF Matrix
  //It will return a matrix where each row represents the different values between a string j of cross validation and the
  //various strings of the training set
  val cosineMatrix = cosineVector(TFIDFMatrixCV, convertedMatrix)

  //For every vector of the cosineVector list, it is calculated the position of the maximum value.
  //This position corresponds to the most similar string of training data with the string of CV data considered
  println("COSINE SIMILARITY")
  val positionsC :DenseVector[Int] = cosineMatrix(*, ::).map(row => argmax(row))
  val valuesC :DenseVector[Double] = cosineMatrix(*, ::).map(row => max(row))
  val cvLength = countLength(cvSetStopWords).map(x=> x._2)
  val categorizePositionsC = positionsC.map(x => trainingSetVector(x))

  val finalCategorizationC =  DenseVector((0 until valuesC.length).map(i=>

  if(categorizePositionsC.data(i) == 0 && valuesC.data(i) < 0.45 && containsMoreString(3, listOfCVintersected.drop(i).head, specificKeywords)) 1
  else if(categorizePositionsC.data(i) == 0 && valuesC.data(i) < 0.70 && containsMoreString(4, listOfCVintersected.drop(i).head, specificKeywords)) 1
  else if (categorizePositionsC.data(i) == 1 && valuesC.data(i) < 0.65 && containsLessString(0, listOfCVintersected.drop(i).head, specificKeywords)) 0
  else categorizePositionsC.data(i)).toArray)


/*
  val categorizePositions =  DenseVector((0 until valuesC.length).map(i=>

  if((valuesC.data(i) < 0.40 && ( cvLength.drop(i).head < 8) ) || cvLength.drop(i).head <2 ) 0
  else trainingSetVector.data(positionsC.data(i))).toArray)
 // val specificKeywords = List("WEBSITE","delivery","delivered","PHONENUMBER" ,"MONEY","PER", "reply", "text", "send","sent","ringtone","free", "freemsg", "click","chat","offer", "won","service","lottery","cash","congrats","win","claim","prize","subscribe", "unsubscribe", "order", "call", "dial", "buy","link","sale","store","visit")

  val finalCategorizationC = decisionTree(categorizePositions,listOfCVintersected,specificKeywords)
*/

  evaluationMetrics(cvCategoriesVector, finalCategorizationC)


  println("EUCLIDEAN SIMILARITY")
  val distanceMatrix = distanceVector(TFIDFMatrixCV, convertedMatrix)
  val positionsE = distanceMatrix(*, ::).map(row => argmin(row))
  val valuesE :DenseVector[Double] = distanceMatrix(*, ::).map(row => min(row))

  val categorizePositionsE =  DenseVector((0 until valuesE.length).map(i=>

  if((valuesE.data(i) > 0.60 && ( cvLength.drop(i).head < 8) ) || cvLength.drop(i).head <2 ) 0
  else trainingSetVector.data(positionsE.data(i))).toArray)

  val finalCategorizationE = decisionTree(categorizePositionsE,listOfCVintersected,specificKeywords)
  //listOfCVintersected
  // == 0 && trainingSetStopWords.drop(x).head._2.contains("WEBSITE"))
  evaluationMetrics(cvCategoriesVector, finalCategorizationE)

/*
println("EUCLIDEAN SIMILARITY")
val distanceMatrix = distanceVector(TFIDFMatrixCV, convertedMatrix)
val positionsE = distanceMatrix(*, ::).map(row => argmin(row))
val valuesE :DenseVector[Double] = distanceMatrix(*, ::).map(row => min(row))
val categorizePositionsE = positionsE.map(x => trainingSetVector(x))

val finalCategorizationE = DenseVector((0 until positionsE.length).map(i=>
  if(categorizePositionsE.data(i) == 0 && valuesE.data(i)>3 && containsMoreString(3, listOfCVintersected.drop(i).head, specificKeywords)) 1
  else if (categorizePositionsE.data(i) == 1 && valuesE.data(i)>3 && containsLessString(0, listOfCVintersected.drop(i).head, specificKeywords)) 0
  else categorizePositionsE.data(i)).toArray)

*/
evaluationMetrics(cvCategoriesVector, finalCategorizationE)

  val superFinalCategorization = (finalCategorizationC + finalCategorizationE + decisionT ).map(x=>
    x match {
      case 0 => 0
      case 1 => 0
      case 2 => 1
      case 3 => 1
    }
  )

 println("FINAL METRICS")
 evaluationMetrics(cvCategoriesVector,superFinalCategorization)




  //Running time in seconds
  val finalTime = System.currentTimeMillis
  val timeRunning = (finalTime - inicialTime).toDouble/1000 + " seconds"