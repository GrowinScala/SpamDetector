import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import ProcessData._

//import list of Stop Words provided early
lazy val stopWordsList = readListFromFile("src\\main\\resources\\spamdata\\stopWords.txt")
//apply stemmer to the list of stop Words and take the common words
val stemmedStopWords = applyStemmer(stopWordsList).distinct


  //splitA("src\\main\\resources\\spamdata\\spam.dat")
  lazy val trainingSetLoaded = readListFromFile("src\\main\\re" +
                             "sources\\spamdata\\trainingset.dat")
  lazy val trainingSet = parseA(trainingSetLoaded)
/*
  //saveToFile("src\\main\\resources\\spamdata\\trainingset.dat", trainingSet)
  */
  val trainingSetLower = uppertoLower(trainingSet)
  val trainingReplace = replaceOverall(trainingSetLower)
  val trainingSetPonctuation = takePunctuation(trainingReplace)
  val trainingStemmed = trainingSetPonctuation.map(x=> x._1)
                        .zip(applyStemmer(trainingSetPonctuation.map(x=> x._2)))
  val trainingSetStopWords = takeStopWords(stemmedStopWords, trainingStemmed)
  //
  val TFmatrix = makeTFMatrix(trainingSetStopWords)
  //Creates TFIDF matrix through TF matrix
  val TFIDFMatrix = makeTFIDFMatrix(TFmatrix)
  //Saves in csv file the matrix TFIDF
  saveToFile("src\\main\\resources\\spamdata\\matrixTFIDF.csv", TFIDFMatrix)


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////CROSS-VALIDATION///////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Read the matrix created and saved in function
 lazy val TFIDFMatrixCV = readMatrixFromFile("src\\main\\resources\\spamdata\\matrixTFIDF.csv")

// trial of a list the would be the cross-validation Set

val cvSet = List(
    (1, "Congrats 2 mobile 3G Videophones R yours. call 09063458130 now! videochat wid ur mates, play java games, Dload polypH music, noline rentl. bx420. ip4. 5we. 150p"),
    (0,"I hope your pee burns tonite."),
    (0,"K, wat s tht incident?"),
    (1,"Todays Voda numbers ending 1225 are selected to receive a ?50award. If you have a match please call 08712300220 quoting claim code 3100 standard rates app "),
    (1,"FreeMsg Hey there darling it's been 3 week's now and no word back! I'd like some fun you up for it still? Tb ok! XxX std chgs to send, ?1.50 to rcv")
  )


 // val cvList = readListFromFile("src\\main\\resources\\spamdata\\crossvalidation.dat")
 //val cvSet = parseA(cvList)

  //Turn the characters to lower case
  val cvSetLower = uppertoLower(cvSet)

  //Generalize data into a specific group of words
  val cvReplace = replaceOverall(cvSetLower)

  //Remove all punctuation
  val cvSetPonctuation = takePunctuation(cvReplace)

  //Creates an ordered list of the categories (spam->1 or ham ->0)(int) for each string
  val cvCategories = cvSetPonctuation.map(x=> x._1)
  //Apply stemmer to the list of sentences (string) and merge it with the correspondent category (ham -> 0 or spam ->1)
  val cvStemmed = cvCategories.zip(applyStemmer(cvSetPonctuation.map(x=> x._2)))

  //Remove stopwords from training set
  val cvSetStopWords = takeStopWords(stemmedStopWords, cvStemmed)

  //Read list of Words that were achieved in the training set data
  lazy val listOfWords = readListFromFile("src\\main\\re" +
  "sources\\spamdata\\listOfWords.dat")

  //maps each word with the value 0
  val mappedLisfOfWords : Map[String,Double]= listOfWords.map(x=> x->0.0).toMap

  //List of words of every sentence without repetitions, without empty strings
  val listOfCVSentences = cvSetStopWords.map(x=>
                        tokenization(x._2).filterNot(x=> x.equals("")))


  //Filter, from the cross validation set, the sentences that are not considered in the list of Words
  //created from the training set
  val listOfCVintersected = listOfCVSentences.map(x=> x.filter(y => listOfWords.contains(y)))


  //Every words is attributed the value of converted sentence into a map where each vector
  //maps the proportion of the words presented in a specific sentence (Term Frequency)
  val convertedVectorList: List[DenseVector[Double]] = listOfCVintersected.map(x=>
                        DenseVector((mappedLisfOfWords ++ x.foldLeft(Map.empty[String, Double]){
                        (count, word) => count + (word -> (count.getOrElse(word, 0.0) + 1.0))
                        }).values.toArray))

  //Cosine similarity is a measure of similarity between two non-zero vectors of an inner product space that
  //measures the cosine of the angle between them
  val cosineVector : List[Transpose[DenseVector[Double]]]  = (convertedVectorList.map(vector => TFIDFMatrixCV(::,*)
                    .map(collumn => cosineSimilarity(vector.toArray, collumn.toArray))))

  //For every vector of the cosineVector list, it is calculated the position of the maximum value.
  //This position corresponds to the most similar string of training data with the string of CV data considered


  val positionsC: List[IndexedSeq[Int]] = positions(cosineVector,3)

  //
  val categorizePositions = seeMajority(positionsC,trainingSet)
  // positionsC.map(x => trainingSet.drop(x).head._1)

  f1Score(cvCategories, categorizePositions)
