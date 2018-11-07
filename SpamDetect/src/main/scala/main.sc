import ProcessData._
import breeze.linalg.{*, DenseMatrix, DenseVector, argmax, max}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////TRAINING DATA-SET///////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Initial Runtime time
val inicialTime: Long = System.currentTimeMillis

//Gets the path until the file name
//Filters spaces from User so that intellij is able to read the correct path
val spamDataPath =  getClass.getResource("/spamdata").getPath.replaceAll("%20", " ")

//Load training set from file
lazy val trainingSetLoaded = readListFromFile("src\\main\\resources\\spamdata\\trainingset.dat")

//Converts training set in a categorize list
lazy val trainingSet = parseA(trainingSetLoaded)

//Converts all categorizations (ham or spam as 0 and 1) into a Dense Vector
val trainingSetVector =  DenseVector(trainingSet.map(x => x._1).toArray)

//Load stopwords list from file
lazy val stopWordsList = readListFromFile("src\\main\\resources\\spamdata\\stopWords.txt")

//Apply stemmer to stopwords
lazy val stemmedStopWords = applyStemmer(stopWordsList)

//Convert all characters to lower case
val trainingSetLower = uppertoLower(trainingSet)

//Generalize data into a specific group of words
val trainingReplace = replaceOverall(trainingSetLower)

//Remove all punctuation
val trainingSetPunctuation = takePunctuation(trainingReplace)

//Apply stemmer to the list of sentences (string) and merge it with the correspondent category (ham -> 0 or spam ->1)
val trainingStemmed = trainingSetPunctuation.map(x=> x._1).zip(applyStemmer(trainingSetPunctuation.map(x=> x._2)))

//Remove stopwords from training set
val trainingSetStopWords = takeStopWords(stemmedStopWords, trainingStemmed)

//Make term frequency matrix from target set
val termFrequencyMatrix = makeTFMatrix(trainingSetStopWords)

//Save results to target file
saveToFile("src\\main\\resources\\spamdata\\output.dat", trainingSetStopWords)


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////CROSS-VALIDATION///////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Read the matrix created and saved in function
lazy val TFIDFMatrixCV = readMatrixFromFile(spamDataPath +"/matrixTFIDF.csv")

//Read cross validation set
val cvList = readListFromFile(spamDataPath + "/crossvalidation.dat")

//Converts cross validation set in a categorize list
val cvSet = parseA(cvList)

//Turn the characters to lower case
val cvSetLower = uppertoLower(cvSet)

//Generalize data into a specific group of words
val cvReplace = replaceOverall(cvSetLower)

//Remove all punctuation
val cvSetPonctuation = takePunctuation(cvReplace)

//Creates an ordered list of the categories (spam->1 or ham ->0)(int) for each string
val cvCategories = cvSetPonctuation.map(x=> x._1)

// Converts cvCatategories into a Dense Vector
val cvCategoriesVector = DenseVector(cvCategories.toArray)

//Apply stemmer to the list of sentences (string) and merge it with the correspondent category (ham -> 0 or spam ->1)
val cvStemmed = cvCategories.zip(applyStemmer(cvSetPonctuation.map(x=> x._2)))

//Remove stopwords from training set
val cvSetStopWords = takeStopWords(stemmedStopWords, cvStemmed)

//Read list of Words that were achieved in the training set data
val listOfWords = readListFromFile(spamDataPath + "/listOfWords.dat")

//Maps each word with the value 0
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
val positionsC :DenseVector[Int] = cosineMatrix(*, ::).map(row => argmax(row))

//For every vector of the cosineVector list, it is calculated the maximum value.
//This value corresponds to the most similar string of training data with the string of CV data considered
val valuesC :DenseVector[Double] = cosineMatrix(*, ::).map(row => max(row))

//Counts the number of words in the filtered cross validation
val cvLength = countLength(cvSetStopWords).map(x=> x._2)

//Gets the categorization of the most similar cosine vectors between cross validation set and training set
val categorizePositions =  DenseVector((0 until valuesC.length).map(i=>

  //If cosine similatiry is less then 0.4 and the correspondent sentence has less than 8 words it´s classified has ham
  if(valuesC.data(i) < 0.40 && ( cvLength.drop(i).head < 8) ) 0

  else trainingSetVector.data(positionsC.data(i))).toArray)

//Performs evaluation metrics between the real categorization value and the predicted value
evaluationMetrics(cvCategoriesVector, categorizePositions)

//Final Runtime time
val finalTime = System.currentTimeMillis

//Run time in seconds
val timeRunning = (finalTime - inicialTime)/1000 + " seconds"