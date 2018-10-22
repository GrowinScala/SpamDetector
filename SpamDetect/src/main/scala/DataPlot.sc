import ProcessData._

//Load training set from file
lazy val trainingSetLoaded = readFromFile("src\\main\\re" +
  "sources\\spamdata\\trainingset.dat")

//Converts training set in a categorize list
lazy val trainingSet = parseA("src\\main\\resources\\spamdata\\trainingset.dat",trainingSetLoaded)

//Load stopwords list from file
lazy val stopWordsList = readFromFile("src\\main\\resources\\spamdata\\stopWords.txt")

//Apply stemmer to stopwords
lazy val stemmedStopWords = applyStemmer(stopWordsList).distinct

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

//Save results to target file
saveToFile("src\\main\\resources\\spamdata\\output.dat", trainingSetStopWords)




