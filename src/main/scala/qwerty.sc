import org.apache.spark.{SparkConf, SparkContext}

val conf = new SparkConf()
conf.setMaster("local[*]")
conf.setAppName("Simple Application")

val sc = new SparkContext(conf)

val logFile = "/src/resources/spamdata/trainingset.dat"
val logData = sc.textFile(logFile).cache()
val numAs = logData.filter(line => line.contains("a")).count()
val numBs = logData.filter(line => line.contains("b")).count()

println(s"Lines with a: $numAs, Lines with b: $numBs")