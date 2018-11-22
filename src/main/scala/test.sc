/**
 * Set timer
 */
val inicialTime: Long = System.currentTimeMillis
val m = new Main("Due to a new legislation, those struggling with debt can now apply to have it written off. For more information text the word INFO or to opt out text STOP")
m.convertToResponse

/**
 * Running time in seconds
 */
val finalTime = System.currentTimeMillis
val timeRunning = (finalTime - inicialTime).toDouble / 1000 + " seconds"