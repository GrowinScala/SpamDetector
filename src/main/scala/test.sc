/**
  * Set timer
  */
val inicialTime: Long = System.currentTimeMillis
val m = new Main("Where are you? At the mall? Call me")
m.convertToResponse

/**
  * Running time in seconds
  */
val finalTime = System.currentTimeMillis
val timeRunning = (finalTime - inicialTime).toDouble / 1000 + " seconds"