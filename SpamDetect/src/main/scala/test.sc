import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import ProcessData._
import javafx.scene.chart._
import org.knowm.xchart.{CategoryChart, CategoryChartBuilder, QuickChart}

//splitA("src\\main\\resources\\spamdata\\spam.dat")

val trainingSet = parseA("src\\main\\resources\\spamdata\\trainingset.dat")
val trainingSetcounted = countLength(trainingSet)

