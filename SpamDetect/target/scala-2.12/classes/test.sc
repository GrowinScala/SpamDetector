import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import ProcessData._
import javafx.scene.chart._
import org.knowm.xchart.{CategoryChart, CategoryChartBuilder, QuickChart}

//splitA("src\\main\\resources\\spamdata\\spam.dat")

val trainingSet = parseA("src\\main\\resources\\spamdata\\trainingset.dat")
val trainingSetcounted = countLength(trainingSet)
/*
var f= Figure()
var p1 = f.subplot(2, 1, 0)
var g1 = cvSetcounted.filter(x => x._1==0).map(x => x._2)
p1 += hist(g1,50, "Ham")
var p2 = f.subplot(2, 1, 1)
var g2 = cvSetcounted.filter(x => x._1==1).map(x => x._2)
p2 += hist(g2, 15, "Spam")
//p2.xaxis
*/
val g1 = trainingSetcounted.filter(x => x._1==0).map(x => x._2)
var g2 = trainingSetcounted.filter(x => x._1==1).map(x => x._2)
saveToFileInt("src\\main\\resources\\spamdata\\g1.dat", g1)
saveToFileInt("src\\main\\resources\\spamdata\\g2.dat", g2)

//XYChart chart = QuickChart.getChart("Sample Chart", "X", "Y", "y(x)", )
