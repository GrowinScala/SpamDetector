import java.io.File

import DecisionTrees.{CosineTree, EuclideanTree}
import DefinedStrings.{FilesName, SpecificWords}
import ProcessingInformation.{ProcessData, ProcessSet}
import breeze.linalg._

import scala.io.Source
//import ProcessingInformation.ProcessData._
import DecisionTrees.DecisionTree._
import java.net.URLDecoder

val result = URLDecoder.decode("Rui Valente")
//getClass.getResource("/spamdata").getPath