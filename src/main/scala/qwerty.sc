import java.io.File

import DecisionTrees.{CosineTree, EuclideanTree}
import DefinedStrings.{FilesName, SpecificWords}
import ProcessingInformation.{ProcessData, ProcessSet}
import breeze.linalg._

import scala.io.Source
//import ProcessingInformation.ProcessData._
import DecisionTrees.DecisionTree._
import scalariform.formatter.preferences._


val fileName = new FilesName()
val dataProcess = new ProcessData()
breeze.linalg.csvread(new File(fileName.fileMatrixTFIDF),' ')
dataProcess.readListFromFile(fileName.fileListOfWords)
//breeze.linalg.csvread(new File(fileName.fileMatrixTFIDF), separator = ' ')
//Source.fromFile(fileName.fileListOfWords).getLines().toList