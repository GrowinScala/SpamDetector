import java.io.File

object ProcessData {

  private[data] def filePath = {
    val resource = this.getClass.getClassLoader.getResource("spamdata/spam.dat")
    if (resource == null) sys.error("Please download the dataset as explained in the assignment instructions")
    new File(resource.toURI).getPath
  }
  private[data] def parse(line: String): List[(Int,String)] = {

    val index = line.indexOf(",")
    val classification = line.substring(0,index) match {
      case "ham" => 0
      case "spam" => 1
    }
    val text  = line.takeRight(line.length-index)
    List((classification, text))

  }

}
