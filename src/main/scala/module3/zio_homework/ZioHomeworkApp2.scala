package module3.zio_homework

object ZioHomeworkApp2 {
  def main(args: Array[String]): Unit = {
//    zio.Runtime.default.unsafeRun(loadConfigOrDefault)
//    zio.Runtime.default.unsafeRun(sumOfElementsApp)
    zio.Runtime.default.unsafeRun(appSpeedUp)
  }
}
