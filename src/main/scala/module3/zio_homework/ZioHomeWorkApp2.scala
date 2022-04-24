package module3.zio_homework

import zio.ULayer

object ZioHomeWorkApp2 {

  lazy val appEnv: ULayer[TimeLogService] = TimeLog.live

  def main(args: Array[String]): Unit = {
    zio.Runtime.default.unsafeRun(runApp.provideSomeLayer[ZDI](appEnv))
  }
}