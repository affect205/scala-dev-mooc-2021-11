package module3

import module3.zio_homework.config.AppConfig
import zio.clock.Clock
import zio.console.{Console, _}
import zio.duration.Duration
import zio.macros.accessible
import zio.random._
import zio.{Has, RIO, ULayer, URIO, ZIO, ZLayer}

import scala.concurrent.duration.MILLISECONDS
import scala.language.postfixOps

package object zio_homework {

  type ZDI = Console with Clock with Random

  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */
  lazy val guessProgram: RIO[ZDI, Unit] = {
    for {
      _ <- ZIO.environment[Clock].map(_.get)
      _ <- ZIO.environment[Console].map(_.get)
      random <- ZIO.environment[Random].map(_.get)
      randomVal <- random.nextIntBetween(0, 10)
      attempts <- doWhile(v => randomVal == v)
      _ <- putStrLn(s"You have guessed the number $randomVal per $attempts attempts.")
    } yield()
  }

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */
  def doWhile(predicate: Int => Boolean, attempts: Int = 1): RIO[ZDI, Int] = {
    for {
      _ <- putStrLn("Input number between 0 and 9:")
      input <- getStrLn
      number <- input match {
        case x if x.matches("\\d+") => ZIO.succeed(x.toInt)
        case x => putStrLn(s"Non parsable input '$x'. Used attempts: $attempts. Try again..") *> doWhile(predicate, attempts + 1)
      }
      triedAttempts <- if (predicate(number)) {
        ZIO.succeed(attempts)
      } else {
        putStrLn(s"You haven't guessed. Used attempts: $attempts. Try again..") *> doWhile(predicate, attempts + 1)
      }
    } yield triedAttempts
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault: RIO[Console, Unit] = for {
    conf <- config.load.orElse(
      ZIO.effect(AppConfig(
        appName = "default_app",
        appUrl = "http://host:8080/path/to/anything")
      )
    )
    _ <- putStrLn(s"App config: $conf")
  } yield()


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  def eff(implicit effectDelay: Duration): URIO[ZDI, Int] = {
    (for {
      random <- ZIO.environment[Random].map(_.get)
      randomVal <- random.nextIntBetween(0, 11)
      _ <- putStrLn(s"Generated value: $randomVal")
    } yield randomVal).delay(effectDelay)
  }
  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  def effects(implicit effectDelay: Duration): Seq[URIO[ZDI, Int]] = for {
    _ <- 0 until 10
    effects = eff
  } yield effects


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  def sumOfValuesApp(implicit effectDelay: Duration): URIO[ZDI, Int] = {
    zioConcurrency.printEffectRunningTime(
      for {
        sumOfElements <- ZIO.foldLeft(effects)(0)((s, a) => a.map(_+s))
        _ <- putStrLn(s"Sum of elements: $sumOfElements")
      } yield sumOfElements)
  }


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: URIO[ZDI, Int] = {
    implicit val effectDelay: Duration = Duration(100, MILLISECONDS)
    for {
      sumOfValues <- sumOfValuesApp
    } yield sumOfValues
  }


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  type TimeLogService = Has[TimeLog.Service]

  @accessible
  object TimeLog {

    trait Service {
      def run[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
    }

    class TimeLogServiceImpl extends Service {
      override def run[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = {
        zioConcurrency.printEffectRunningTime(zio)
      }
    }

    val live: ULayer[TimeLogService] = ZLayer.succeed(new TimeLogServiceImpl())
  }

  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  def appWithTimeLogg(implicit effectDelay: Duration): URIO[ZDI with TimeLogService, Int] = {
    for {
      timeLogService <- ZIO.service[TimeLog.Service]
      sumOfElements <- timeLogService.run(ZIO.foldLeft(effects)(0)((s, a) => a.map(_+s)))
      _ <- putStrLn(s"Sum of elements: $sumOfElements")
    } yield sumOfElements
  }

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  lazy val runApp: URIO[ZDI with TimeLogService, Int] = {
    implicit val effectDelay: Duration = Duration.fromMillis(100)
    appWithTimeLogg
  }
}