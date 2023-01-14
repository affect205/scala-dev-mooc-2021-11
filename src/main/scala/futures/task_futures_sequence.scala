package futures

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    Future.traverse(futures)(
      _.map(Success(_)).recover {
        case ex: Exception => Failure(ex)
      }
    )
      .map(result => result.partitionMap {
        case Success(x: A) => Left(x)
        case Failure(ex) => Right(ex)
      }
      )
  }

  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    futures.foldLeft[Future[(List[A], List[Throwable])]](Future.successful(List.empty[A], List.empty[Throwable])) {
      case (acc, future) =>
        acc.flatMap { case (successList, failureList) =>
          future.map(value => ((value +: successList), failureList))
            .recoverWith { case ex =>
              Future.successful((successList, (ex +: failureList)))
            }
        }
    }
  }

}
