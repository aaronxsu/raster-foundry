package com.rasterfoundry.akkautil

import com.rasterfoundry.datamodel.AuthResult

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{
  Directive0,
  ExceptionHandler,
  RequestContext,
  RouteResult,
  StandardRoute
}
import akka.http.scaladsl.server.directives.{
  RouteDirectives,
  FutureDirectives,
  CompleteOrRecoverWithMagnet,
  SecurityDirectives
}
import io.circe._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait CommonHandlers extends RouteDirectives {

  implicit val ec: ExecutionContext

  def completeWithOneOrFail(
      future: ⇒ Future[Int]
  ): RequestContext => Future[RouteResult] =
    FutureDirectives.onComplete(future) {
      case Success(count) => completeSingleOrNotFound(count)
      case Failure(err)   => failWith(err)
    }

  def completeOrFail(
      magnet: CompleteOrRecoverWithMagnet
  ): RequestContext => Future[RouteResult] =
    FutureDirectives.completeOrRecoverWith(magnet)(failWith)

  def completeSingleOrNotFound(count: Int): StandardRoute = count match {
    case 1 => complete(StatusCodes.NoContent)
    case 0 => complete(StatusCodes.NotFound)
    case _ =>
      throw new IllegalStateException(s"Result expected to be 1, was $count")
  }

  def completeSomeOrNotFound(count: Int): StandardRoute = count match {
    case 0          => complete(StatusCodes.NotFound)
    case x if x > 0 => complete(StatusCodes.NoContent)
    case _ =>
      throw new IllegalStateException(
        s"Result expected to be 0 or positive, was $count")
  }

  def authorizeAuthResultAsync[T](
      authFut: Future[AuthResult[T]]): Directive0 = {
    SecurityDirectives.authorizeAsync { authFut map { _.toBoolean } }
  }

  def circeDecodingError = ExceptionHandler {
    case df: DecodingFailure =>
      complete {
        (StatusCodes.BadRequest, DecodingFailure.showDecodingFailure.show(df))
      }
  }
}
