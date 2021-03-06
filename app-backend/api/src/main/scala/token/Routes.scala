package com.rasterfoundry.api.token

import com.rasterfoundry.akkautil.{Authentication, UserErrorHandler}
import com.rasterfoundry.api.utils.Auth0ErrorHandler
import akka.http.scaladsl.server.Route
import com.rasterfoundry.datamodel.auth.RefreshToken
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._

/**
  * Routes for tokens
  */
trait TokenRoutes
    extends Authentication
    with UserErrorHandler
    with Auth0ErrorHandler {

  val tokenRoutes: Route =
    (handleExceptions(auth0ExceptionHandler) & handleExceptions(
      userExceptionHandler)) {
      pathEndOrSingleSlash {
        get { listRefreshTokens } ~
          post { getAuthorizedToken }
      } ~
        pathPrefix(Segment) { deviceId =>
          pathEndOrSingleSlash {
            delete { revokeRefreshToken(deviceId) }
          }
        }
    }

  def listRefreshTokens: Route = authenticate { user =>
    complete {
      TokenService.listRefreshTokens(user)
    }
  }

  def getAuthorizedToken: Route = {
    entity(as[RefreshToken]) { refreshToken =>
      complete {
        TokenService.getAuthorizedToken(refreshToken)
      }
    }
  }

  def revokeRefreshToken(deviceId: String): Route = authenticate { user =>
    complete {
      TokenService.revokeRefreshToken(user, deviceId)
    }
  }
}
