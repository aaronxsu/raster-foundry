package com.rasterfoundry.api.project

import com.rasterfoundry.api.utils.queryparams.QueryParametersCommon
import com.rasterfoundry.datamodel._
import com.rasterfoundry.datamodel.GeoJsonCodec._
import com.rasterfoundry.database._
import com.rasterfoundry.akkautil._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server._
import cats.effect._
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import doobie.Transactor
import doobie.implicits._

import scala.concurrent.ExecutionContext
import java.util.UUID

trait ProjectAnnotationRoutes
    extends Authentication
    with CommonHandlers
    with PaginationDirectives
    with ProjectAuthorizationDirectives
    with QueryParametersCommon {

  implicit val xa: Transactor[IO]
  implicit val ec: ExecutionContext

  def listAnnotations(projectId: UUID): Route = extractTokenHeader { tokenO =>
    extractMapTokenParam { mapTokenO =>
      (projectAuthFromMapTokenO(mapTokenO, projectId) |
        projectAuthFromTokenO(tokenO, projectId) | projectIsPublic(projectId)) {
        (withPagination & annotationQueryParams) {
          (page: PageRequest, queryParams: AnnotationQueryParameters) =>
            complete {
              (queryParams.withOwnerInfo match {
                case Some(true) =>
                  AnnotationDao
                    .listByLayerWithOwnerInfo(projectId, page, queryParams)
                    .transact(xa)
                    .unsafeToFuture
                    .map { p =>
                      {
                        fromPaginatedResponseToGeoJson[
                          AnnotationWithOwnerInfo,
                          AnnotationWithOwnerInfo.GeoJSON
                        ](p)
                      }
                    }
                case _ =>
                  AnnotationDao
                    .listByLayer(projectId, page, queryParams)
                    .transact(xa)
                    .unsafeToFuture
                    .map { p =>
                      {
                        fromPaginatedResponseToGeoJson[
                          Annotation,
                          Annotation.GeoJSON
                        ](p)
                      }
                    }
              })
            }
        }
      }
    }
  }

  def createAnnotation(projectId: UUID): Route = authenticate { user =>
    authorizeAuthResultAsync {
      ProjectDao
        .authorized(user, ObjectType.Project, projectId, ActionType.Annotate)
        .transact(xa)
        .unsafeToFuture
    } {
      entity(as[AnnotationFeatureCollectionCreate]) { fc =>
        val annotationsCreate = fc.features map { _.toAnnotationCreate }
        onSuccess(
          AnnotationDao
            .insertAnnotations(annotationsCreate.toList, projectId, user)
            .transact(xa)
            .unsafeToFuture
            .map { annotations: List[Annotation] =>
              fromSeqToFeatureCollection[Annotation, Annotation.GeoJSON](
                annotations
              )
            }
        ) { createdAnnotation =>
          complete((StatusCodes.Created, createdAnnotation))
        }
      }
    }
  }

  def exportAnnotationShapefile(projectId: UUID): Route = authenticate { user =>
    (annotationExportQueryParameters) { annotationExportQP =>
      authorizeAuthResultAsync {
        ProjectDao
          .authorized(user, ObjectType.Project, projectId, ActionType.View)
          .transact(xa)
          .unsafeToFuture
      } {
        onSuccess(
          AnnotationDao
            .listForProjectExport(projectId, annotationExportQP)
            .transact(xa)
            .unsafeToFuture
        ) {
          case annotations @ (_: List[Annotation]) => {
            complete(
              AnnotationShapefileService
                .getAnnotationShapefileDownloadUrl(annotations, user)
            )
          }
          case _ =>
            complete(
              throw new Exception(
                "Annotations do not exist or are not accessible by this user"
              )
            )
        }
      }
    }
  }

  def getAnnotation(projectId: UUID, annotationId: UUID): Route = authenticate {
    user =>
      authorizeAuthResultAsync {
        ProjectDao
          .authorized(user, ObjectType.Project, projectId, ActionType.View)
          .transact(xa)
          .unsafeToFuture
      } {
        rejectEmptyResponse {
          complete {
            AnnotationDao
              .getAnnotationById(projectId, annotationId)
              .transact(xa)
              .unsafeToFuture
              .map {
                _ map { _.toGeoJSONFeature }
              }
          }
        }
      }
  }

  def updateAnnotation(projectId: UUID): Route =
    authenticate { user =>
      authorizeAuthResultAsync {
        ProjectDao
          .authorized(user, ObjectType.Project, projectId, ActionType.Annotate)
          .transact(xa)
          .unsafeToFuture
      } {
        entity(as[Annotation.GeoJSON]) {
          updatedAnnotation: Annotation.GeoJSON =>
            onSuccess(
              AnnotationDao
                .updateAnnotation(projectId, updatedAnnotation.toAnnotation)
                .transact(xa)
                .unsafeToFuture
            ) { count =>
              completeSingleOrNotFound(count)
            }
        }
      }
    }

  def deleteAnnotation(projectId: UUID, annotationId: UUID): Route =
    authenticate { user =>
      authorizeAuthResultAsync {
        ProjectDao
          .authorized(user, ObjectType.Project, projectId, ActionType.Annotate)
          .transact(xa)
          .unsafeToFuture
      } {
        onSuccess(
          AnnotationDao
            .deleteById(projectId, annotationId)
            .transact(xa)
            .unsafeToFuture
        ) {
          completeSingleOrNotFound
        }
      }
    }

  def deleteProjectAnnotations(projectId: UUID): Route = authenticate { user =>
    authorizeAuthResultAsync {
      ProjectDao
        .authorized(user, ObjectType.Project, projectId, ActionType.Annotate)
        .transact(xa)
        .unsafeToFuture
    } {
      onSuccess(
        AnnotationDao
          .deleteByProjectLayer(projectId)
          .transact(xa)
          .unsafeToFuture
      ) {
        completeSomeOrNotFound
      }
    }
  }

  def listAnnotationGroups(projectId: UUID): Route = authenticate { user =>
    authorizeAuthResultAsync {
      ProjectDao
        .authorized(user, ObjectType.Project, projectId, ActionType.View)
        .transact(xa)
        .unsafeToFuture
    } {
      complete {
        AnnotationGroupDao
          .listForProject(projectId)
          .transact(xa)
          .unsafeToFuture
      }
    }
  }

  def createAnnotationGroup(projectId: UUID): Route = authenticate { user =>
    authorizeAuthResultAsync {
      ProjectDao
        .authorized(user, ObjectType.Project, projectId, ActionType.Annotate)
        .transact(xa)
        .unsafeToFuture
    } {
      entity(as[AnnotationGroup.Create]) { agCreate =>
        complete {
          AnnotationGroupDao
            .createAnnotationGroup(projectId, agCreate, user)
            .transact(xa)
            .unsafeToFuture
        }
      }
    }
  }

  def getAnnotationGroup(projectId: UUID, agId: UUID): Route = authenticate {
    user =>
      authorizeAuthResultAsync {
        ProjectDao
          .authorized(user, ObjectType.Project, projectId, ActionType.View)
          .transact(xa)
          .unsafeToFuture
      } {
        complete {
          AnnotationGroupDao
            .getAnnotationGroup(projectId, agId)
            .transact(xa)
            .unsafeToFuture
        }
      }
  }

  def getAnnotationGroupSummary(
      projectId: UUID,
      annotationGroupId: UUID
  ): Route = authenticate { user =>
    authorizeAuthResultAsync {
      ProjectDao
        .authorized(user, ObjectType.Project, projectId, ActionType.View)
        .transact(xa)
        .unsafeToFuture
    } {
      complete {
        AnnotationGroupDao
          .getAnnotationGroupSummary(annotationGroupId)
          .transact(xa)
          .unsafeToFuture
      }
    }
  }

  def updateAnnotationGroup(projectId: UUID, agId: UUID): Route = authenticate {
    user =>
      authorizeAuthResultAsync {
        ProjectDao
          .authorized(user, ObjectType.Project, projectId, ActionType.Annotate)
          .transact(xa)
          .unsafeToFuture
      } {
        entity(as[AnnotationGroup]) { annotationGroup =>
          complete {
            AnnotationGroupDao
              .updateAnnotationGroup(projectId, annotationGroup, agId)
              .transact(xa)
              .unsafeToFuture
          }
        }
      }
  }

  def deleteAnnotationGroup(projectId: UUID, agId: UUID): Route = authenticate {
    user =>
      authorizeAuthResultAsync {
        ProjectDao
          .authorized(user, ObjectType.Project, projectId, ActionType.Annotate)
          .transact(xa)
          .unsafeToFuture
      } {
        complete {
          AnnotationGroupDao
            .deleteAnnotationGroup(projectId, agId)
            .transact(xa)
            .unsafeToFuture
        }
      }
  }

}
