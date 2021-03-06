package com.rasterfoundry.batch.stacExport

import com.rasterfoundry.batch.Job
import com.rasterfoundry.batch.util.conf.Config
import com.rasterfoundry.database.util.RFTransactor
import com.rasterfoundry.database._
import com.rasterfoundry.datamodel._
import com.rasterfoundry.common.RollbarNotifier
import com.rasterfoundry.common.S3

import geotrellis.server.stac._
import java.sql.Timestamp
import java.util.Date
import java.util.UUID
import java.nio.charset.Charset
import java.io.ByteArrayInputStream
import doobie._
import doobie.implicits._
import io.circe._
import io.circe.syntax._
import cats.implicits._
import cats.effect.IO
import com.amazonaws.services.s3.model.{
  PutObjectRequest,
  ObjectMetadata,
  PutObjectResult
}

final case class WriteStacCatalog(exportId: UUID)(
    implicit val xa: Transactor[IO]
) extends Config
    with RollbarNotifier {

  val name = WriteStacCatalog.name

  protected def s3Client = S3()

  protected def putObjectToS3(
      selfLinkO: Option[String],
      dataO: Option[Json],
      contentType: String
  ): IO[Option[PutObjectResult]] = IO {
    (selfLinkO, dataO) match {
      case (Some(selfLink), Some(data)) =>
        val key = selfLink.replace(s"s3://${dataBucket}/", "")
        val dataByte = data.noSpaces.getBytes(Charset.forName("UTF-8"))
        val dataStream = new ByteArrayInputStream(dataByte)
        val dataMd = new ObjectMetadata()
        dataMd.setContentType(contentType)
        dataMd.setContentLength(dataByte.length)
        Some(
          s3Client.putObject(
            new PutObjectRequest(dataBucket, key, dataStream, dataMd)
          )
        )
      case (None, _) =>
        logger.error("No selflink for label data")
        None
      case (_, None) =>
        logger.error("No label data to upload")
        None
      case _ =>
        logger.error(s"No data and self link to upload")
        None
    }
  }

  protected def getStacSelfLink(stacLinks: List[StacLink]): Option[String] =
    stacLinks.find(_.rel == Self).map(_.href)

  // Use the SELF type link on each object to upload it to the correct place
  // label items are accopanied by a geojson asset, which should be uploaded relative
  // to the item itself
  protected def writeToS3(
      catalog: StacCatalog,
      layerSceneLabelCollectionsItemsAssets: List[
        (
            StacCollection, // layer collection
            (StacCollection, List[StacItem]), // scene collection and scene items
            (StacCollection, StacItem, (Option[Json], String)) // label collection, label item, label data, and s3 location
        )
      ]
  ): IO[List[PutObjectResult]] = {
    // catalog
    val writeCatalogIO: IO[Option[PutObjectResult]] = putObjectToS3(
      getStacSelfLink(catalog.links),
      Some(catalog.asJson),
      "application/json"
    )

    val writeOtherIO: List[IO[Option[PutObjectResult]]] =
      layerSceneLabelCollectionsItemsAssets.map {
        case (
            layerCollection,
            (sceneCollection, sceneItemList),
            (labelCollection, labelItem, (labelData, labelDataLink))
            ) =>
          // layer collection
          val writeLayerCollectionIO = putObjectToS3(
            getStacSelfLink(layerCollection.links),
            Some(layerCollection.asJson),
            "application/json"
          )
          // scene collection
          val writeSceneCollectionIO = putObjectToS3(
            getStacSelfLink(sceneCollection.links),
            Some(sceneCollection.asJson),
            "application/json"
          )
          // scene items
          val writeSceneItemIOs = sceneItemList.map(
            sceneItem =>
              putObjectToS3(
                getStacSelfLink(sceneItem.links),
                Some(sceneItem.asJson),
                "application/json"
            )
          )
          // label collection
          val writeLabelCollectionIO = putObjectToS3(
            getStacSelfLink(labelCollection.links),
            Some(labelCollection.asJson),
            "application/json"
          )
          // label item
          val writeLabelItemIO = putObjectToS3(
            getStacSelfLink(labelItem.links),
            Some(labelItem.asJson),
            "application/json"
          )
          // label data
          val writeLabelDataIO = putObjectToS3(
            Some(labelDataLink),
            labelData,
            "application/geo+json"
          )
          List(
            writeLayerCollectionIO,
            writeSceneCollectionIO,
            writeLabelCollectionIO,
            writeLabelItemIO,
            writeLabelDataIO
          ) ++ writeSceneItemIOs
      } flatten

    (List(writeCatalogIO) ++ writeOtherIO).sequence.map(_.flatten)
  }

  protected def sceneTaskAnnotationforLayers(
      layerDefinitions: List[StacExport.LayerDefinition],
      taskStatuses: List[String]
  ): ConnectionIO[Map[
    UUID,
    (
        List[Scene],
        Option[UnionedGeomExtent],
        List[Task],
        Option[UnionedGeomExtent],
        Option[Json],
        Option[StacLabelItemPropertiesThin]
    )
  ]] = {
    (layerDefinitions traverse {
      case StacExport.LayerDefinition(projectId, layerId) =>
        for {
          projectTypeO <- ProjectDao.getAnnotationProjectType(projectId)
          infoOption <- projectTypeO match {
            case Some(projectType) =>
              createLayerInfoMap(projectId, layerId, taskStatuses, projectType)
            case _ => Option.empty.pure[ConnectionIO]
          }
        } yield {
          infoOption match {
            case Some(info) => Some((layerId, info))
            case _          => None
          }
        }
    }) map { _.flatten.toMap }
  }

  protected def createLayerInfoMap(
      projectId: UUID,
      layerId: UUID,
      taskStatuses: List[String],
      projectType: MLProjectType
  ): ConnectionIO[Option[
    (
        List[Scene],
        Option[UnionedGeomExtent],
        List[Task],
        Option[UnionedGeomExtent],
        Option[Json],
        Option[StacLabelItemPropertiesThin]
    )
  ]] =
    for {
      scenes <- ProjectLayerScenesDao.listLayerScenesRaw(layerId)
      scenesGeomExtent <- ProjectLayerScenesDao.createUnionedGeomExtent(layerId)
      tasks <- TaskDao.listLayerTasksByStatus(projectId, layerId, taskStatuses)
      tasksGeomExtent <- TaskDao.createUnionedGeomExtent(
        projectId,
        layerId,
        taskStatuses
      )
      annotations <- AnnotationDao.getLayerAnnotationJsonByTaskStatus(
        projectId,
        layerId,
        taskStatuses,
        projectType
      )
      labelItemPropsThin <- ProjectDao.getAnnotationProjectStacInfo(projectId)
    } yield {
      Some(
        (
          scenes,
          scenesGeomExtent,
          tasks,
          tasksGeomExtent,
          annotations,
          labelItemPropsThin
        )
      )
    }

  protected def buildCatalog(contentBundle: ContentBundle): (
      StacCatalog, // catalog
      List[
        (
            StacCollection, // layer collection
            (StacCollection, List[StacItem]), // scene collection and scene items
            (StacCollection, StacItem, (Option[Json], String)) // label collection, label item, label data, and s3 location
        )
      ]
  ) = {
    /*
     Exported Catalog:
     |-> Layer collection
     |   |-> Scene Collection
     |   |   |-> Scene Item
     |   |   |-> Scene Item
     |   |   |-> (One or more scene items)
     |   |-> Label Collection
     |   |   |-> Label Item (Only one)
     |   |   |-> Label Data in GeoJSON Feature Collection
     |-> (One or more Layer Collections)
     Final structure is going to be on s3
     */
    val catalogBuilder =
      new StacCatalogBuilder[
        StacCatalogBuilder.CatalogBuilder.EmptyCatalog
      ]()
    val stacVersion = "0.8.0-rc1"
    val currentPath = s"s3://${dataBucket}/stac-exports"
    val catalogId = contentBundle.export.id.toString
    val catalogParentPath = s"${currentPath}/${catalogId}"
    val catalogDescription =
      s"Exported from Raster Foundry ${(new Timestamp((new Date()).getTime())).toString()}"
    val catalogOwnLinks = List(
      StacLink(
        // s3://rasterfoundry-production-data-us-east-1/stac-exports/<catalogId>/catalog.json
        s"${catalogParentPath}/catalog.json",
        Self,
        Some(`application/json`),
        Some(s"Catalog ${catalogId}"),
        List()
      ),
      // s3://rasterfoundry-production-data-us-east-1/stac-exports/<catalogId>/catalog.json
      StacLink(
        "catalog.json",
        StacRoot,
        Some(`application/json`),
        Some(s"Catalog ${catalogId}"),
        List()
      )
    )
    catalogBuilder
      .withVersion(stacVersion)
      .withParentPath(catalogParentPath, true)
      .withId(contentBundle.export.id.toString)
      .withTitle(contentBundle.export.name)
      .withDescription(catalogDescription)
      .withLinks(catalogOwnLinks)
      .withContents(contentBundle)
      .build()
  }

  def run(): IO[Unit] = {

    logger.info(s"Exporting STAC export for record ${exportId}...")

    /*
      For project:
      get the project extras field

      For each project layer:
      fetch scenes
      fetch tasks, filter by exportDefinition.taskStatuses
      fetch the annotations filtered by task statuses
      save the annotations as a geojson feature collection
      (map the labels of annotations according the project extras field, setting properties etc) -> already done in Daos

      returns:
      Map[
        UUID, // layer ID
        (
          List[Scene],
          List[Task],
          Option[Json], // STAC-compliant annotation data in a geojson feature collection
          Option[StacLabelItemPropertiesThin] // project label and class definition, will be used to populate label STAC item properties
        )
      ]
     */

    logger.info(s"Getting STAC export data for record ${exportId}...")
    val dbIO = for {
      exportDefinition <- StacExportDao.unsafeGetById(exportId)
      _ <- StacExportDao.update(
        exportDefinition.copy(exportStatus = ExportStatus.Exporting),
        exportDefinition.id
      )
      layerSceneTaskAnnotation <- sceneTaskAnnotationforLayers(
        exportDefinition.layerDefinitions,
        exportDefinition.taskStatuses
      )
    } yield (exportDefinition, layerSceneTaskAnnotation)

    logger.info(
      s"Creating content bundle with layers, scenes, and labels for record ${exportId}..."
    )

    val createCatalogIO: IO[
      (
          ContentBundle,
          StacCatalog,
          List[
            (
                StacCollection, // layer collection
                (StacCollection, List[StacItem]), // scene collection and scene items
                (StacCollection, StacItem, (Option[Json], String)) // label collection, label item, label data, and s3 location
            )
          ]
      )
    ] = dbIO.transact(xa) map {
      case (exportDef, layerInfo) =>
        val contentBundle = ContentBundle(
          exportDef,
          layerInfo
        )
        logger.info(s"Building a catalog for record ${exportId}...")

        val (catalog, layerSceneLabelCollectionsItemsAssets) =
          buildCatalog(contentBundle)
        logger.info(s"Built a catalog for record ${exportId}...")
        (contentBundle, catalog, layerSceneLabelCollectionsItemsAssets)
    }

    for {
      contentAndCatalog <- createCatalogIO
      (contentBundle, catalog, layerSceneLabelCollectionsItemsAssets) = contentAndCatalog
      _ <- IO {
        logger.info(s"Writing catalog to S3 for record ${exportId}...")
      }
      _ <- writeToS3(catalog, layerSceneLabelCollectionsItemsAssets)
      _ <- IO { logger.info(s"Wrote catalog to S3 for record ${exportId}...") }
      _ <- IO {
        logger.info(
          s"Updating export location and status for record ${exportId}..."
        )
      }
      exportUpdateCount <- StacExportDao
        .update(
          contentBundle.export.copy(
            exportStatus = ExportStatus.Exported,
            exportLocation =
              getStacSelfLink(catalog.links).map(_.replace("/catalog.json", ""))
          ),
          contentBundle.export.id
        )
        .transact(xa)
    } yield {
      logger
        .info(
          s"${exportUpdateCount} STAC export record for ${exportId} is updated"
        )
    }
  }

}

object WriteStacCatalog extends Job {
  val name = "write_stac_catalog"

  def runJob(args: List[String]): IO[Unit] = {
    RFTransactor.xaResource.use(transactor => {
      implicit val xa = transactor
      val job = args.toList match {
        case List(id: String) => WriteStacCatalog(UUID.fromString(id))
      }

      job.run
    })
  }
}
