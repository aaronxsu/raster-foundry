package com.rasterfoundry.datamodel

import java.sql.Timestamp
import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import geotrellis.vector.{Geometry, Projected, io => _}
import io.circe.generic.JsonCodec
import io.circe.generic.extras._
import io.circe.Encoder
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._

@JsonCodec
final case class AnnotationFeatureCollectionCreate(
    features: Seq[Annotation.GeoJSONFeatureCreate]
)

@JsonCodec
final case class Annotation(
    id: UUID,
    projectId: UUID,
    createdAt: Timestamp,
    createdBy: String,
    modifiedAt: Timestamp,
    owner: String,
    label: String,
    description: Option[String],
    machineGenerated: Option[Boolean],
    confidence: Option[Float],
    quality: Option[AnnotationQuality],
    geometry: Option[Projected[Geometry]],
    annotationGroup: UUID,
    labeledBy: Option[String],
    verifiedBy: Option[String],
    projectLayerId: UUID,
    taskId: Option[UUID]
) extends GeoJSONSerializable[Annotation.GeoJSON] {
  def toGeoJSONFeature: Annotation.GeoJSON = Annotation.GeoJSON(
    this.id,
    this.geometry,
    AnnotationProperties(
      this.projectId,
      this.createdAt,
      this.createdBy,
      this.modifiedAt,
      this.owner,
      this.label,
      this.description,
      this.machineGenerated,
      this.confidence,
      this.quality,
      this.annotationGroup,
      this.labeledBy,
      this.verifiedBy,
      this.projectLayerId,
      this.taskId
    )
  )
}

@JsonCodec
final case class AnnotationProperties(
    projectId: UUID,
    createdAt: Timestamp,
    createdBy: String,
    modifiedAt: Timestamp,
    owner: String,
    label: String,
    description: Option[String],
    machineGenerated: Option[Boolean],
    confidence: Option[Float],
    quality: Option[AnnotationQuality],
    annotationGroup: UUID,
    labeledBy: Option[String] = None,
    verifiedBy: Option[String] = None,
    projectLayerId: UUID,
    taskId: Option[UUID] = None
)

@JsonCodec
final case class AnnotationPropertiesCreate(
    owner: Option[String],
    label: String,
    description: Option[String],
    machineGenerated: Option[Boolean],
    confidence: Option[Float],
    quality: Option[AnnotationQuality],
    annotationGroup: Option[UUID],
    labeledBy: Option[String] = None,
    verifiedBy: Option[String] = None,
    taskId: Option[UUID] = None
)

object Annotation extends LazyLogging {

  implicit val config: Configuration =
    Configuration.default.copy(transformMemberNames = {
      case "_type" => "type"
      case other   => other
    })

  def tupled = (Annotation.apply _).tupled
  def create = Create.apply _
  @ConfiguredJsonCodec
  final case class GeoJSON(
      id: UUID,
      geometry: Option[Projected[Geometry]],
      properties: AnnotationProperties,
      _type: String = "Feature"
  ) extends GeoJSONFeature {
    def toAnnotation: Annotation = {
      Annotation(
        id,
        properties.projectId,
        properties.createdAt,
        properties.createdBy,
        properties.modifiedAt,
        properties.owner,
        properties.label match {
          case "" => "Unlabeled"
          case _  => properties.label
        },
        properties.description,
        properties.machineGenerated,
        properties.confidence,
        properties.quality,
        geometry,
        properties.annotationGroup,
        properties.labeledBy,
        properties.verifiedBy,
        properties.projectLayerId,
        properties.taskId
      )
    }
  }

  @JsonCodec
  final case class Create(
      owner: Option[String],
      label: String,
      description: Option[String],
      machineGenerated: Option[Boolean],
      confidence: Option[Float],
      quality: Option[AnnotationQuality],
      geometry: Option[Projected[Geometry]],
      annotationGroup: Option[UUID],
      labeledBy: Option[String] = None,
      verifiedBy: Option[String] = None,
      taskId: Option[UUID] = None
  ) extends OwnerCheck {

    def toAnnotation(
        projectId: UUID,
        user: User,
        defaultAnnotationGroup: UUID,
        projectLayerId: UUID
    ): Annotation = {
      val now = new Timestamp(new java.util.Date().getTime)
      val ownerId = checkOwner(user, this.owner)
      Annotation(
        UUID.randomUUID, // id
        projectId, // projectId
        now, // createdAt
        user.id, // createdBy
        now, // modifiedAt
        ownerId, // owner
        label match {
          case "" => "Unlabeled"
          case _  => label
        },
        description,
        machineGenerated,
        confidence,
        quality,
        geometry,
        annotationGroup.getOrElse(defaultAnnotationGroup),
        labeledBy,
        verifiedBy,
        projectLayerId,
        taskId
      )
    }
  }

  @JsonCodec
  final case class GeoJSONFeatureCreate(
      geometry: Option[Projected[Geometry]],
      properties: AnnotationPropertiesCreate
  ) extends OwnerCheck {
    def toAnnotationCreate: Annotation.Create = {
      Annotation.Create(
        properties.owner,
        properties.label,
        properties.description,
        properties.machineGenerated,
        properties.confidence,
        properties.quality,
        geometry,
        properties.annotationGroup,
        properties.labeledBy,
        properties.verifiedBy,
        properties.taskId
      )
    }
  }
}

@JsonCodec
final case class AnnotationWithOwnerInfo(
    id: UUID,
    projectId: UUID,
    createdAt: Timestamp,
    createdBy: String,
    modifiedAt: Timestamp,
    owner: String,
    label: String,
    description: Option[String],
    machineGenerated: Option[Boolean],
    confidence: Option[Float],
    quality: Option[AnnotationQuality],
    geometry: Option[Projected[Geometry]],
    annotationGroup: UUID,
    labeledBy: Option[String],
    verifiedBy: Option[String],
    projectLayerId: UUID,
    taskId: Option[UUID],
    ownerName: String,
    ownerProfileImageUri: String
) extends GeoJSONSerializable[AnnotationWithOwnerInfo.GeoJSON] {
  def toGeoJSONFeature = AnnotationWithOwnerInfo.GeoJSON(
    this.id,
    this.geometry,
    AnnotationWithOwnerInfoProperties(
      this.projectId,
      this.createdAt,
      this.createdBy,
      this.modifiedAt,
      this.owner,
      this.label,
      this.description,
      this.machineGenerated,
      this.confidence,
      this.quality,
      this.annotationGroup,
      this.labeledBy,
      this.verifiedBy,
      this.projectLayerId,
      this.taskId,
      this.ownerName,
      this.ownerProfileImageUri
    )
  )
}

object AnnotationWithOwnerInfo {

  implicit val config: Configuration =
    Configuration.default.copy(transformMemberNames = {
      case "_type" => "type"
      case other   => other
    })

  @ConfiguredJsonCodec
  final case class GeoJSON(
      id: UUID,
      geometry: Option[Projected[Geometry]],
      properties: AnnotationWithOwnerInfoProperties,
      _type: String = "Feature"
  ) extends GeoJSONFeature
}

@JsonCodec
final case class AnnotationWithOwnerInfoProperties(
    projectId: UUID,
    createdAt: Timestamp,
    createdBy: String,
    modifiedAt: Timestamp,
    owner: String,
    label: String,
    description: Option[String],
    machineGenerated: Option[Boolean],
    confidence: Option[Float],
    quality: Option[AnnotationQuality],
    annotationGroup: UUID,
    labeledBy: Option[String] = None,
    verifiedBy: Option[String] = None,
    projectLayerId: UUID,
    taskId: Option[UUID] = None,
    ownerName: String,
    ownerProfileImageUri: String)

@JsonCodec
final case class AnnotationWithClasses(
    id: UUID,
    projectId: UUID,
    createdAt: Timestamp,
    createdBy: String,
    modifiedAt: Timestamp,
    owner: String,
    description: Option[String],
    machineGenerated: Option[Boolean],
    confidence: Option[Float],
    quality: Option[AnnotationQuality],
    geometry: Option[Projected[Geometry]],
    annotationGroup: UUID,
    labeledBy: Option[String],
    verifiedBy: Option[String],
    projectLayerId: UUID,
    taskId: Option[UUID],
    classes: Json
) extends GeoJSONSerializable[AnnotationWithClasses.GeoJSON] {
  def toGeoJSONFeature = AnnotationWithClasses.GeoJSON(
    this.id,
    this.geometry,
    AnnotationWithClassesProperties(
      this.projectId,
      this.createdAt,
      this.createdBy,
      this.modifiedAt,
      this.owner,
      this.description,
      this.machineGenerated,
      this.confidence,
      this.quality,
      this.annotationGroup,
      this.labeledBy,
      this.verifiedBy,
      this.projectLayerId,
      this.taskId,
      this.classes
    )
  )
}

object AnnotationWithClasses {
  final case class GeoJSON(
      id: UUID,
      geometry: Option[Projected[Geometry]],
      properties: AnnotationWithClassesProperties,
      _type: String = "Feature"
  ) extends GeoJSONFeature

  object GeoJSON {
    implicit val annoWithClassesGeojonEncoder: Encoder[GeoJSON] =
      Encoder.forProduct4("id", "geometry", "properties", "type")(
        geojson =>
          (geojson.id, geojson.geometry, geojson.properties, geojson._type)
      )
  }
}

final case class AnnotationWithClassesProperties(
    projectId: UUID,
    createdAt: Timestamp,
    createdBy: String,
    modifiedAt: Timestamp,
    owner: String,
    description: Option[String],
    machineGenerated: Option[Boolean],
    confidence: Option[Float],
    quality: Option[AnnotationQuality],
    annotationGroup: UUID,
    labeledBy: Option[String],
    verifiedBy: Option[String],
    projectLayerId: UUID,
    taskId: Option[UUID],
    classes: Json
)

object AnnotationWithClassesProperties {
  implicit val annotationWithClassesPropertiesEncoder
    : Encoder[AnnotationWithClassesProperties] =
    new Encoder[AnnotationWithClassesProperties] {
      final def apply(properties: AnnotationWithClassesProperties): Json = {
        val classMap: Map[String, Json] =
          properties.classes.as[Map[String, Json]].getOrElse(Map.empty)
        (
          Map(
            "projectId" -> properties.projectId.asJson,
            "createdAt" -> properties.createdAt.asJson,
            "createdBy" -> properties.createdBy.asJson,
            "modifiedAt" -> properties.modifiedAt.asJson,
            "owner" -> properties.owner.asJson,
            "description" -> properties.description.asJson,
            "machineGenerated" -> properties.machineGenerated.asJson,
            "confidence" -> properties.confidence.asJson,
            "quality" -> properties.quality.asJson,
            "annotationGroup" -> properties.annotationGroup.asJson,
            "labeledBy" -> properties.labeledBy.asJson,
            "verifiedBy" -> properties.verifiedBy.asJson,
            "projectLayerId" -> properties.projectLayerId.asJson,
            "taskId" -> properties.taskId.asJson
          ) ++ classMap
        ).asJson
      }
    }
}

final case class AnnotationWithClassesFeatureCollection(
    features: List[AnnotationWithClasses.GeoJSON],
    `type`: String = "FeatureCollection"
)

object AnnotationWithClassesFeatureCollection {
  implicit val annoWithClassesFCEncoder
    : Encoder[AnnotationWithClassesFeatureCollection] =
    deriveEncoder[AnnotationWithClassesFeatureCollection]
}

@JsonCodec
final case class AnnotationFeatureCollection(
    features: List[Annotation.GeoJSON],
    `type`: String = "FeatureCollection"
)
