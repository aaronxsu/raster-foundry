package com.rasterfoundry.database

import com.rasterfoundry.datamodel._
import cats.implicits._
import doobie._
import io.circe.syntax._
import org.postgresql.util.PGobject
import java.time.LocalDate

import geotrellis.proj4.CRS
import geotrellis.raster.CellType

package object meta {
  trait RFMeta
      extends GtWktMeta
      with CirceJsonbMeta
      with EnumMeta
      with PermissionsMeta {

    implicit val crsMeta: Meta[CRS] =
      Meta[String].timap(CRS.fromString)(_.toProj4String)

    implicit val cellTypeMeta: Meta[CellType] =
      Meta[String].timap(CellType.fromName)(CellType.toName)

    implicit val timeRangeMeta: Meta[(LocalDate, LocalDate)] =
      Meta.Advanced
        .other[PGobject]("tsrange")
        .timap[(LocalDate, LocalDate)](intervalString => {
          // We can't piggy-back on json here, since the format of the interval string makes
          // circe _extremely angry_ and we can't even construct an HCursor to write a
          // custom decoder.
          val (s1, s2) = intervalString.getValue
            .replace("\"", "")
            .replace(" 00:00:00", "")
            .replace("[", "")
            .replace(")", "")
            .split(",")
            .toList match {
            case h :: t :: Nil =>
              (h, t)
            case _ =>
              ("", "")
          }
          Either
            .catchNonFatal((LocalDate.parse(s1), LocalDate.parse(s2)))
            .leftMap[(LocalDate, LocalDate)](e => throw e)
            .merge
        })(
          a => {
            val o = new PGobject
            o.setType("tsrange")
            o.setValue(a.asJson.noSpaces.replace("\"", ""))
            o
          }
        )
  }
}
