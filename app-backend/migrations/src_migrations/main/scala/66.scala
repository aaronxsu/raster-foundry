import slick.driver.PostgresDriver.api._
import com.liyaos.forklift.slick.SqlMigration

object M66 {
  RFMigrations.migrations = RFMigrations.migrations :+ SqlMigration(66)(List(
    sqlu"""
ALTER TABLE scenes DROP CONSTRAINT scene_name_org_datasource;
"""
  ))
}
