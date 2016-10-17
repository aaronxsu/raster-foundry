package com.azavea.rf.database.fields

import com.azavea.rf.database.ExtendedPostgresDriver.api._

trait BucketFields  { self: Table[_] =>
  def name: Rep[String]
  def slugLabel: Rep[String]
  def description: Rep[String]
}
