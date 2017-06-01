package com.azavea.rf.common

import com.azavea.rf.database.Database
import com.azavea.rf.database.tables.{ToolRuns, Tools}
import com.azavea.rf.datamodel.{Tool, ToolRun, User}
import com.azavea.rf.tool.ast.MapAlgebraAST
import com.azavea.rf.tool.ast.MapAlgebraAST._
import com.azavea.rf.tool.eval.{ASTDecodeError, DatabaseError, Interpreter}
import com.azavea.rf.tool.params.EvalParams

import cats._
import cats.data._
import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import io.circe._

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

package object ast {

  def assembleSubstitutions(
    ast: MapAlgebraAST,
    user: User,
    assembled: Map[UUID, MapAlgebraAST] = Map()
  )(implicit database: Database, ec: ExecutionContext): OptionT[Future, Map[UUID, MapAlgebraAST]] =
    ast match {
      case ToolReference(id, refId) =>
        OptionT(Tools.getTool(refId, user)).map({ referent =>
          val astPatch = referent.definition.as[MapAlgebraAST].valueOr(throw _)
          if (assembled.keys.toList.contains(refId)) ??? // CYCLE DETECTED
          else Map(refId -> astPatch)
        })
      case op: MapAlgebraAST.Operation =>
        val childSubstitutions = op.args.map({ arg => assembleSubstitutions(arg, user, assembled) }).sequence
        childSubstitutions.map({ substitutions =>
          substitutions.foldLeft(Map[UUID, MapAlgebraAST]())({ case (map, subs) => map ++ subs })
        })
      case _ =>
        OptionT.pure[Future, Map[UUID, MapAlgebraAST]](Map[UUID, MapAlgebraAST]())
      }

  def substituteBranches(
    ast: MapAlgebraAST,
    user: User,
    substitutions: Map[UUID, MapAlgebraAST]
  )(implicit database: Database, ec: ExecutionContext): OptionT[Future, MapAlgebraAST] = ast match {
    case source: Source =>
      OptionT.pure[Future, MapAlgebraAST](source)
    case operation: Operation =>
      val updatedArgs: Future[List[MapAlgebraAST]] = operation.args.map({ arg =>
        substituteBranches(arg, user, substitutions)
      }).map(_.value)
      .sequence
      .map(_.flatten)

      OptionT.liftF(updatedArgs.map(operation.withArgs(_)))
    case ToolReference(id, refId) => {
      OptionT.fromOption(substitutions.get(refId))
    }
  }


  /** Validate an AST, given some ToolRun. In the case of success, returns
    * the zero element of some specified Monoid.
    */
  def validateAST[M: Monoid](
    toolRunId: UUID,
    user: User
  )(implicit database: Database, ec: ExecutionContext): Future[M] = {

    val result: OptionT[Future, Interpreter.Interpreted[M]] = for {
      toolRun <- OptionT(database.db.run(ToolRuns.getToolRun(toolRunId, user)))
      tool    <- OptionT(Tools.getTool(toolRun.tool, user))
      oldAst  <- OptionT.pure[Future, MapAlgebraAST](tool.definition.as[MapAlgebraAST].valueOr(throw _))
      subs    <- assembleSubstitutions(oldAst, user)
      ast     <- substituteBranches(oldAst, user, subs)
      params  <- OptionT.pure[Future, EvalParams](toolRun.executionParameters.as[EvalParams].valueOr(throw _))
    } yield {
      val ast = prepareAst(t, tr, user)
      Interpreter.interpretPure[M](ast, params.sources)
    }

    result.value.map({
      case Some(Valid(a)) => a
      case Some(Invalid(nel)) => throw InterpreterException(nel)
      case None => throw InterpreterException(NonEmptyList.of(DatabaseError(toolRunId)))
    })
  }

  def prepareAST(t: Tool.WithRelated, tr: ToolRun, user: User): Interpreter.Interpreted[MapAlgebraAST] = ???

  def validateASTPure[M: Monoid](ast: MapAlgebraAST, params: EvalParams): Interpreter.Interpreted[M] = {

    //(t.definition.as[MapAlgebraAST] |@| tr.executionParameters.as[EvalParams]).map({ case (ast, params) =>
    //  Interpreter.interpretPure[M](ast, params.sources)
    //}) match {
    //  case Right(a) => a
    //  case Left(err) => Invalid(NonEmptyList.of(ASTDecodeError(tr.id, err)))
    //}
    ???
  }
}
