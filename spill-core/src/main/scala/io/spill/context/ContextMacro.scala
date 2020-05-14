package io.spill.context

import scala.reflect.macros.whitebox.{ Context => MacroContext }
import io.spill.ast.{ Ast, Dynamic, Lift, Tag }
import io.spill.quotation.Quotation
import io.spill.util.LoadObject
import io.spill.util.MacroContextExt._
import io.spill.quotation.IsDynamic
import io.spill.NamingStrategy
import io.spill.idiom._

import scala.util.Success
import scala.util.Failure

trait ContextMacro extends Quotation {
  val c: MacroContext
  import c.universe.{ Function => _, Ident => _, _ }

  protected def expand(ast: Ast): Tree =
    q"""
      val (idiom, naming) = ${idiomAndNamingDynamic}
      val (ast, statement) = ${translate(ast)}
      io.spill.context.Expand(${c.prefix}, ast, statement, idiom, naming)
    """

  protected def extractAst[T](quoted: Tree): Ast =
    unquote[Ast](c.typecheck(q"quote($quoted)"))
      .map(VerifyFreeVariables(c))
      .getOrElse {
        Dynamic(quoted)
      }

  private def translate(ast: Ast): Tree =
    IsDynamic(ast) match {
      case false => translateStatic(ast)
      case true  => translateDynamic(ast)
    }

  private implicit val tokenLiftable: Liftable[Token] = Liftable[Token] {
    case ScalarTagToken(lift) =>
      q"io.spill.idiom.ScalarTagToken(${lift: Tag})"
    case QuotationTagToken(lift) =>
      q"io.spill.idiom.QuotationTagToken(${lift: Tag})"
    case StringToken(string) => q"io.spill.idiom.StringToken($string)"
    case ScalarLiftToken(lift) =>
      q"io.spill.idiom.ScalarLiftToken(${lift: Lift})"
    case Statement(tokens) =>
      q"io.spill.idiom.Statement(scala.List(..$tokens))"
    case SetContainsToken(a, op, b) =>
      q"io.spill.idiom.SetContainsToken($a, $op, $b)"
  }

  private def translateStatic(ast: Ast): Tree = {
    idiomAndNamingStatic match {
      case Success((idiom, naming)) =>
        val (normalizedAst, statement) = idiom.translate(ast)(naming)

        val (string, _) =
          ReifyStatement(
            idiom.liftingPlaceholder,
            idiom.emptySetContainsToken,
            statement,
            forProbing = true
          )

        ProbeStatement(idiom.prepareForProbing(string), c)

        c.query(string, idiom)

        q"($normalizedAst, ${statement: Token})"
      case Failure(ex) =>
        c.info(
          s"Can't translate query at compile time because the idiom and/or the naming strategy aren't known at this point."
        )
        translateDynamic(ast)
    }
  }

  private def translateDynamic(ast: Ast): Tree = {
    c.info("Dynamic query")
    q"""
      val (idiom, naming) = ${idiomAndNamingDynamic}
      idiom.translate($ast)(naming)
    """
  }

  private def idiomAndNaming = {
    val (idiom :: n :: _) =
      c.prefix.actualType
        .baseType(c.weakTypeOf[Context[Idiom, NamingStrategy]].typeSymbol)
        .typeArgs
    (idiom, n)
  }

  private def idiomAndNamingDynamic =
    q"(${c.prefix}.idiom, ${c.prefix}.naming)"

  private def idiomAndNamingStatic = {
    val (idiom, naming) = idiomAndNaming
    for {
      idiom <- LoadObject[Idiom](c)(idiom)
      naming <- LoadNaming.static(c)(naming)
    } yield {
      (idiom, naming)
    }
  }
}
