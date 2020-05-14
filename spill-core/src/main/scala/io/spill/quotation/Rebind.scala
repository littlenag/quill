package io.spill.quotation

import scala.reflect.macros.whitebox.Context
import io.spill.ast.Ast
import io.spill.ast.Function
import io.spill.ast.FunctionApply
import io.spill.ast.Ident
import io.spill.ast.QuotedReference

object Rebind {

  def apply(
    c: Context
  )(tree: c.Tree, ast: Ast, astParser: c.Tree => Ast): Option[Ast] = {
    import c.universe.{Function => _, Ident => _, _}

    def toIdent(s: Symbol) =
      Ident(s.name.decodedName.toString)

    def paramIdents(method: MethodSymbol) =
      method.paramLists.flatten.map(toIdent)

    def placeholder(t: Tree): Tree =
      q"null.asInstanceOf[${t.tpe}]"

    tree match {
      case q"$conv($orig).$m[..$t](...$params)" =>
        val convMethod = conv.symbol.asMethod
        val origIdent = paramIdents(convMethod).head
        val paramsIdents = paramIdents(convMethod.returnType.member(m).asMethod)
        val paramsAsts = params.flatten.map(astParser)
        val reifiedTree = q"$conv(${placeholder(orig)}).$m[..$t](...${params
          .map(_.map(placeholder(_)))})"
        val function =
          QuotedReference(reifiedTree, Function(origIdent :: paramsIdents, ast))
        val apply = FunctionApply(function, astParser(orig) :: paramsAsts)
        Some(apply)
      case _ =>
        None
    }
  }
}
