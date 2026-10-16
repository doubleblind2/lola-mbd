package unaa_lola

import unaa_lola.AST.{Expression, Identifier, Specification, ExactSpecification, EnumFreeSpecification}
import unaa_lola.Value.Type

/**
 * Class for flattening a specification (i.e., replacing all offsets greater 1 or lower -1 by a sequence of +1 and -1
 * offsets assigned to new temporary streams).
 */
class Flattener {

  private var id: Int = -1

  // Obtain unused stream identifier.
  private def freshId(tpe: Type): Identifier = {id += 1; Identifier("$" + id, tpe)}

  private def flattenExpression(e: Expression, extract: Set[Identifier], inline: Set[Identifier], defs: Map[Identifier, Expression]): (Expression, Map[Identifier, Expression]) = {
    e match {
      case AST.Application(op, exps) =>
        val nexp = exps.map(flattenExpression(_, extract, inline, defs))
        (AST.Application(op, nexp.map(_._1)), nexp.foldLeft[Map[Identifier, Expression]](Map()){case (c, n) => c ++ n._2})
      case AST.Offset(exp, offset, default) if offset == 0 => flattenExpression(exp, extract, inline, defs)
      case AST.Offset(id: Identifier, offset, default) if (offset == -1 || offset == 1) && extract.contains(id) =>
        val fid = freshId(id.typ)
        (AST.Offset(fid, offset, default), Map(fid -> id))
      case AST.Offset(exp, offset, default) =>
          val sgn = offset / Math.abs(offset)
          var exps : Map[Identifier, Expression] = Map()
          val ce = flattenExpression(exp, extract, inline, defs)

          exps = ce._2
          val inner = ce._1 match {
            case id: Identifier if !extract.contains(id) =>
              id
            case _ =>
              val id = freshId(exp.getType)
              exps += (id -> ce._1)
              id
          }

          val newExp =
              (1 to Math.abs(offset)).foldLeft[Expression](inner) {
                case (id, n) =>
                  val no = AST.Offset(id.asInstanceOf[Identifier], sgn, default)

                  if (n != Math.abs(offset)) {
                    val nid = freshId(exp.getType)
                    exps += (nid -> no)
                    nid
                  } else {
                    no
                  }
              }

        (newExp, exps)
      case i: Identifier if inline.contains(i) =>
        flattenExpression(defs(i), extract, inline, defs)

      case _ => (e, Map())
    }
  }

  private def inlineAnalysis(s: Specification): Set[Identifier] = {
    def crawlExp0Use(e: Expression): Seq[Identifier] = {
      e match {
        case AST.Application(_, exps) => exps.flatMap(crawlExp0Use)
        case AST.Offset(id, 0, _) => Seq(id)
        case id: Identifier => Seq(id)
        case _ => Seq()
      }
    }

    def crawlExpN0Use(e: Expression): Seq[Identifier] = {
      e match {
        case AST.Application(_, exps) => exps.flatMap(crawlExpN0Use)
        case AST.Offset(id, o, _) if o != 0 => Seq(id)
        case _ => Seq()
      }
    }

    var noInline: Set[Identifier] = Set()
    var cnt: Map[Identifier, Int] = Map()

    s.definitions.foreach{
      case (_, e) =>
        noInline ++= crawlExpN0Use(e)
        val an = crawlExp0Use(e)
        an.foreach{ i =>
          cnt = cnt + (i -> (cnt.getOrElse(i, 0) + 1))
        }
    }

    cnt.filter{case (_,n) => n <= 7}.keySet -- noInline -- s.inputs.map{case (n,t) => Identifier(n,t)} -- s.outputs -- s.linkedStreams.values
  }

  /**
   * Flattens a specification, i.e., offsets only reference streams by -1, 0 or +1.
   * @param s Specification to be flattened
   * @param noInputOffsets Do not flatten input streams
   * @return The flattened specification
   */
  def flattenSpec(s: Specification, noInputOffsets: Boolean = false) : Specification = {
    def flattenExactDef(es: ExactSpecification): ExactSpecification = {
      val inp = s.inputs.map{case (n, t) => Identifier(n, t)}.toSet
      val inline = inlineAnalysis(s)
      es.copy(definitions = es.definitions.map{case (id, exp) => (id, flattenExpression(exp, if (noInputOffsets) inp else Set(), inline, s.definitions))}.flatMap{case (id, (exp, map)) => map + (id -> exp)}.filter{case (i,_) => !inline.contains(i)})
    }

    s match {
      case spec: ExactSpecification => flattenExactDef(spec)
      case spec: EnumFreeSpecification => flattenExactDef(spec.orgSpec).replaceEnumsWithReals // flatten original spec and replace enums again
    }
  }

}
