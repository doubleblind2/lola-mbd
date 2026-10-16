package unaa_lola

import unaa_lola.util.Common.OInterval
import unaa_lola.Value.{Type, Real, Bool, Enum, Value, RealValue, BoolValue, EnumValue}
import unaa_lola.util.Common
import unaa_lola.util.Errors

/**
 * Contains classes which are building the AST of the specification
 */

object AST {

  /**
    * General specification type all concrete specifications inherit from
    */
  sealed trait Specification {
    // TODO maybe make inputs also set of identifiers instead of map
    val inputs: Map[String, Type]
    val definitions: Map[Identifier, Expression]
    val outputs: Seq[Identifier]
    val internal: Set[String]
    val components: Set[String]
    val linkedStreams: Map[Identifier, Identifier]
    val samplingRates: Map[Identifier, (Double, Double, Double)]

    /**
      * The set of all stream identifiers on the left-hand side (i.e. input, output and intermediate streams)
      */
    val identifiers = (definitions.keys ++ inputs.map{case (n, t) => Identifier(n, t)}).toSet

    /**
      * The subset of input streams that are not internal
      */
    val nonInternalInputs = inputs.removedAll(internal)
    
    // TODO implement proper toString function for EnumFreeSpec and remove this
    /** @inheritdoc */
    override def toString: String = inputs.map { case (i, t) => s"Input $i : Stream[$t]" }.mkString("\n") + "\n" +
      definitions.map { case (i, e) => s"$i = $e" }.mkString("\n") + "\n" +
      outputs.map { i => s"Output $i" }.mkString("\n")

    /**
      * Determines all identifiers referenced in the specification with a given offset
      *
      * @param offset The offset of the identifers we are interested in
      * @return The set of all identifiers refernced with the given offset
      */
    def crawlOffsets(offset: Int): Set[Identifier] = {
      def crawlOffsetsExp(exp: Expression): Set[Identifier] = {
        exp match {
          case Application(_, exps) => exps.flatMap(crawlOffsetsExp).toSet
          case Offset(id, o, _) if o == offset => Set(id)
          case _ => Set()
        }
      }

      definitions.values.flatMap(crawlOffsetsExp).toSet
    }

    /**
      * Determines the actual type of a stream of an enum type if it was previously encoded using a real stream
      *
      * @param stream The identifier of the encoding stream
      * @return The enum type of the encoded stream, None if the stream wasn't encoded
      */
    def getEncodedEnum(stream: Identifier): Option[Enum]

  }

  /**
   * Concrete specification whose enumerations are not abstracted by reals
   * 
   * @param inputs Map of input streams (name and type)
   * @param definitions Map of intermediate (defined) streams (name and corresponding expression)
   * @param outputs Set of stream identifiers for output streams
   * @param internal Subset of input streams, that will not be read but are always assumed to be entirely unknown
   * @param enumerations Set of enum types
   * @param samplingRates Maps a subset of real outputs to 3-tuples defining that those streams shall be sampled on output, i.e. returned as [[RealSetValue]]s instead of of [[RealRange]]s.
   *                      For a 3-tuple (f,t,b) f is the start point, t the end point and b the rate itself, i.e. the space beween each two values.
   */
  case class ExactSpecification(inputs: Map[String, Type], 
                                definitions: Map[Identifier, Expression], 
                                outputs: Seq[Identifier], 
                                internal: Set[String], 
                                components: Set[String],
                                enumerations: Set[Enum], 
                                linkedStreams: Map[Identifier, Identifier], 
                                samplingRates: Map[Identifier, (Double, Double, Double)]) extends Specification {

    override def getEncodedEnum(stream: Identifier): Option[Enum] = None

    /**
      * Replaces all enum streams in the specification with real streams
      *
      * @return An [[EnumFreeSpecification]] containing the original spec (for decoding purposes) and the updated streams
      */
    def replaceEnumsWithReals: EnumFreeSpecification = {
      // Replaces enum ids with real ids and enum constants with their corresponding real representative
      def crawlExpression(exp: Expression): Expression = {
        exp match {
          case Application(op, exps) => Application(op, exps.map(crawlExpression))
          case Offset(id, offset, default: EnumValue) => Offset(crawlExpression(id).asInstanceOf[Identifier], offset, default.realRepresentative)
          case Identifier(name, _: Enum) => Identifier(name, Real)
          case Constant(v: EnumValue) => Constant(v.realRepresentative)
          case _ => exp
        }
      }

      // Computes conjunction (CNF) over all enum streams specifying which real values are possible
      def getEnumAssumptions: Option[Application] = {
        // Disjunction over all possible values (real representatives) of a single stream
        def encodeAssumption(idName: String, enumType: Enum): Application = {
          enumType.constants.map {
            const =>
              val realRep = Constant(EnumValue(const, enumType).realRepresentative)
              Application(AST.eq, Seq(Identifier(idName, Real), realRep))
          }.reduce[Application]{case (a, b) => Application(or, Seq(a, b))}
        }

        // only if there is any enum stream
        if (identifiers.exists(_.getType.isInstanceOf[Enum])) {
          val app = identifiers.filter(_.getType.isInstanceOf[Enum]).map {
            case Identifier(name, e: Enum) => encodeAssumption(name, e)
          }.reduce[Application]{case (a, b) => Application(and, Seq(a, b))}
          Some(app)
        } else {
          None
        }
      }
      
      val newInputs = inputs.mapValues {
        case t: Enum => Real
        case t => t
      }.toMap

      var newDefs = definitions.map {
        case (Identifier(name, _: Enum), exp) => (Identifier(name, Real) -> crawlExpression(exp))
        case (id, exp) => (id -> crawlExpression(exp))
      }

      val newOutputs = outputs.map {
        case Identifier(name, _: Enum) => Identifier(name, Real)
        case id => id
      }

      // add assumptions for enum identifiers
      val currentAss = newDefs.get(Common.assStream)
      val enumAss = getEnumAssumptions
      (currentAss, enumAss) match {
        case (Some(a1), Some(a2)) => newDefs += (Common.assStream -> Application(and, Seq(a1, a2)))
        case (Some(a), None) => newDefs += (Common.assStream -> a)
        case (None, Some(a)) => newDefs += (Common.assStream -> a)
        case (None, None) => 
      }

      EnumFreeSpecification(this, newInputs, newDefs, newOutputs, internal, components, linkedStreams, samplingRates)
    }

    override def toString: String = {
      // TODO ensure correct upper/lower case (or change the parser be more flexible)

      def typeStr(t: Type): String = t match {
        case Bool => "Bool"
        case Real => "Real"
        case Enum(name, _) => name
      }

      def numStr(v: Double): String = {
        require(v.isFinite && !v.isNaN, s"Cannot represent $v as a number literal")
        if (v.toLong == v) {
          v.toLong.toString
        } else {
          java.math.BigDecimal.valueOf(v).toPlainString
        }
      }

      def valStr(v: Value): String = v match {
        case BoolValue(b) => b.toString
        case RealValue(r) => numStr(r)
        case EnumValue(name, _) => name
        case other => throw new IllegalArgumentException(s"Value '$other' has no literal representation")
      }

      def exprStr(expr: Expression): String = {
        expr match {
          case Identifier(name, _) => name
          case Constant(v) => valStr(v)
          case Offset(id, offset, default) => s"${id.name}[$offset, ${valStr(default)}]"
          case Application(op, exps) => op match {
            case AST.not => s"!${exprStr(exps(0))}"
            case AST.ite => s"if ${exprStr(exps(0))} then ${exprStr(exps(1))} else ${exprStr(exps(2))}"
            case AST.times if exps(0) == Constant(RealValue(-1)) => s"-${exprStr(exps(1))}"
            case AST.plus => exps(1) match {
              case Application(AST.times, Seq(Constant(RealValue(-1)), rhs)) => s"(${exprStr(exps(0))} - ${exprStr(rhs)})"
              case _ => s"(${exprStr(exps(0))} + ${exprStr(exps(1))})"
            }
            case _ => s"(${exprStr(exps(0))} ${op.toString} ${exprStr(exps(1))})"
          }
        }
      }

      val sb = new StringBuilder()

      enumerations.toSeq.sortBy(_.name).foreach {e =>
        sb.append(s"enum ${e.name} { ${e.constants.mkString(", ")} };\n")  
      }
      if (enumerations.nonEmpty) sb.append("\n")
      
      val regularInputs = inputs.removedAll(internal).toSeq.sortBy(_._1)
      regularInputs.foreach { case (name, typ) =>
        sb.append(s"in $name: ${typeStr(typ)};\n")  
      }
      if (regularInputs.nonEmpty) sb.append("\n")

      val regularInternals = internal.removedAll(components).toSeq.sorted
      regularInternals.foreach { name =>
        sb.append(s"internal $name: ${typeStr(inputs(name))};\n")
      }
      if (regularInternals.nonEmpty) sb.append("\n")

      components.toSeq.sorted.foreach { name =>
        sb.append(s"comp $name;\n")  
      }
      if (components.nonEmpty) sb.append("\n")

      val sortedDefs = definitions.keys.toSeq.sortBy(_.name)
      sortedDefs.foreach { id =>
        sb.append(s"def ${id.name}: ${typeStr(id.getType)};\n")  
      }
      if (sortedDefs.nonEmpty) sb.append("\n")
      sortedDefs.foreach {id =>
        sb.append(s"${id.name} = ${exprStr(definitions(id))};\n")  
      }
      if (sortedDefs.nonEmpty) sb.append("\n")

      outputs.foreach { id =>
        samplingRates.get(id) match {
          case Some((from, to, by)) => sb.append(s"out ${id.name} from ${numStr(from)} to ${numStr(to)} by ${numStr(by)};\n")
          case None => sb.append(s"out ${id.name};\n")
        }  
      }
      if (outputs.nonEmpty) sb.append("\n")

      linkedStreams.toSeq.sortBy(_._1.name).foreach { case (id1, id2) =>
        sb.append(s"${id1.name} linked with ${id2.name};\n")  
      }
      
      sb.toString.stripTrailing
    }
    
  }

  /**
    * Concrete specification acting as a container for an [[ExactSpecification]] but without enumerations.
    *
    * @param inputs Map of input streams (name and type)
    * @param definitions Map of intermediate (defined) streams (name and corresponding expression)
    * @param outputs Set of stream identifiers for output streams
    * @param internal Subset of input streams, that will not be read but are always assumed to be entirely unknown
    * @param samplingRates Maps a subset of real outputs to 3-tuples defining that those streams shall be sampled on output, i.e. returned as [[RealSetValue]]s instead of of [[RealRange]]s.
    *                      For a 3-tuple (f,t,b) f is the start point, t the end point and b the rate itself, i.e. the space beween each two values.
    */
  case class EnumFreeSpecification(orgSpec: ExactSpecification, 
                                   inputs: Map[String, Type], 
                                   definitions: Map[Identifier, Expression], 
                                   outputs: Seq[Identifier], 
                                   internal: Set[String], 
                                   components: Set[String],
                                   linkedStreams: Map[Identifier, Identifier], 
                                   samplingRates: Map[Identifier, (Double, Double, Double)]) extends Specification {
    override def getEncodedEnum(stream: Identifier): Option[Enum] = {
      if (orgSpec.inputs.contains(stream.name)) {
        // inputs
        orgSpec.inputs(stream.name) match {
          case e: Enum => Some(e)
          case _ => None
        }
      } else {
        // non-inputs
        orgSpec.definitions.keySet.find(_.name == stream.name) match {
          case Some(Identifier(_, e: Enum)) => Some(e)
          case Some(Identifier(_, _)) => None
          case None => throw new Errors.InternalException(s"Stream $stream unspecified", panic = true)
        }
      }
    }
  }


  /**
   * Basic expressions (application of an operator to a stream, offset operator, constant stream)
   */
  sealed trait Expression {
    def getType: Type
  }

  /**
   * Application of an operation on a bunch of streams
   * @param op Operator which is executed
   * @param exps Sequence of streams on which the operator is applied
   */
  case class Application(op: Operator, exps: Seq[Expression]) extends Expression {
    override def toString: String = if (exps.size == 2) s"(${exps(0)}) ${op} (${exps(1)})" else s"${op}(${exps.map(_.toString).mkString(", ")})"

    override def getType: Type = 
      op match {
        case AST.ite => exps(1).getType
        case _ => op.returnType
      }
  }

  /**
   * Offset operator
   * @param id Identifier on which offset is applied
   * @param offset Offset to current timestamp that is requested
   * @param default Default value if the requested timestamp does not exist (i.e. negative or too large)
   */
  case class Offset(id: Identifier, offset: Int, default: Value) extends Expression {
    override def toString: String = s"($id)[${offset}, ${default}]"

    override def getType: Type = id.getType
  }

  /**
   * A stream with a constant value on all timestamps
   * @param v The value of the stream
   */
  case class Constant(v: Value) extends Expression {
    override def toString: String = v.toString

    override def getType: Type = v.getType
  }


  /**
   * Common superclass for identifiers
   * @param name Name of the identifier
   * @param typ Type of the identifier
   */
  class SupIdentifier(val name: String, val typ: Type) extends Expression {
    override def toString: String = name

    override def getType: Type = typ
  }

  /**
   * A Identifier is used to identify streams of the specification but also for unknown values during evaluation
   * @param name Name of the identifier
   * @param typ Type of the identifier
   */
  case class Identifier(override val name: String, override val typ: Type) extends SupIdentifier(name, typ)

  /**
    * A SymbolIdentifier is used for unknown values of streams during the evaluation offset by a certain amount of time steps
    *
    * @param id The underlying stream this symbol represents
    * @param ts The timestamp of the symbol, representing the stream id offset by ts steps
    */
  case class SymbolIdentifier(baseId: Identifier, ts: Int) extends SupIdentifier(s"${baseId.name}^${ts}", baseId.typ);
  

  //Functions

  /**
   * Super class for a bunch of operators that can be applied to streams:
   */
  sealed trait Operator {
    def returnType: Type
  }

  case object eq extends Operator {
    override def toString = "=="

    override def returnType: Type = Bool
  }

  case object ne extends Operator {
    override def toString = "!="

    override def returnType: Type = Bool
  }

  case object and extends Operator {
    override def toString = "&&"

    override def returnType: Type = Bool
  }

  case object or extends Operator {
    override def toString = "||"

    override def returnType: Type = Bool
  }

  case object not extends Operator {
    override def toString = "!"

    override def returnType: Type = Bool
  }

  case object iff extends Operator {
    override def toString = "<->"

    override def returnType: Type = Bool
  }

  case object imp extends Operator {
    override def toString = "->"

    override def returnType: Type = Bool
  }

  case object plus extends Operator {
    override def toString = "+"

    override def returnType: Type = Real
  }

  case object times extends Operator {
    override def toString = "*"

    override def returnType: Type = Real
  }

  case object lt extends Operator {
    override def toString = "<"

    override def returnType: Type = Bool
  }

  case object gt extends Operator {
    override def toString = ">"

    override def returnType: Type = Bool
  }

  case object le extends Operator {
    override def toString = "<="

    override def returnType: Type = Bool
  }

  case object ge extends Operator {
    override def toString = ">="

    override def returnType: Type = Bool
  }

  case object ite extends Operator {
    override def toString = "ite"

    override def returnType: Type = Real //TODO ideally this should be variable. not a problem though since AST.Application still returns the correct type
  }

}
