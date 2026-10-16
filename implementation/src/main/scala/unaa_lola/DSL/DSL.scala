package unaa_lola.DSL

import unaa_lola.AST._
import unaa_lola.Value._
import unaa_lola.AST
import unaa_lola.Frontend.SpecContainer.emptySpecContainer
import unaa_lola.Frontend.SpecContainer
import unaa_lola.util.Common.assStream

object DSL {
  // TODO change object to 'EmptySpec' and use apply function
  /**
    * Entry point to build a specification.
    *
    * @return A builder for an empty specification.
    */
  def spec: SpecBuilder = SpecBuilder(emptySpecContainer)
}

/**
  * Contains functions to incrementally build a specification.
  *
  * @param container The underlying [[SpecContainer]] representing a partially built specification
  */
case class SpecBuilder(container: SpecContainer) {

  private def requireUndeclared(name: String): Unit = 
    require(!container.isDeclared(name), s"Stream '$name' is already declared")
  
  private def requireDeclared(name: String): Unit = 
    require(container.isDeclared(name), s"Stream '$name' has not been declared yet")

  /**
    * Finalizes the specification.
    *
    * @return The fully build specification
    */
  def build: ExactSpecification = {
    if (container.unassignedDefs.nonEmpty) {
      throw new IllegalStateException(s"Streams declared but not assigned an expression: ${container.unassignedDefs.map(d => s"'$d'").mkString(", ")}")
    }
    container.partialSpec
  }

  /**
    * Adds input streams.
    *
    * @param ids Identifiers for the input streams
    * @return The updated builder
    */
  def input(ids: Identifier*): SpecBuilder = ids.foldLeft(this) {
    case (curr, id) =>
      requireUndeclared(id.name)
      SpecBuilder(curr.container.addInput(id.name, id.typ))
  }

  /**
    * Adds internal streams.
    *
    * @param ids Identifiers for the internal streams
    * @return The updated builder
    */
  def internal(ids: Identifier*): SpecBuilder = ids.foldLeft(this) {
    case (curr, id) =>
      requireUndeclared(id.name)
      SpecBuilder(container.addInternal(id.name, id.typ))
  }

  /**
    * Adds components.
    *
    * @param ids Boolean identifiers for the components
    * @return The updated builder
    */
  def component(ids: Identifier*): SpecBuilder = ids.foldLeft(this) {
    case (curr, id) =>  
      requireUndeclared(id.name)
      require(id.getType == Bool, s"Identifier '$id' cannot be delcared a component because it does have type Bool")
      SpecBuilder(container.addComponent(id.name))
  }

  /**
    * Defines a stream with a defining expression.
    *
    * @param id Identifier for the stream
    * @param expr It's defined expression
    * @return The updated builder
    */
  def define(id: Identifier, expr: Expression): SpecBuilder = {
    requireUndeclared(id.name)
    require(expr.getType == id.getType, s"Type mismatch for '{$id.name}': declared ${id.getType} but expression has type ${expr.getType}")
    SpecBuilder(container.addUnassignedDefinition(id.name, expr.getType).assign(id.name, expr))
  }

  /**
    * Declares a defined stream but does not yet define its defining expression. (Can be necessary for crosswise recursion)
    * [[assign]] must be called for this stream before [[build]] is called.
    *
    * @param id Identifier for the stream
    * @return The updated builder
    */
  def define(id: Identifier): SpecBuilder = {
    requireUndeclared(id.name)
    SpecBuilder(container.addUnassignedDefinition(id.name, id.typ))
  }

  /**
    * Assigns a defining expression to a previously declared defined stream.
    *
    * @param id The already declared defined stream
    * @param expr Its assigned expression
    * @return The updated builder
    */
  def assign(id: Identifier, expr: Expression): SpecBuilder = {
    // TODO maybe check if already assigned, not really necessary though
    requireDeclared(id.name)
    val declaredType = container.idNameToType(id.name) 
    require(declaredType == expr.getType, s"Type mismatch for '{$id.name}': declared $declaredType but expression has type ${expr.getType}")
    SpecBuilder(container.assign(id.name, expr))
  }

  /**
    * Adds the assumption.
    *
    * @param expr The assumption expression
    * @return The updated builder
    */
  def assumption(expr: Expression): SpecBuilder = {
    requireUndeclared(assStream.name)
    require(expr.getType == Bool, s"Assumption must be Boolean but expression '$expr' has type ${expr.getType}")
    define(assStream, expr)
  }

  /**
    * Adds output streams without sampling.
    *
    * @param ids Identifiers for the output streams
    * @return The updated builder
    */
  def output(ids: Identifier*): SpecBuilder = ids.foldLeft(this) {
    case (curr, id) =>
      requireDeclared(id.name)
      SpecBuilder(curr.container.addOutput(id.name, None))
  }

  /**
    * Defines a single output stream with sampling.
    *
    * @param id Identifier for the output stream
    * @param from Start sampling from here
    * @param to End sampling here
    * @param by The sampling step
    * @return The updated builder
    */
  def output(id: Identifier, from: Double, to: Double, by: Double): SpecBuilder = {
    requireDeclared(id.name)
    require(id.typ == Real, s"'${id.name}' cannot be sampled as it is not of type Real")
    require(by > 0, s"'${id.name}' cannot be sampled with non-positive spacing between two values")
    require(from <= to, s"'${id.name}' cannot be sampled if the from value is larger than the to value")
    SpecBuilder(container.addOutput(id.name, Some(from, to, by)))
  }

  /**
    * Defines a new enumeration type.
    *
    * @param enumType The enum type with its name and constants
    * @return The updated builder
    */
  def enum(enumType: Enum): SpecBuilder = {
    val Enum(name, constants) = enumType
    require(!container.occursInEnum(name), s"Enum '$name' already defined")
    require(constants.nonEmpty, s"Enum '$name' must have at least one constant")
    val knownConstants = constants.filter(container.occursInEnum(_))
    require(knownConstants.isEmpty, s"Enum constants ${knownConstants.map(c => s"'$c'").mkString(", ")} already defined in other enums.")
    val duplicates = constants.diff(constants.distinct).distinct
    require(duplicates.isEmpty, s"Enum constants ${knownConstants.map(c => s"'$c'").mkString(", ")} occur multiple times.")
    SpecBuilder(container.addEnum(name, constants))
  }

  /**
    * Links stream1 to Boolean stream2.
    *
    * @param stream1 A stream of any type
    * @param stream2 A Boolean stream
    * @return The updated builder
    */
  def linked(stream1: Identifier, stream2: Identifier): SpecBuilder = {
    requireDeclared(stream1.name)
    requireDeclared(stream2.name)
    require(stream2.typ == Bool, s"Second stream '$stream2' of linking must be of type Bool but has type ${stream2.typ}")
    SpecBuilder(container.addLink(stream1.name, stream2.name))
  }

}


/**
  * Contains (implicit) functions to help building expression when using the DSL.
  */
object ExpressionBuilder {

  /** Implicit casting from double to real constant */
  implicit def const(v: Double): Constant = Constant(RealValue(v))

  /** Implicit casting from boolean to boolean constant */
  implicit def const(v: Boolean): Constant = Constant(BoolValue(v))

  /**
    * An enum constant.
    *
    * @param name Name of the constant
    * @param enumType The enum type
    * @return The built constant
    */
  def const(name: String, enumType: Enum): Constant = {
    require(enumType.constants.contains(name))
    Constant(EnumValue(name, enumType))
  }

  /** Alias for [[Identifier]] */
  def stream(name: String, typ: Type): Identifier = Identifier(name, typ)

  /** Shorthand for a component */
  def comp(name: String): Identifier = Identifier(name, Bool)

  /** Shorthand for if-then-else construction */
  def ite(cond: Expression, thenExpr: Expression, elseExpr: Expression): Expression = 
    Application(AST.ite, Seq(cond, thenExpr, elseExpr))

  // TODO check for redundant definitions
  /** Alias for building an enum type */
  def enum(name: String, constants: String*): Enum = Enum(name, constants)
  
  /** Implicit applications of operands to two expressions */
  implicit class ExprOps(val lhs: Expression) extends AnyVal {
    // TODO type checking with require

    // Comparisons
    def ===(rhs: Expression): Expression = Application(AST.eq, Seq(lhs, rhs))
    def !==(rhs: Expression): Expression = Application(AST.ne, Seq(lhs, rhs))
    def <(rhs: Expression): Expression = Application(AST.lt, Seq(lhs, rhs))
    def >(rhs: Expression): Expression = Application(AST.gt, Seq(lhs, rhs))
    def <=(rhs: Expression): Expression = Application(AST.le, Seq(lhs, rhs))
    def >=(rhs: Expression): Expression = Application(AST.ge, Seq(lhs, rhs))

    // Arithmetic
    def +(rhs: Expression): Expression = Application(AST.plus, Seq(lhs, rhs))
    def *(rhs: Expression): Expression = Application(AST.times, Seq(lhs, rhs))
    def -(rhs: Expression): Expression = Application(AST.plus, Seq(lhs, Application(AST.times, Seq(Constant(RealValue(-1)), rhs))))
    def unary_- : Expression = Application(AST.times, Seq(Constant(RealValue(-1)), lhs))

    // Boolean
    def &&(rhs: Expression): Expression = Application(AST.and, Seq(lhs, rhs))
    def ||(rhs: Expression): Expression = Application(AST.or, Seq(lhs, rhs))
    def unary_! : Expression = Application(AST.not, Seq(lhs))
    def -->(rhs: Expression): Expression = Application(AST.imp, Seq(lhs, rhs))
    def <->(rhs: Expression): Expression = Application(AST.iff, Seq(lhs, rhs))

    // Offset
    def at(offset: Int, default: Constant): Offset = {
      require(lhs.isInstanceOf[Identifier], s"Offset operator only applicable on stream identifiers, got '$lhs'")
      Offset(lhs.asInstanceOf[Identifier], offset, default.v)
    }
    def prev(default: Constant): Offset = at(-1, default)
    def next(default: Constant): Offset = at(1, default)

  }

}