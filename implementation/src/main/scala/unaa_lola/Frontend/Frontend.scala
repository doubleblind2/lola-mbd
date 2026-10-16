package unaa_lola.Frontend

import org.antlr.v4.runtime._
import unaa_lola.Value.{Type, Bool, Real, Enum}
import unaa_lola.{AST, Value}
import unaa_lola.antlr4._
import unaa_lola.antlr4.LolaParser._
import org.antlr.v4.runtime.tree.TerminalNode
import java.util.regex.Pattern
import scala.jdk.CollectionConverters._
import unaa_lola.Frontend.SpecContainer.emptySpecContainer

/**
  * Object containing the general entry point for compiling lola specifications as well as functions used during compilation.
  */
object LolaCompiler {

  /**
    * Entry point to initiate the compilation of a specification.
    *
    * @param code The specification as string
    * @param throwSyntaxErrorsImmediately Set this if the first syntax error during lexing or parsing should be thrown immediately instead of collecting all of them.
    * @return Either an exception or the specification as [[AST.ExactSpecification]]
    */
  def apply(code: String, throwSyntaxErrorsImmediately: Boolean = false): Either[LolaCompilerException, AST.ExactSpecification] = {
    try {
      val prepCode = preprocess(code)
      val errorListener = if (throwSyntaxErrorsImmediately) new ThrowingErrorListener else new CollectingErrorListener

      val lexer = new LolaLexer(CharStreams.fromString(prepCode))
      lexer.removeErrorListeners
      lexer.addErrorListener(errorListener)

      val parser = new LolaParser(new CommonTokenStream(lexer))
      parser.removeErrorListeners
      parser.addErrorListener(errorListener)

      val tree = parser.lola()
      if (!throwSyntaxErrorsImmediately) errorListener.asInstanceOf[CollectingErrorListener].finish()

      val specVisitor = new LolaSpecVisitor
      specVisitor.visit(tree)
      Right(specVisitor.toSpec)
    } catch {
      case t: LolaCompilerException => Left(t)
    }
  }
  
  /**
    * Preprocesses the specification by resolving macros.
    *
    * @param code The original specification
    * @return The macro-less specification
    */
  private def preprocess(code: String): String = {
    val remComments = code.split("\n").map(_.split("#")(0)).mkString("\n")

    val pattern = """@([0-9]+):([0-9]+)@([^@]*)@"""
    var out = remComments

    val p = Pattern.compile(pattern)
    val p2 = Pattern.compile("""\{(-?[0-9]+)\}""")
    var m = p.matcher(out)

    while(m.find()) {
      val replacement = (m.group(1).toInt to m.group(2).toInt).map{
        i =>
          var rep = m.group(3)
          var m2 = p2.matcher(rep)

          while(m2.find()) {
            rep = rep.replace(m2.group(0), s"${i + m2.group(1).toInt}")
            m2 = p2.matcher(rep)
          }

          rep
      }.mkString("\n\n")

      out = out.replaceFirst(pattern, replacement)
      m = p.matcher(out)
    }

    out

  }
  
  /**
    * Extracts the location, i.e. line and column, of an ANTLR context.
    *
    * @param ctx ANTRL context containing the information
    * @return The location
    */
  def getLocation(ctx: ParserRuleContext): Location = Location(ctx.start.getLine, ctx.start.getCharPositionInLine)

  /**
    * Map translating ANTLR tokens for operators into [[AST]] operators.
    * for MINUS use AST.plus and AST.times
    */
  val parserOpToASTOp = Map(
    NOT -> AST.not,
    EQ -> AST.eq,
    NEQ -> AST.ne,
    AND -> AST.and,
    OR -> AST.or,
    IMPL -> AST.imp,
    IFF -> AST.iff,
    TIMES -> AST.times,
    PLUS -> AST.plus,
    LT -> AST.lt,
    GT -> AST.gt,
    LEQ -> AST.le,
    GEQ -> AST.ge,
  )

}

object SpecContainer {
  def emptySpecContainer = SpecContainer(AST.ExactSpecification(Map(), Map(), Seq(), Set(), Set(), Set(), Map(), Map()), Set(), Map(), Map())
}

/**
  * Container for a partially built specification.
  * Also provides functions for incrementally building said specification.
  * Neccessary error checks and handling must be done separately.
  *
  * @param partialSpec The partial specification itself
  * @param unassignedDefs Set of streams that have been declared but not yet assigned an expression
  * @param idNameToType Maps each stream id to its type
  * @param enumConstToType Maps each enum constant to its enum type
  */
case class SpecContainer(partialSpec: AST.ExactSpecification, unassignedDefs: Set[AST.Identifier], idNameToType: Map[String, Type], enumConstToType: Map[String, Enum]) {

  /**
    * Determines whether a stream is declared.
    *
    * @param idName The id of the stream
    * @return True if declared
    */
  def isDeclared(idName: String): Boolean = idNameToType.keySet.contains(idName)

  /**
    * Determines if an enum constant or enum type is defined.
    *
    * @param name The enum constant or enum type
    * @return True if defined
    */
  def occursInEnum(name: String): Boolean = partialSpec.enumerations.flatMap(e => e.constants.toSet + e.name).contains(name)

  /**
    * Adds a new input stream.
    *
    * @param idName The name of the input stream
    * @param typ The type of the input stream
    * @return The new partial spec
    */
  def addInput(idName: String, typ: Type): SpecContainer = {
    val newInputs = partialSpec.inputs + (idName -> typ)
    val newSpec = partialSpec.copy(inputs = newInputs)
    val newTypeMap = idNameToType + (idName -> typ)
    copy(partialSpec = newSpec, idNameToType = newTypeMap)
  }

  /**
    * Adds a new internal stream.
    *
    * @param idName The name of the internal stream
    * @param typ The type of the internal stream
    * @return The new partial spec
    */
  def addInternal(idName: String, typ: Type): SpecContainer = {
    val tmpContainer = addInput(idName, typ)
    val newSpec = tmpContainer.partialSpec.copy(internal = tmpContainer.partialSpec.internal + idName)
    tmpContainer.copy(partialSpec = newSpec)
  }

  def addComponent(idName: String): SpecContainer = {
    val tmpContainer = addInternal(idName, Bool)
    val newSpec = tmpContainer.partialSpec.copy(components = tmpContainer.partialSpec.components + idName)
    tmpContainer.copy(partialSpec = newSpec)
  }

  /**
    * Adds a new non-input stream (yet without assigned expression).
    *
    * @param idName The name of the stream
    * @param typ The type of the stream.
    * @return The new partial spec
    */
  def addUnassignedDefinition(idName: String, typ: Type): SpecContainer = {
    val newUnassignedDefs = unassignedDefs + AST.Identifier(idName, typ)
    val newTypeMap = idNameToType + (idName -> typ)
    copy(unassignedDefs = newUnassignedDefs, idNameToType = newTypeMap)
  }

  /**
    * Assigns an already declared stream an expression.
    *
    * @param idName The name of the stream
    * @param expr The assigned expression
    * @return The new partial spec
    */
  def assign(idName: String, expr: AST.Expression): SpecContainer = {
    val id = AST.Identifier(idName, expr.getType)
    val newDefs = partialSpec.definitions + (id -> expr)
    val newSpec = partialSpec.copy(definitions = newDefs)
    val newUnassignedDefs = unassignedDefs - id
    copy(partialSpec = newSpec, unassignedDefs = newUnassignedDefs)
  }

  /**
    * Declares a stream to be an output stream.
    *
    * @param idName The name of the stream
    * @return The new partial spec
    */
  def addOutput(idName: String, rateTuple: Option[(Double, Double, Double)]): SpecContainer = {
    val id = AST.Identifier(idName, idNameToType(idName))
    val newOutputs = partialSpec.outputs :+ id
    val newRates = if (rateTuple.isEmpty) partialSpec.samplingRates else partialSpec.samplingRates + (id -> rateTuple.get)
    val newSpec = partialSpec.copy(outputs = newOutputs, samplingRates = newRates)
    copy(partialSpec = newSpec)
  }

  /**
    * Adds an enum type with its constants.
    *
    * @param enumName The name of the enum type
    * @param constants The sequence of constant names
    * @return The new partial spec
    */
  def addEnum(enumName: String, constants: Seq[String]): SpecContainer = {
    val newEnum = Enum(enumName, constants)
    val newEnums = partialSpec.enumerations + newEnum
    val newSpec = partialSpec.copy(enumerations = newEnums)
    val newEnumMap = enumConstToType ++ constants.map(const => (const -> newEnum))
    copy(partialSpec = newSpec, enumConstToType = newEnumMap)
  }

  /**
    * Links a stream leftId to a Boolean stream rightId
    *
    * @param leftId the left stream
    * @param rightId the Boolean right stream
    * @return The new partial spec
    */
  def addLink(leftId: String, rightId: String): SpecContainer = {
    val newLink = (AST.Identifier(leftId, idNameToType(leftId)), AST.Identifier(rightId, Bool))
    val newSpec = partialSpec.copy(linkedStreams = partialSpec.linkedStreams + newLink)
    copy(partialSpec = newSpec)
  }

}

/**
  * ANTLR visitor for compiling the entire specification.
  * Call toSpec to retrieve the finished specification.
  */
class LolaSpecVisitor extends LolaBaseVisitor[Unit] {

  private var specContainer = emptySpecContainer

  private def isDeclared(idName: String): Boolean = specContainer.isDeclared(idName)
  private def isUnassigned(idName: String): Boolean = specContainer.unassignedDefs.map(_.name).contains(idName)
  private def isInput(idName: String): Boolean = specContainer.partialSpec.inputs.contains(idName)
  private def isOutput(idName: String): Boolean = specContainer.partialSpec.outputs.map(_.name).contains(idName)
  private def occursInEnum(name: String): Boolean = specContainer.occursInEnum(name)
  private def getIdType(idName: String): Type = specContainer.idNameToType(idName)

  private def exprVisitor = LolaExprVisitor(specContainer)
  private def typeVisitor = LolaTypeVisitor(specContainer)

  /**
    * Retrieves the compiled specification.
    *
    * @return The compiled specification
    */
  def toSpec: AST.ExactSpecification = specContainer.partialSpec

  override def visitLola(ctx: LolaContext): Unit = {
    ctx.statement.forEach(visit)
    if (specContainer.unassignedDefs.nonEmpty) {
      throw LolaDeclarationException(Location(ctx.stop.getLine, ctx.stop.getCharPositionInLine), 
        s"the following id's are declared but have not been assigned an expression: ${specContainer.unassignedDefs.mkString(", ")}")
    }
  }

  override def visitStatement(ctx: StatementContext): Unit = visit(ctx.getChild(0))

  override def visitInput(ctx: InputContext): Unit = {
    val idName = ctx.id.getText
    val idType = typeVisitor.visit(ctx.exprType)
    if (isDeclared(idName)) {
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' is already declared")
    }
    specContainer = specContainer.addInput(idName, idType)
  }

  override def visitInternal(ctx: InternalContext): Unit = {
    val idName = ctx.id.getText
    val idType = typeVisitor.visit(ctx.exprType)
    if (isDeclared(idName)) {
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' is already declared")
    }
    specContainer = specContainer.addInternal(idName, idType)
  }

  override def visitComp(ctx: CompContext): Unit = {
    val idName = ctx.id.getText
    if (isDeclared(idName)) {
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' is already declared")
    }
    specContainer = specContainer.addComponent(idName)
  }

  override def visitDefinition(ctx: DefinitionContext): Unit = {
    val idName = ctx.id.getText
    val idType = typeVisitor.visit(ctx.exprType)
    if (isDeclared(idName)) {
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' is already declared")
    }
    specContainer = specContainer.addUnassignedDefinition(idName, idType)
    
  }

  override def visitAssignment(ctx: AssignmentContext): Unit = {
    val idName = ctx.id.getText

    if (!isDeclared(idName)) {
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' is not yet declared")
    }
    if (!isUnassigned(idName)) {
      if (isInput(idName)) {
        throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' is declared as a non-defined stream and can thus not be assigned an expression")
      } else {
        throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' is already assigned an expression")
      }
    }
  
    val expr = exprVisitor.visit(ctx.expr)
    if (expr.getType != getIdType(idName)) {
      throw LolaTypeException(LolaCompiler.getLocation(ctx), s"id '${idName}' is of type ${getIdType(idName)} but its assigned expression is of type ${expr.getType}")
    }
    specContainer = specContainer.assign(idName, expr)
  }

  override def visitDefAssign(ctx: DefAssignContext): Unit = {
    val idName = ctx.id.getText
    val idType = typeVisitor.visit(ctx.exprType)
    if (isDeclared(idName)) {
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' is already declared")
    }
    
    specContainer = specContainer.addUnassignedDefinition(idName, idType)

    val expr = exprVisitor.visit(ctx.expr)
    if (expr.getType != idType) {
      throw LolaTypeException(LolaCompiler.getLocation(ctx), s"id '${idName}' is of type $idType but its assigned expression is of type ${expr.getType}")
    }
    
    specContainer = specContainer.assign(idName, expr)
  }

  override def visitOutput(ctx: OutputContext): Unit = {
    val idName = ctx.id.getText
    if (!isDeclared(idName)) {
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' is not yet declared")
    }
    if (isOutput(idName)) {
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' is already marked as an output")
    }

    val rateDef = ctx.number.asScala.toSeq.map(_.getText.toDouble)
    if (rateDef.isEmpty) {
      specContainer = specContainer.addOutput(idName, None)
    } else {
      val (from, to, by) = (rateDef(0), rateDef(1), rateDef(2))
      if (specContainer.idNameToType(idName) != Real) {
        throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' cannot be sampled as it is not of type Real")
      }
      if (by <= 0) {
        throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' cannot be sampled with non-positive spacing between two values")
      }
      if (from > to) {
        throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '${idName}' cannot be sampled if the from value is larger than the to value")
      }
      specContainer = specContainer.addOutput(idName, Some((from, to, by)))
    }
  }

  override def visitEnumDef(ctx: EnumDefContext): Unit = {
    val typeName = ctx.ENUM_TYPE.getText
    if (occursInEnum(typeName)) {
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"enum type $typeName is already defined")
    }

    val constCtxs = ctx.enumConst.asScala.toSeq
    val constNames = constCtxs.map(_.getText)

    val duplicates = constNames.diff(constNames.distinct).distinct
    val errorSeq = constNames.zip(constCtxs).foldLeft(Seq[LolaDeclarationException]()) {
      case (curr, (constName, constCtx)) => 
        var newCurr = curr
        if (occursInEnum(constName)) {
          newCurr :+= LolaDeclarationException(LolaCompiler.getLocation(constCtx), s"enum constant $constName is already defined in another enum")
        }
        if (duplicates.contains(constName)) {
          newCurr :+= LolaDeclarationException(LolaCompiler.getLocation(constCtx), s"enum type $typeName contains constant $constName more than once")
        }
        newCurr
    }
    if (errorSeq.nonEmpty) throw LolaMultiCompilerException(errorSeq)

    specContainer = specContainer.addEnum(typeName, constNames.toSeq)
  }

  override def visitLinked(ctx: LinkedContext): Unit = {
    val leftId = ctx.id(0).getText
    val rightId = ctx.id(1).getText

    if (!isDeclared(leftId)) {
      LolaDeclarationException(LolaCompiler.getLocation(ctx.id(0)), s"Left stream '$leftId' of linking is not declared")
    }
    if (!isDeclared(rightId)) {
      LolaDeclarationException(LolaCompiler.getLocation(ctx.id(1)), s"Right stream '$rightId' of linking is not declared")
    }
    if (getIdType(rightId) != Bool) {
      LolaDeclarationException(LolaCompiler.getLocation(ctx.id(1)), s"Right stream '$rightId' of linking must be of type Bool but has type ${getIdType(rightId)}")
    }

    specContainer = specContainer.addLink(leftId, rightId)
  }

}

/**
  * ANTLR visitor for compiling a single lola expression.
  *
  * @param specContainer The current partial specification (for providing information about defined streams and enums)
  */
case class LolaExprVisitor(specContainer: SpecContainer) extends LolaBaseVisitor[AST.Expression] {

  // Constants
  override def visitConstant(ctx: ConstantContext): AST.Constant = visit(ctx.getChild(0)).asInstanceOf[AST.Constant]
  override def visitTrueLiteral(ctx: TrueLiteralContext): AST.Constant = AST.Constant(Value.BoolValue(true))
  override def visitFalseLiteral(ctx: FalseLiteralContext): AST.Constant = AST.Constant(Value.BoolValue(false))
  override def visitNumber(ctx: NumberContext): AST.Constant = AST.Constant(Value.RealValue(ctx.getText.toDouble))
  override def visitEnumConst(ctx: EnumConstContext): AST.Constant = {
    val constName = ctx.getText

    if (!specContainer.enumConstToType.contains(constName)) {
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"enum constant $constName is not defined")
    }
    val enumType = specContainer.enumConstToType(constName)

    AST.Constant(Value.EnumValue(constName, enumType))
  }

  // ID 
  override def visitId(ctx: IdContext): AST.Identifier = {
    val idName = ctx.getText
    if (!specContainer.isDeclared(idName)) {
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"id '$idName' is not yet declared")
    }
    AST.Identifier(idName, specContainer.idNameToType(idName))
  }


  // Labeled sub expressions
  override def visitPrefixOpExpr(ctx: PrefixOpExprContext): AST.Application = {
    val parserOp = ctx.prefixOp.getChild(0).asInstanceOf[TerminalNode].getSymbol.getType
    val expr = visit(ctx.expr)

    // Type checking
    val expectedType = parserOp match {
      case NOT => Bool
      case MINUS => Real
    }
    if (expr.getType != expectedType) {
      throw LolaTypeException(LolaCompiler.getLocation(ctx.expr), s"operator '${ctx.prefixOp.getText}' expects type $expectedType but found ${expr.getType}")
    }

    parserOp match {
      case MINUS => AST.Application(AST.times, Seq(AST.Constant(Value.RealValue(-1)), expr)) // special case for MINUS
      case _ => AST.Application(LolaCompiler.parserOpToASTOp(parserOp), Seq(expr))
    }
  }

  override def visitInfixOp1Expr(ctx: InfixOp1ExprContext): AST.Application = resolveInfixOp(ctx)
  override def visitInfixOp2Expr(ctx: InfixOp2ExprContext): AST.Application = resolveInfixOp(ctx)
  override def visitInfixOp3Expr(ctx: InfixOp3ExprContext): AST.Application = resolveInfixOp(ctx)
  override def visitInfixOp4Expr(ctx: InfixOp4ExprContext): AST.Application = resolveInfixOp(ctx)
  override def visitInfixOp5Expr(ctx: InfixOp5ExprContext): AST.Application = resolveInfixOp(ctx)

  private def resolveInfixOp(infixExprCtx: ParserRuleContext): AST.Application = {
    // a bit janky but probably no much better way since the InfixOpExprContext's don't have a common super class besides ParserRuleContext
    // but not all ParserRuleContexts have an infixOp subcontext (even if you would use constructs like "infixOp=infixOpPrio1" in the grammar)
    val parserOpCtx = infixExprCtx match {
      case ctx: InfixOp1ExprContext => ctx.infixOpPrio1
      case ctx: InfixOp2ExprContext => ctx.infixOpPrio2
      case ctx: InfixOp3ExprContext => ctx.infixOpPrio3
      case ctx: InfixOp4ExprContext => ctx.infixOpPrio4
      case ctx: InfixOp5ExprContext => ctx.infixOpPrio5
    }

    val parserOp = parserOpCtx.getChild(0).asInstanceOf[TerminalNode].getSymbol.getType
    val left = visit(infixExprCtx.getChild(0))
    val right = visit(infixExprCtx.getChild(2))
    val inputTypes = (left.getType, right.getType)

    // Type checking
    parserOp match {
      case EQ | NEQ => 
        if (left.getType != right.getType) {
          throw LolaTypeException(LolaCompiler.getLocation(parserOpCtx), s"operator '${parserOpCtx.getText}' expects two expressions of the same type but found $inputTypes")
        }
      case _ => 
        val expectedTypes: (Type, Type) = parserOp match {
          case AND | OR | IMPL | IFF => (Bool, Bool)
          case TIMES | PLUS | MINUS | LT | GT | LEQ | GEQ => (Real, Real)
        }
        if (inputTypes != expectedTypes) {
          throw LolaTypeException(LolaCompiler.getLocation(parserOpCtx), s"operator '${parserOpCtx.getText}' expects types $expectedTypes but found $inputTypes")
        }
    }

    parserOp match {
      case MINUS => AST.Application(AST.plus, Seq(left, AST.Application(AST.times, Seq(AST.Constant(Value.RealValue(-1)), right)))) // special case for MINUS
      case _ => AST.Application(LolaCompiler.parserOpToASTOp(parserOp), Seq(left, right))
    }
  }

  override def visitIteExpr(ctx: IteExprContext): AST.Expression = {
    val exprs = ctx.expr.asScala.toSeq.map(visit)
    
    // Type checking
    if (exprs.head.getType != Bool) {
      throw LolaTypeException(LolaCompiler.getLocation(ctx.expr(0)), s"ite condition expression must be of type Bool but found ${exprs.head.getType}")
    }
    if (exprs(1).getType != exprs(2).getType) {
      throw LolaTypeException(LolaCompiler.getLocation(ctx), s"ite return expressions must be of the same type but found ${(exprs(1).getType, exprs(2).getType)}")
    }

    AST.Application(AST.ite, exprs)
  }

  override def visitOffsetExpr(ctx: OffsetExprContext): AST.Expression = {
    val id = visitId(ctx.id)
    val offset = ctx.INT.getText.toInt
    val default = visitConstant(ctx.constant)

    // Type checking
    if (id.getType != default.getType) {
      throw LolaTypeException(LolaCompiler.getLocation(ctx), s"offset id '$id' is of type ${id.getType} but default value is of type ${default.getType}")
    }

    AST.Offset(id, offset, default.v)
  }

  override def visitConstantExpr(ctx: ConstantExprContext): AST.Constant = visitConstant(ctx.constant)
  
  override def visitIdExpr(ctx: IdExprContext): AST.Identifier = visitId(ctx.id)

  override def visitParExpr(ctx: ParExprContext): AST.Expression = visit(ctx.expr)

}

/**
  * ANTLR visitor for compiling a type (Bool, Real, Enum).
  *
  * @param specContainer The current partial specification (for providing information about defined enums)
  */
case class LolaTypeVisitor(specContainer: SpecContainer) extends LolaBaseVisitor[Type] {

  override def visitBoolType(ctx: BoolTypeContext): Type = Bool
  
  override def visitRealType(ctx: RealTypeContext): Type = Real

  override def visitEnumType(ctx: EnumTypeContext): Enum = {
    val typeName = ctx.getText
    specContainer.partialSpec.enumerations.find(_.name == typeName).getOrElse(
      throw LolaDeclarationException(LolaCompiler.getLocation(ctx), s"enum type $typeName is not defined")
    )
  }

}
