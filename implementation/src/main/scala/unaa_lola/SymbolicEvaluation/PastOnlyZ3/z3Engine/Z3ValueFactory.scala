package unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine

import unaa_lola.AST
import unaa_lola.AST.Identifier
import unaa_lola.Value.{Bool, Real}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.POValue.{Value, UncertainBoolValue, UncertainRealValue}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.ValueFactory
import unaa_lola.util.Errors

/**
 * Common factory class for creating [[unaa_lola.SymbolicEvaluation.z3Engine.LinAlg.Z3LinAlgFormula]] and [[unaa_lola.SymbolicEvaluation.z3Engine.Logical.Z3LogicalFormula]] (wrapped in AST values)
 * @param env The corresponding [[Z3QueryEnvironment]]
 */
class Z3ValueFactory(env: Z3QueryEnvironment) extends ValueFactory {

  /**
   * Calculates the value of an operation on formulas and constant values
   * @param op The operator
   * @param vals formulas and constant values on which the operation is performed
   * @return Value resulting from the operation (either certain or uncertain)
   */
  override def buildFromASTOp(op: AST.Operator, vals: Seq[Value]): Value = {

    if (vals.forall(_.getType == Bool)) {
      env.getLogicalFormulaFactory.buildFromASTOp(op, vals)
    } else if (vals.forall(_.getType == Real)) {
      env.getLinAlgFormulaFactory.buildFromASTOp(op, vals)
    } else {
      throw new Errors.InternalException(s"Z3ValueFactory.buildFromASTOp cannot handle this operation: $op(${vals.mkString(",")})")
    }

  }

  /**
   * Create uncertain constant with a given identifier
   * @param id Identifier to be used
   * @return Constant encoding the identifier
   */
  override def buildConstant(id: Identifier): Value = {
    id.typ match {
      case Real => UncertainRealValue(env.getLinAlgFormulaFactory.const(id))
      case Bool => UncertainBoolValue(env.getLogicalFormulaFactory.const(id))
    }
  }

}
