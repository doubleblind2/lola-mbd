package unaa_lola

/**
  * Enumeration of all implemented evaluation engines
  * TODO rework these once migrated to Scala 3
  *      differentiate between symbolic and non-symbolic implementations and only allow the former for evaluation engines
  */
object Implementations extends Enumeration {
  type Implementation = Value

  val certain, range, boolean_automaton, z3, z3_past_only = Value

  val validEvaluatorNames: Set[String] = Implementations.values.map(_.toString)

  /**
    * Checks if an implementation natively supports enumerations.
    *
    * @param impl The implementation in question
    * @return True if impls natively supports enums, false if not
    */
  def supportsEnums(impl: Implementation): Boolean = Set(certain).contains(impl)
}

