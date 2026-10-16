package unaa_lola

import unaa_lola.util.Errors
import unaa_lola.util.Common.OInterval

/**
  * Hosts classes about types and values used during user interaction (input, output) as well as during evaluation in the CertainEvaluator and ValueRangeEvaluator.
  * Symbolic evaluation implementations should use their own structure to represent symbolic values.
  */
object Value {

  //Types

  /**
   * Basic types (real, bool, enum)
   */
  sealed trait Type

  /**
   * Type for reals (Double in the implementation)
   */
  case object Real extends Type {
    override def toString: String = "Real"
  }

  /**
   * Type for bools
   */
  case object Bool extends Type {
    override def toString: String = "Bool"
  }

  /**
    * Type for enums
    *
    * @param name The name of the type
    * @param constants The sequence of values defined by the type, i.e. values expressions of this type can assume
    */
  case class Enum(name: String, constants: Seq[String]) extends Type {
    override def toString: String = s"Enum($name)"

    /**
      * String including sequence of constants
      */
    val verboseString = s"Enum($name, {${constants.mkString(", ")}})"
  }

  /**
    * Super class for value types
    * TODO make this sealed again once CarrierValue in PastOnlyZ3Evaluator has been removed again
    */
  trait Value {
    /**
      * Returns the type of value
      *
      * @return The type of the value
      */
    def getType: Type

    /**
      * Returns a string representative of the value
      *
      * @param verbose Indicates if the representation should be verbose (e.g. including constraint descriptions for ranges)
      * @return The string representative
      */
    def toString(verbose: Boolean = true): String

    /**
      * Indicates whether the value is certain, i.e. the value is not an expression describing mutliple concrete values
      *
      * @return True if certain
      */
    def isCertain: Boolean
  }
  
  /**
    * Known bool value
    *
    * @param v The boolean value
    */
  case class BoolValue(v: Boolean) extends Value {
    override val getType: Type = Bool
    override val isCertain: Boolean = true
    override def toString(verbose: Boolean = true): String = v.toString
  }
  
  /**
    * Known real value
    *
    * @param v The real (double) value
    */
  case class RealValue(v: Double) extends Value {
    override val getType: Type = Real
    override val isCertain: Boolean = true
    override def toString(verbose: Boolean = true): String = String.format("%.2f", v)
  }

  /**
    * Invalid real value used only for outputs in case a real stream linked to a bool stream
    * is queried but said bool stream is known to be false.
    */
  case object InvalidRealValue extends Value {
    override val getType: Type = Real
    override val isCertain: Boolean = true
    override def toString(verbose: Boolean = true): String = "INVALID"
  }
  
  /**
   * Interval over reals. Either limit may be infinite (None).
   *
   * @param from The lower limit
   * @param to The upper limit
   * @param desc A string describing the value. Concrete format is implementation dependent. Defaults to a string "(from, to)" if no further information is provided.
   */
  case class RangeValue(from: Option[Double], to: Option[Double], desc: String = "") extends Value {
    private def rangeToString(from: Option[Double], to: Option[Double]): String = {
      def formatDbleOpt(d: Option[Double]): String = if (d.isEmpty) "?" else String.format("%.2f", d.get)

      if (from.isEmpty && to.isEmpty) "?" else s"[${formatDbleOpt(from)}|${formatDbleOpt(to)}]"
    }
    override val getType: Type = Real
    override val isCertain: Boolean = false
    override def toString(verbose: Boolean = true): String = rangeToString(from, to) + (if (verbose && desc != "") s" //$desc" else "")

    /**
      * Replaces the lower and upper limits of the range by the smallest and largest double respectively, thus making the range finite
      *
      * @return The finite range
      */
    def finiteRepresentation: RangeValue = {
      val newFrom = from match {
        case None => Some(Double.NegativeInfinity)
        case d => d
      }
      val newTo = to match {
        case None => Some(Double.PositiveInfinity)
        case d => d
      }
      copy(from = newFrom, to = newTo)
    }
  }

  /**
    * Set of reals
    *
    * @param options The set of possible reals (doubles)
    */
  case class RealSetValue(options: Set[Double]) extends Value {
    override val getType: Type = Real
    override val isCertain: Boolean = false
    override def toString(verbose: Boolean = true): String = if (options.size == 1) options.head.toString else s"{${sortedOptions.map(_.toString).mkString(", ")}}"

    lazy val sortedOptions: Seq[Double] = options.toSeq.sorted

    /**
      * Overapproximates the set to a finite [[RealRange]]
      *
      * @return The corresponding range
      */
    def toRangeValue: RangeValue = RangeValue(Some(options.min), Some(options.max))
  }
  
  /**
   * Unknown bool value.
   * 
   * @param desc A string describing the value. Concrete format is implementation dependent. Defaults to "?" if no further information is provided.
   */
  case class UnknownBoolValue(desc: String = "") extends Value {
    override val getType: Type = Bool
    override val isCertain: Boolean = false
    override def toString(verbose: Boolean = true): String = "?" + (if (verbose && desc != "") s" //$desc" else "")
  }

  /**
    * Known value of an enum type
    *
    * @param v The string defining the value
    * @param enumType The type this value corresponds to
    */
  case class EnumValue(v: String, enumType: Enum) extends Value {
    if (!enumType.constants.contains(v)) {
      throw new Errors.InternalException(s"CertainEnumValue $v initiated of type ${enumType.name} but ${enumType.name} only enumerates the constants ${enumType.constants}", panic = true)
    }
    override val getType: Type = enumType
    override val isCertain: Boolean = true
    override def toString(verbose: Boolean = true): String = v

    /**
      * The [[RealValue]] representing this value (used during encoding and decoding)
      */
    lazy val realRepresentative: RealValue = RealValue(enumType.constants.indexOf(v))
  }

  /**
    * Unknown value of an enum type
    *
    * @param options The set of possible known values, represented by their names
    * @param enumType The type the options correspond to
    */
  case class UncertainEnumValue(options: Set[String], enumType: Enum) extends Value {
    if (!options.subsetOf(enumType.constants.toSet)) {
      throw new Errors.InternalException(s"Options $options of UncertainEnumValue are not a subset of $enumType's constants ${enumType.constants}", panic = true)
    }

    override val getType: Type = enumType
    override val isCertain: Boolean = false
    override def toString(verbose: Boolean = true): String = if (options.size == 1) options.head else s"{${sortedOptions.map(_.toString).mkString(", ")}}"

    // ordered by appearance in the enum type definition
    lazy val sortedOptions: Seq[String] = {
      val indexMap = enumType.constants.zipWithIndex.toMap
      val ordering: Ordering[String] = Ordering.by(indexMap)
      options.toSeq.sorted(ordering)
    }

    /**
      * Converts (and overapproximates) the set of enum values to a finite real range
      *
      * @return The resulting [[RealRange]]
      */
    def realRangeRepresentative: RangeValue = RangeValue(Some(enumType.constants.indexOf(sortedOptions.head)), Some(enumType.constants.indexOf(sortedOptions.last)))

    /**
      * Converts the set of enum values to a set of real values
      *
      * @return The resulting [[RealSetValue]]
      */
    def realSetRepresentative: RealSetValue = RealSetValue(options.map(enumType.constants.indexOf(_).toDouble))
  }

  // For API use
  implicit def valueConst(v: Boolean): BoolValue = BoolValue(v)
  implicit def valueConst(v: Double): RealValue = RealValue(v)
  def range(a: Double, b: Double): RangeValue = RangeValue(Some(a), Some(b))
  def range(a: Option[Double], b: Option[Double]): RangeValue = RangeValue(a, b) // due to type erasure ints must be explicitly converted to double
  val unknownBool = UnknownBoolValue()
  val unknownReal = RangeValue(None, None)
  
}