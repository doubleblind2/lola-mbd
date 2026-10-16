package unaa_lola

import unaa_lola.AST.{Identifier, Specification, ExactSpecification}
import unaa_lola.Value._
import unaa_lola.util.Logging

import java.util.Scanner
import unaa_lola.AST.EnumFreeSpecification
import unaa_lola.util.Errors
import unaa_lola.CertainEvaluation.CertainEvaluator
import unaa_lola.RangeEvaluation.ValueRangeEvaluator
import unaa_lola.BooleanFuture.BooleanFutureEvaluator
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.PastOnlyZ3Evaluator
import unaa_lola.SymbolicEvaluation.SymbolicEvaluator
import unaa_lola.Frontend.LolaCompiler

object Evaluator {

  /**
    * Returns an instantiation of an [[Evaluator]] implementation from specification and configuration.
    *
    * @param spec The specification
    * @param impl One of the available evaluators
    * @param config A configuration
    * @return The evaluator
    */
  def makeEvaluator(spec: ExactSpecification, impl: Implementations.Implementation, config: Config): Evaluator = {
    impl match {
      case Implementations.certain => new CertainEvaluator(spec, config)
      case Implementations.range => new ValueRangeEvaluator(spec, config)
      case Implementations.boolean_automaton => new BooleanFutureEvaluator(spec, config)
      case Implementations.z3_past_only => new PastOnlyZ3Evaluator(spec, config)
      case symbolicImpl => new SymbolicEvaluator(symbolicImpl, spec, config)
    }
  }

  /**
    * Builds an instantiation of an [[Evaluator]] implementation from a specification file and configuration.
    *
    * @param specSourcePath The path to the specification file
    * @param impl One of the available evaluators
    * @param config A configuration
    * @return The evaluator
    */
  def makeEvaluator(specSourcePath: String, impl: Implementations.Implementation, config: Config): Evaluator = 
    LolaCompiler(specSourcePath) match {
      case Left(exception) => throw exception
      case Right(spec) => makeEvaluator(spec, impl, config)
    }

}

/**
  * Abstract class for every evaluator implementation.
  * Handles the bulk of the input/output logic.
  *
  * @param spec The exact specification as received from the frontend parser. This shouldn't be used after initalizing, instead use flatspec.
  * @param config A configuration
  * @param supportsEnums Set this if the concrete evaluator natively supports enumerations
  * @param ultraFlat Set this to not flatten input streams (only used by [[BooleanFutureEvaluator]])
  */
abstract class Evaluator(spec: ExactSpecification, config: Config, supportsEnums: Boolean, ultraFlat: Boolean = false) {

  /**
    * The flattened specification. Enumerations have also been replaced if unsupported. Always use this instead of the class parameter.
    */
  val flatspec: Specification = (new Flattener).flattenSpec(if (supportsEnums) spec else spec.replaceEnumsWithReals, ultraFlat)

  private var csvHeader : Seq[Option[Identifier]] = Seq()
  private var outputValues: Map[Identifier, Value] = Map()
  private var inputBuffer: Map[Identifier, Value] = Map()

  var ts: Int = -1

  private val scanner = config.inStream.map(new Scanner(_))

  def printOutput(x: String) = config.outStream.get.println(x)

  // TODO make inputs parameter of spec just a set of identifiers instead of Map[String, Type]
  // TODO remove this afterwards
  private val inputIds = spec.inputs.map{case (s, t) => Identifier(s, t)}.toSet

  /**
    * Adds value for at least one input stream.
    * Can be called multiple times.
    * If an input for the same input stream is provided multiple times for one timestamp, the most recent one is used.
    *
    * @param inputs A map mapping input stream identifiers to their corresponding values
    */
  def addInputs(inputs: Map[Identifier, Value]): Unit = inputs.foreach{case (id, value) => addInput(id, value)}

  /**
    * Adds the value for an input stream.
    * If an input for the same input stream is provided multiple times for one timestamp, the most recent one is used.
    *
    * @param id The identifier of the input stream
    * @param value The value to be input
    */
  def addInput(id: Identifier, value: Value): Unit = {
    require(spec.inputs.contains(id.name), s"Identifier'$id' not declared as an input stream")
    require(spec.inputs(id.name) == value.getType, s"Type mismatch. Input value '$value' has type '${value.getType}' but stream '${id.name}' has type '${id.getType}'")
    inputBuffer += (id -> value)
  }


  /**
   * Evaluates the specification for one time instant.
   * Shouldn't be called directly, instead use this class's step functions.
   */
  def eval(): Unit

  /**
    * Returns the inputs for every input stream for the current timestamp. 
    * If inputs are read from a Java InputStream (see config), this function either reads a single line for CSV-style inputs
    * or one line for each input stream when in equational mode. The latter stalls until there is a value for each input stream.
    * When used as API, the inputs have to have been already provided using [[addInput]] or [[addInputs]].
    * 
    * Should not be called directly but only from within [[eval]] in the evaluator implementation.
    * 
    * @return A map mapping each input stream identifier to the corresponding value
    */
  def consumeInputs: Map[Identifier, Value] = {
    if (config.inStream.isDefined) {
      if (config.csvIn) readCsvInputs()
      else readInputs()
    } else if (inputBuffer.keySet != inputIds) {
      throw new IllegalStateException(s"Evaluation cannot continue because not all inputs have been provided. Missing inputs for {${(inputIds -- inputBuffer.keySet).mkString(", ")}}")
    } 
    fillInternalInputs()
    inputBuffer
  }

  /**
    * Must be called when using CSV-style inputs/outputs before any inputs are read.
    * Reads the CSV header from the configured InputStream for the input streams and/or prints to the configured PrintStream the header for th eoutput streams.
    */
  def setupCSV(): Unit = {
    if (config.csvIn && config.inStream.isDefined) {
      readCsvHeader()
    }

    if (config.csvOut && config.outStream.isDefined) {
      printOutput(flatspec.outputs.map(i => i.name).mkString(","))
    } else {
      Logging.log("Ready for evaluation!")
    }
  }

  /**
    * Repeatedly calls [[step]] until (inclusive) the provided timestamp.
    * Can only be used when inputs are provided via a configured InputStream.
    *
    * @param until The timestamp up to which the evaluation is performed
    * @return Maps mapping output stream identifiers to values, ordered by timestamp
    */
  def multiSteps(until: Int): Seq[Map[Identifier, Value]] = {
    (ts+1 to until).foldLeft(Seq[Map[Identifier, Value]]()) {
      (curr, _) => curr :+ step()
    }
  }

  /**
    * Repeatedly calls [[step]] until the end of the trace
    * Can only be used when inputs are provided via a configured InputStream.
    *
    * @return Maps mapping output stream identifiers to values, ordered by timestamp
    */
  def allSteps(): Seq[Map[Identifier, Value]] = {
    multiSteps(config.maxTs.getOrElse(Int.MaxValue - 1))
  }

  /**
    * Runs the evaluator for one time step.
    * Increments the current timestamp, calls the evaluator specific [[eval]] function, 
    * and prints the output values if a PrintStream is configured.
    *
    * @return A map mapping output stream identifiers to the corresponding values for this timestamp
    */
  def step(): Map[Identifier, Value] = {
    if (ts >= config.maxTs.getOrElse(Int.MaxValue - 1))
      throw new IllegalStateException(s"Next evaluation step will exceed max trace length of ${config.maxTs.getOrElse(Int.MaxValue - 1)}")  

    ts += 1
    outputValues = Map()
    eval()
    inputBuffer = Map()
    if (outputValues.keySet != spec.outputs.toSet)
      throw new IllegalStateException(s"Missing output values for streams {${(spec.outputs.toSet -- outputValues.keySet).mkString(", ")}} after evaluation")
    printOutputValues()
    outputValues
  }

  /**
    * Runs the evaluator for one time step for a given set of input.
    * Increments the current timestamp, calls the evaluator specific [[eval]] function, 
    * and prints the output values if a PrintStream is configured.
    *
    * @param inputs A map mapping all input stream identifiers to the desired corresponding input values
    * @return A map mapping output stream identifiers to the corresponding values for this timestamp
    */
  def step(inputs: Map[Identifier, Value]): Map[Identifier, Value] = {
    addInputs(inputs)
    step()
  }

  // prints output vales to the PrintStream is set
  private def printOutputValues(): Unit = {
    if (config.outStream.isEmpty) return 

    if (config.csvOut) {
      printOutput(spec.outputs.map(outputValues(_).toString(config.verbose)).mkString(","))
    } else {
      spec.outputs.foreach {
        stream => printOutput(s"$ts: $stream = ${outputValues(stream).toString(config.verbose)}")
      }
    }
  }

  /**
    * Stores the value of one output stream.
    * Should not be called directly but only from within an evaluator implementation's [[eval]] function.
    * Must be called for each output stream once per timestamp.
    * Takes care of decoding a real value back to an enum value if necessary.
    *
    * @param stream The output stream identifier. In case of an enum this may be the encoding real stream.
    * @param output The output value. In case of an enum this may be the encoding real value.
    */
  def singleValOutput(stream: Identifier, output: Value): Unit = {
    if (!flatspec.outputs.contains(stream)) throw new Errors.InternalException(s"$stream is not an output stream.")

    val encodedEnum = flatspec.getEncodedEnum(stream)
    val encodedStream = if (encodedEnum.isEmpty) stream else Identifier(stream.name, encodedEnum.get)
    val encodedOutput = decodeRealsToEnumValues(stream, output)

    outputValues += (encodedStream -> encodedOutput)
  }

  // decodes a real value back to an enum value if the real stream encodes an enum stream
  private def decodeRealsToEnumValues(stream: Identifier, value: Value): Value = {
    val encodedEnum = flatspec.getEncodedEnum(stream)

    if (encodedEnum.isEmpty) value else {
      value match {
        case RealValue(v) => EnumValue(encodedEnum.get.constants(v.asInstanceOf[Int]), encodedEnum.get) 
        case RealSetValue(options) =>
          val encodedValues = options.map(d => encodedEnum.get.constants(d.asInstanceOf[Int]))
          UncertainEnumValue(encodedValues, encodedEnum.get)
        case RangeValue(from, to, _) =>
          val enumLength = encodedEnum.get.constants.length
          val f = (if (from.isEmpty || from.get < 0) 0 else from.get).asInstanceOf[Int]
          val t = (if (to.isEmpty || to.get > enumLength - 1) enumLength - 1 else to.get).asInstanceOf[Int]
          val encodedValues = encodedEnum.get.constants.slice(f, t + 1)
          UncertainEnumValue(encodedValues.toSet, encodedEnum.get)
      }
    }
  }

  // parses a string received for an input stream to a value
  // also takes care of encoding enum inputs to reals if necessary
  private def inputStringToValue(stream: Identifier, input: String): Value = {
    val encodedEnum = flatspec.getEncodedEnum(stream)

    try {
      if (input == "?") {
        // entirely unknown
        stream.getType match {
          case Bool => UnknownBoolValue()
          case Real if encodedEnum.isDefined => UncertainEnumValue(encodedEnum.get.constants.toSet, encodedEnum.get).realSetRepresentative
          case Real => RangeValue(None, None)
          case e: Enum => UncertainEnumValue(e.constants.toSet, e)
        }
      } else if (input.startsWith("[")) {
        // real ranges
        stream.getType match {
          case Real if encodedEnum.isEmpty => 
            val cv = input.replace("[", "").replace("]", "").split(",")
            val f = if (cv(0) == "?") None else Some(cv(0).toDouble)
            val t = if (cv(1) == "?") None else Some(cv(1).toDouble)
            if (f.isDefined && t.isDefined && f.get > t.get) {
              throw new Errors.InternalException(s"Real range $input received with smaller lower bound than upper bound.")
            } else if (f.isDefined && t.isDefined && f.get == t.get) {
              RealValue(f.get)
            } else {
              RangeValue(f, t)
            }
        }
      } else if (input.startsWith("{")) {
        // sets
        val inputConsts = input.replace("{", "").replace("}", "").split(",").toSet
        if (inputConsts == Set("")) {
          throw new Errors.InternalException(s"Empty set for $stream received. Empty sets are not allowed.")
        } else if (inputConsts.size == 1) {
          inputStringToValue(stream, inputConsts.toSeq(0)) // singleton set
        } else {
          stream.getType match {
            case Bool => UnknownBoolValue() // same as ?
            case Real if encodedEnum.isDefined => UncertainEnumValue(inputConsts, encodedEnum.get).realSetRepresentative
            case Real => RealSetValue(inputConsts.map(_.toDouble))
            case e: Enum => UncertainEnumValue(inputConsts, e)
          }
        }
      } else {
        // certain
        stream.getType match {
          case Bool => BoolValue(input.toBoolean)
          case Real if encodedEnum.isDefined => EnumValue(input, encodedEnum.get).realRepresentative
          case Real => RealValue(input.toDouble)
          case e: Enum => EnumValue(input, e)
        }
      }
    } catch {
      // TODO give the user the chance to try again
      case t: Throwable => throw new Errors.InternalException(s"Input parsing for $stream failed with value $input ($t)")
    }
  }

  // read single line
  private def nextInputLine(): String = {
    try {
      scanner.get.nextLine()
    } catch {
      case t: Throwable =>
        throw Errors.EndOfInputReachedException
    }
  }

  // read non-CSV inputs for all input streams 
  private def readInputs(): Unit = {
    var todo = flatspec.nonInternalInputs
    while(todo.nonEmpty) {
      val inp = nextInputLine()
      if (inp == "exit") System.exit(0) // CLI mode only

      val in = inp.replaceAll(" ", "").split("=")

      if (in.length == 2 && todo.contains(in(0))) {
        Logging.log(s"Input received: ${in.mkString(" = ")}")
        val stream = Identifier(in(0), todo(in(0)))
        val value = inputStringToValue(stream, in(1))
        inputBuffer += (stream -> value)

        todo = todo - in(0)
      } else if (inp.trim == "") {
        // wait for input 
        // TODO also maybe allow comments
      } else if (in.length != 2) {
        Logging.log(s"Input \"${inp}\" is not in the correct format. Try again!", channel = Logging.TECHNICAL_LOG)
      } else {
        Logging.log(s"Input stream \"${in(0)}\" (full input: \"${inp}\") either not known or already consumed for this timestamp. Try again!", channel = Logging.TECHNICAL_LOG)
      }
    }
  }

  // read CSV header, has to be called before any CSV input readings are allowed
  // order of the stream names in the header defines order of the input values
  private def readCsvHeader(): Unit = {
    val inp = nextInputLine()
    val headerEntries = inp.split(",")

    csvHeader = headerEntries.map(
      e =>
        if (!flatspec.nonInternalInputs.contains(e)) {
          Logging.log(s"Column $e is no valid input stream for the specification.", sev = Logging.WARNING)
          None
        } else {
          Some(Identifier(e, flatspec.inputs(e)))
        }
    )
  }

  // read CSV inputs for all input streams
  private def readCsvInputs(): Unit = {
    val inp = nextInputLine()
    Logging.log(s"Input received: $inp")

    val valueEntries = inp.split(""",(?![^\{\[]*[\]\}])""")

    csvHeader.zip(valueEntries).foreach {
      case (Some(stream), input) => inputBuffer += (stream -> inputStringToValue(stream, input))
      case _ =>
    }
  }

  // sends dummy "?" inputs for every internal stream to the evaluator
  private def fillInternalInputs(): Unit = {
    flatspec.inputs.removedAll(flatspec.nonInternalInputs.keySet).foreach {
      case (s, t) =>
        val stream = Identifier(s, t)
        inputBuffer += (stream -> inputStringToValue(stream, "?"))
    }
  }

}
