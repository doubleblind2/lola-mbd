package unaa_lola

import unaa_lola.BooleanFuture.BooleanFutureEvaluator
import unaa_lola.CertainEvaluation.CertainEvaluator
import unaa_lola.RangeEvaluation.ValueRangeEvaluator
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.PastOnlyZ3Evaluator
import unaa_lola.util.Logging
import unaa_lola.SymbolicEvaluation._
import Implementations.Implementation
import unaa_lola.Frontend.LolaCompiler

import scopt.OParser
import unaa_lola.AST.Identifier
import unaa_lola.Value.Bool
import unaa_lola.AST.SymbolIdentifier
import unaa_lola.util.Errors.EndOfInputReachedException

object Main {

  private def parseInputArguments(args: Array[String]): Option[Config] = {
    val builder = OParser.builder[Config]
    val argParser = {
      import builder._
      OParser.sequence(
        programName("unaa-lola"),
        head("unaa_lola", "v0.2"),
        arg[String]("LolaSourcePath")
          .action {(s, c) =>
            val srcFile = scala.io.Source.fromFile(s)
            val source = try srcFile.mkString finally srcFile.close()
            c.copy(specSource = source, inStream = Some(System.in), outStream = Some(System.out))
          }
          .text("Path to the LOLA specification file."),
        arg[String]("Evaluator")
          .validate {s =>
            if (Implementations.validEvaluatorNames.contains(s.toLowerCase.replace('-', '_'))) success else failure(s"Unsupported evaluator $s received. Only {${Implementations.validEvaluatorNames.mkString(", ")}} are supported.")
          }
          .action{(s, c) => CLIConfig.implementation = Implementations.withName(s.toLowerCase.replace('-', '_')); c}
          .text(s"Use one of {${Implementations.validEvaluatorNames.mkString(", ")}} to either allow only fully certain inputs, uncertainties handled by interval arithmetic abstraction (future and some other features unsupported), uncertainties handled by a symbolic engine (currently only z3), or uncertainties handled by an automaton (supports only boolean specs); see README for z3_past_only."),
        opt[Unit]("csv-in")
          .action((_, c) => c.copy(csvIn = true))
          .text("Consume events in CSV format."),
        opt[Unit]("csv-out")
          .action((_, c) => c.copy(csvOut = true))
          .text("Output events in CSV format."),
        opt[Unit]('v', "verbose")
          .action((_, c) => c.copy(verbose = true))
          .text("Be more verbose."),
        opt[Unit]('d', "debug")
          .action((_, c) => c.copy(loggingChannels = c.loggingChannels + Logging.DEBUG))
          .text("Add debug information to the main log."),
        opt[Unit]("log-file")
          .action((_, c) => c.copy(logFile = true))
          .text("Create file techLog.log instead of logging on std.err."),
        opt[Unit]("no-logging")
          .action((_, c) => c.copy(noLogging = true))
          .text("Do not log at all (aside from runtimes) and print only event outputs."),
        opt[Unit]("log-runtime")
          .action((_, c) => c.copy(logRuntimes = true))
          .text("Create file runtime.log with runtimes per event, one per line."),
        opt[Int]('i', "inputs")
          .validate(i => if (i >= 1) success else failure("--inputs must be positive."))
          .action((i, c) => c.copy(maxTs = Some(i - 1)))
          .text("Number of input lines (time stamps), unknown if unset."),
        opt[Int]("estimation-steps") // TODO maybe replace with more flexible simp strategy selection
          .action((i, c) => c.copy(maxSteps = Some(i)))
          .text("Abandon future estimation if taking longer than this."),
        opt[Unit]("z3-opt-queries-off")
          .action{(_, c) => c.copy(noZ3BoundQuery = true)}
          .text("Switch z3 optimizer off (may lead to worse results but faster runtime)."),
        opt[Unit]("disable-future")
          .action{(_, c) => c.copy(noFuture = true)}
          .text("Disable anticipation, i.e. use maximally abstracted values for positive offsets."),
        opt[Unit]("z3-runtime")
          .action{(_, c) => c.copy(collectZ3Runtimes = true)}
          .text("Collect query calls to z3 separately from total runtime. Only works in conjunction with --log-runtime."),
        opt[Int]('o', "opt-interval")
          .validate(i => if (i >= 1) success else failure("--opt-interval must be positive."))
          .action((i, c) => c.copy(simplificationStrategy = CommonStraegies.IntervalSimplificationStrategy(i)))
          .text("How often optimization (including pruning) takes place during forward calculation, defaults to no optimization."),
        opt[Int]('w', "force-widening")
          .validate(i => if (i >= 1) success else failure("--force-widening must be positive."))
          .action((i, c) => c.copy(futureSimplificationStrategy = CommonStraegies.WideningOnlySimplificationStrategy(i)))
          .text("Forces widening after this many steps during backwards calculation. Defaults to no widening."),
        opt[Int]('k', "keep")
          .action{(i,c) => c.copy(keepPastSymbolsFor = i)}
          .text("How far into the past symbols are kept during pruning and clean-up. Also determines the k for instant and temporal diagnosis."),
        opt[Unit]("singleDiag")
          .action{(_, c) => c.copy(singleDiag = true)}
          .text("Enable 0-instant diagnosis."),
        opt[Unit]("multiDiag")
          .action{(_, c) => c.copy(multiDiag = true)}
          .text("Enable k-instant diagnosis."),
        opt[Unit]("temporalDiag")
          .action{(_, c) => c.copy(temporalDiag = true)}
          .text("Enable k-temporal diagnosis."),
        opt[String]("checkAtempDiag")
          .action{(s, c) => c.copy(specificAtempDiag = Some(Set(Identifier(s, Bool))))},
        opt[String]("checkTempDiag")
          .action{(s, c) => val split = s.split(','); c.copy(specificTempDiag = Some(Set(SymbolIdentifier(Identifier(split(0), Bool), split(1).toInt))))},
        checkConfig {c =>
          if (c.logFile && c.noLogging) failure("--log-file and --no-logging were both provided.") else success  
        }
      )
    }
    OParser.parse(argParser, args, Config())
  }

  /**
   * Main method of the tool
   * @param args Arguments for application configuration (see above)
   */
  def main(args: Array[String]): Unit = {
    parseInputArguments(args) match {
      case None => System.exit(1)
      case Some(config) =>
        if (config.logFile) {
          Logging.addSink(new Logging.FileLogger(config.loggingChannels, s"techLog.log"))
        } else if (!config.noLogging) {
          Logging.addSink(new Logging.StdIOLogger(config.loggingChannels))
        }

        if (config.logRuntimes) {
          Logging.addSink(new Logging.FileLogger(Set(Logging.RUNTIMES), s"runtime.log", true))
        }

        if (config.diagnosisEnabled) {
          Logging.addSink(new Logging.FileLogger(Set(Logging.DIAGNOSES), "diagnoses.log", true))
        }

        if (config.diagnosisEnabled && config.logRuntimes) {
          Logging.addSink(new Logging.FileLogger(Set(Logging.DIAG_RUNTIMES), "diagnosis_runtime.log", true))
        }

        LolaCompiler(config.specSource) match {
          case Left(err) => System.err.println(s"Compilation failed!\n\nDetails:\n$err");
          case Right(ast) =>
            Logging.log(ast.toString, channel = Logging.DEBUG)
            val evaluator = Evaluator.makeEvaluator(ast, CLIConfig.implementation, config)
            try {
              evaluator.setupCSV()
              evaluator.allSteps()
            } catch {
              case EndOfInputReachedException => 
            }
        }
    }
  }
}
