// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas

import collection.mutable.ArrayBuffer
import ee.cyber.simplicitas.parse._

/** Base class for DSL generator main program. It is responsible for
  * processing command-line options and contains useful functions, e.g.,
  * for printing errors. */
class MainBase {
  /** Destination directory where the generated files will be stored. */
  var destDir: String = null
  /** DSL source files. */
  val sources = new ArrayBuffer[String]
  /** Language-specific options. All the options that come
    * after the -- option. */
  val languageOptions = new ArrayBuffer[String]

  /** Parses the command-line options and initializes properties
    * <code>destDir</code>, <code>sources</code> and
    * <code>languageOptions</code>. */
  def parseOptions(argv: Array[String]) {
    var inLanguageOptions = false
    var isDest = false
    for (arg <- argv) {
      if (isDest) {
        destDir = arg
        isDest = false
      } else if (inLanguageOptions) {
        languageOptions += arg
      } else if (arg.startsWith("--")) {
        if (arg == "--") {
          inLanguageOptions = true
        } else if (arg == "--dest") {
          isDest = true
        } else {
          invalidOption(arg)
        }
      } else {
        sources += arg
      }
    }
    if (sources.isEmpty) {
      usage()
    }
  }

  /** If the parsing encountered errors, then print the error messages
    * and exit the program. */
  def checkErrors(grammar: GenericGrammar) {
    checkErrors(grammar.errors)
  }

  /** If the error list is not empty, print the errors and exit the program. */
  def checkErrors(errors: Iterable[SourceMessage]) {
     if (!errors.isEmpty) {
        Console.err.println("Messages")
        errors foreach (Console.err.println)
        exit(1)
      }
   }

  def invalidOption(opt: String) {
    Console.err.println("Invalid option: " + opt)
    usage()
  }

  /** Prints message explaining command-line options. */
  def usage() {
    Console.err.println(
      "Usage: Main [--dest <target directory>] <source files> [-- <generator options>]")
    exit(2)
  }
}
