// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp

import ee.cyber.simplicitas.SourceMessage
import ee.cyber.simplicitas.GenericGrammar

/** Callbacks that are passed to the @see APluginConfig#parse method.<br>
  * Call <code>parse</code> method to parse the source with given grammar.
  * Call <code>reportErrors</code> to log errors discovered during program
  * validation. */
trait ParseCtx {
    /** Report validation errors to user. */
    def reportErrors(errors: Iterable[SourceMessage])
    
    /** Parse the active DSL program with the given grammar. Returns true,
      * if the program was syntactically correct. */
    def parse(grammar: GenericGrammar): Boolean
}
