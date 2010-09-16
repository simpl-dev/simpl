// This file contains generated code. You are not supposed to edit it.
// Instead, use the file FowlerConfig.scala to customize your DSL IDE.

package ee.cyber.simplicitas.fowlerdsl

import ee.cyber.simplicitas.eclipse.GenerateAction
import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.imp.parser._

import org.eclipse.swt.SWT


object FowlerPlugin {
  var instance: FowlerPlugin = null
  
  def getInstance =
    if (instance eq null)
      new FowlerPlugin()
    else
      instance
  
  val factory = () => getInstance
}

class FowlerPlugin extends SimplicitasPlugin {
  def getID = FowlerConfig.pluginId
  def getLanguageID = FowlerConfig.languageId

  FowlerPlugin.instance = this

  colorCache = collection.mutable.Map()

  colorDefs =
    Map[Symbol, Tuple3[String, String, Number]](
      'keyword -> ("Keywords", "128,0,128", SWT.BOLD),
      'operator -> ("Operators", "0,0,0", SWT.NORMAL),
      'comment -> ("Comments", "128,128,0", SWT.ITALIC)) ++
    FowlerConfig.colors
}

class FowlerParseController extends 
  SimplicitasParseController(FowlerConfig.language, FowlerConfig.instance) {}

class FowlerTokenColorer extends TokenColorerBase(FowlerPlugin.factory,
                                                  FowlerConfig.instance) {}

class FowlerTreeModelBuilder
    extends SimplicitasTreeModelBuilder(FowlerConfig.instance) {}

class FowlerLabelProvider
    extends SimplicitasLabelProvider(FowlerConfig.instance) {
  def plugin: org.eclipse.ui.plugin.AbstractUIPlugin =
    FowlerPlugin.getInstance
}

class FowlerReferenceResolver
    extends SimplicitasReferenceResolver(FowlerConfig.instance) {}

class FowlerFoldingUpdater
    extends SimplicitasFoldingUpdater(FowlerConfig.instance) {}

class FowlerDocumentationProvider
    extends SimplicitasDocumentationProvider(FowlerConfig.instance) {}

class FowlerContentProposer
    extends SimplicitasContentProposer(FowlerConfig.instance) {}

class FowlerOccurrenceMarker
    extends SimplicitasOccurrenceMarker(FowlerConfig.instance) {}

class FowlerPreferencePage
    extends SimplicitasPreferencePage(FowlerPlugin.factory) {}

class FowlerPreferenceInitializer
    extends SimplicitasPreferenceInitializer(FowlerPlugin.factory) {}

class FowlerGenerateAction extends GenerateAction(FowlerConfig.instance) {}
