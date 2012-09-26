// This file contains generated code. You are not supposed to edit it.
// Instead, use the file ExpConfig.scala to customize your DSL IDE.

package ee.cyber.simplicitas.exprewriting

import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.imp.parser._
import ee.cyber.simplicitas.eclipse.GenerateAction

import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.SWT


object ExpPlugin {
  var instance: ExpPlugin = null
  
  def getInstance =
    if (instance eq null)
      new ExpPlugin()
    else
      instance
  
  val factory = () => getInstance
}

class ExpPlugin extends SimplicitasPlugin {
  def getID = ExpConfig.pluginId
  def getLanguageID = ExpConfig.languageId

  ExpPlugin.instance = this
  
  colorCache = collection.mutable.Map()

  colorDefs =
    Map[Symbol, Tuple3[String, String, Number]](
      'keyword -> ("Keywords", "128,0,128", SWT.BOLD),
      'operator -> ("Operators", "0,0,0", SWT.NORMAL),
      'comment -> ("Comments", "128,128,0", SWT.ITALIC)) ++
    ExpConfig.colors

  override def initializeImageRegistry(registry: ImageRegistry) {
    super.initializeImageRegistry(registry)

    val bundle = org.eclipse.core.runtime.Platform.getBundle(
            ExpConfig.pluginId);
    val addFun =
      (key: String, path: String) =>
        addImage(key, path, bundle, registry)
    ExpConfig.initializeImages(addFun)
  }
}

class ExpParseController extends 
  SimplicitasParseController(ExpConfig.language, ExpConfig.instance) {}

class ExpTokenColorer extends TokenColorerBase(ExpPlugin.factory,
                                                    ExpConfig.instance) {}

class ExpTreeModelBuilder
  extends SimplicitasTreeModelBuilder(ExpConfig.instance) {
}

class ExpLabelProvider
    extends SimplicitasLabelProvider(ExpConfig.instance) {}

class ExpReferenceResolver
    extends SimplicitasReferenceResolver(ExpConfig.instance) {}

class ExpFoldingUpdater
    extends SimplicitasFoldingUpdater(ExpConfig.instance) {}

class ExpDocumentationProvider
    extends SimplicitasDocumentationProvider(ExpConfig.instance) {}

class ExpContentProposer
    extends SimplicitasContentProposer(ExpConfig.instance) {}

class ExpOccurrenceMarker
    extends SimplicitasOccurrenceMarker(ExpConfig.instance) {}

class ExpPreferencePage
    extends SimplicitasPreferencePage(ExpPlugin.factory) {}

class ExpPreferenceInitializer
    extends SimplicitasPreferenceInitializer(ExpPlugin.factory) {}

class ExpGenerateAction extends GenerateAction(ExpConfig.instance) {}