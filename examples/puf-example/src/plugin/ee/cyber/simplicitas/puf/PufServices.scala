// This file contains generated code. You are not supposed to edit it.
// Instead, use the file PufConfig.scala to customize your DSL IDE.

package ee.cyber.simplicitas.puf

import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.imp.parser._
import ee.cyber.simplicitas.eclipse.GenerateAction

import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.SWT


object PufPlugin {
  var instance: PufPlugin = null
  
  def getInstance =
    if (instance eq null)
      new PufPlugin()
    else
      instance
  
  val factory = () => getInstance
}

class PufPlugin extends SimplicitasPlugin {
  def getID = PufConfig.pluginId
  def getLanguageID = PufConfig.languageId

  PufPlugin.instance = this
  
  colorCache = collection.mutable.Map()

  colorDefs =
    Map[Symbol, Tuple3[String, String, Number]](
      'keyword -> ("Keywords", "128,0,128", SWT.BOLD),
      'operator -> ("Operators", "0,0,0", SWT.NORMAL),
      'comment -> ("Comments", "128,128,0", SWT.ITALIC)) ++
    PufConfig.colors

  override def initializeImageRegistry(registry: ImageRegistry) {
    super.initializeImageRegistry(registry)

    val bundle = org.eclipse.core.runtime.Platform.getBundle(
            PufConfig.pluginId);
    val addFun =
      (key: String, path: String) =>
        addImage(key, path, bundle, registry)
    PufConfig.initializeImages(addFun)
  }
}

class PufParseController extends 
  SimplicitasParseController(PufConfig.language, PufConfig.instance) {}

class PufTokenColorer extends TokenColorerBase(PufPlugin.factory,
                                                    PufConfig.instance) {}

class PufTreeModelBuilder
  extends SimplicitasTreeModelBuilder(PufConfig.instance) {
}

class PufLabelProvider
    extends SimplicitasLabelProvider(PufConfig.instance) {}

class PufReferenceResolver
    extends SimplicitasReferenceResolver(PufConfig.instance) {}

class PufFoldingUpdater
    extends SimplicitasFoldingUpdater(PufConfig.instance) {}

class PufDocumentationProvider
    extends SimplicitasDocumentationProvider(PufConfig.instance) {}

class PufContentProposer
    extends SimplicitasContentProposer(PufConfig.instance) {}

class PufOccurrenceMarker
    extends SimplicitasOccurrenceMarker(PufConfig.instance) {}

class PufPreferencePage
    extends SimplicitasPreferencePage(PufPlugin.factory) {}

class PufPreferenceInitializer
    extends SimplicitasPreferenceInitializer(PufPlugin.factory) {}

class PufGenerateAction extends GenerateAction(PufConfig.instance) {}