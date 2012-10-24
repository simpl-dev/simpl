// This file contains generated code. You are not supposed to edit it.
// Instead, use the file SDConfig.scala to customize your DSL IDE.

package ee.cyber.simplicitas.spamdetector

import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.imp.parser._
import ee.cyber.simplicitas.eclipse.GenerateAction

import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.SWT


object SDPlugin {
  var instance: SDPlugin = null
  
  def getInstance =
    if (instance eq null)
      new SDPlugin()
    else
      instance
  
  val factory = () => getInstance
}

class SDPlugin extends SimplicitasPlugin {
  def getID = SDConfig.pluginId
  def getLanguageID = SDConfig.languageId

  SDPlugin.instance = this
  
  colorCache = collection.mutable.Map()

  colorDefs =
    Map[Symbol, Tuple3[String, String, Number]](
      'keyword -> ("Keywords", "128,0,128", SWT.BOLD),
      'operator -> ("Operators", "0,0,0", SWT.NORMAL),
      'comment -> ("Comments", "128,128,0", SWT.ITALIC)) ++
    SDConfig.colors

  override def initializeImageRegistry(registry: ImageRegistry) {
    super.initializeImageRegistry(registry)

    val bundle = org.eclipse.core.runtime.Platform.getBundle(
            SDConfig.pluginId);
    val addFun =
      (key: String, path: String) =>
        addImage(key, path, bundle, registry)
    SDConfig.initializeImages(addFun)
  }
}

class SDParseController extends 
  SimplicitasParseController(SDConfig.language, SDConfig.instance) {}

class SDTokenColorer extends TokenColorerBase(SDPlugin.factory,
                                                    SDConfig.instance) {}

class SDTreeModelBuilder
  extends SimplicitasTreeModelBuilder(SDConfig.instance) {
}

class SDLabelProvider
    extends SimplicitasLabelProvider(SDConfig.instance) {}

class SDReferenceResolver
    extends SimplicitasReferenceResolver(SDConfig.instance) {}

class SDFoldingUpdater
    extends SimplicitasFoldingUpdater(SDConfig.instance) {}

class SDDocumentationProvider
    extends SimplicitasDocumentationProvider(SDConfig.instance) {}

class SDContentProposer
    extends SimplicitasContentProposer(SDConfig.instance) {}

class SDOccurrenceMarker
    extends SimplicitasOccurrenceMarker(SDConfig.instance) {}

class SDPreferencePage
    extends SimplicitasPreferencePage(SDPlugin.factory) {}

class SDPreferenceInitializer
    extends SimplicitasPreferenceInitializer(SDPlugin.factory) {}

class SDGenerateAction extends GenerateAction(SDConfig.instance) {}