// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.ide

import ee.cyber.simplicitas.eclipse.GenerateAction
import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.imp.parser._

import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.SWT


object SimplPlugin {
  var instance: SimplPlugin = null
  
  def getInstance =
    if (instance eq null)
      new SimplPlugin()
    else
      instance

  val factory = () => getInstance
}

class SimplPlugin extends SimplicitasPlugin {
  def getID = SimplConfig.pluginId
  def getLanguageID = SimplConfig.languageId

  SimplPlugin.instance = this
  
  colorCache = collection.mutable.Map()

  colorDefs =
    Map[Symbol, Tuple3[String, String, Number]](
      'keyword -> ("Keywords", "128,0,128", SWT.BOLD),
      'operator -> ("Operators", "0,0,0", SWT.NORMAL),
      'comment -> ("Comments", "128,128,0", SWT.ITALIC)) ++
    SimplConfig.colors

  override def initializeImageRegistry(registry: ImageRegistry) {
    super.initializeImageRegistry(registry)

    val bundle = org.eclipse.core.runtime.Platform.getBundle(
            SimplConfig.pluginId);
    val addFun =
      (key: String, path: String) =>
        addImage(key, path, bundle, registry)
    SimplConfig.initializeImages(addFun)
  }
}

class SimplParseController extends 
  SimplicitasParseController(SimplConfig.language, SimplConfig.instance) {}

class SimplTokenColorer extends TokenColorerBase(SimplPlugin.factory,
                                                 SimplConfig.instance) {}

class SimplTreeModelBuilder 
    extends SimplicitasTreeModelBuilder(SimplConfig.instance) {
}

class SimplLabelProvider
    extends SimplicitasLabelProvider(SimplConfig.instance) {}

class SimplReferenceResolver
    extends SimplicitasReferenceResolver(SimplConfig.instance) {}

class SimplFoldingUpdater
    extends SimplicitasFoldingUpdater(SimplConfig.instance) {}

class SimplDocumentationProvider
    extends SimplicitasDocumentationProvider(SimplConfig.instance) {}

class SimplContentProposer
    extends SimplicitasContentProposer(SimplConfig.instance) {}

class SimplOccurrenceMarker
    extends SimplicitasOccurrenceMarker(SimplConfig.instance) {}

class SimplPreferencePage
    extends SimplicitasPreferencePage(SimplPlugin.factory) {}

class SimplPreferenceInitializer
    extends SimplicitasPreferenceInitializer(SimplPlugin.factory) {}

class SimplGenerateAction extends GenerateAction(SimplConfig.instance) {}
