// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp

import org.eclipse.imp.parser.IParseController
import org.eclipse.imp.services.IDocumentationProvider

import ee.cyber.simplicitas._

/** Base class for documentation providers. */
class SimplicitasDocumentationProvider(config: APluginConfig)
    extends IDocumentationProvider {
  def getDocumentation(entity: Object, ctlr: IParseController): String =
    getDocumentation(entity.asInstanceOf[CommonNode], ctlr)

  /** The language implementer must override this method to provide
      documentation for the given AST node. */
  def getDocumentation(entity: CommonNode, ctlr: IParseController): String =
    config getDocumentation entity
}
