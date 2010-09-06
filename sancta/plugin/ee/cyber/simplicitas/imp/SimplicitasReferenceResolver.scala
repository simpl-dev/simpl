// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp

import org.eclipse.imp.parser.IParseController
import org.eclipse.imp.services.IReferenceResolver

import ee.cyber.simplicitas._

/** Base class for the reference resolving service. */
class SimplicitasReferenceResolver(config: APluginConfig)
    extends IReferenceResolver {
  def getLinkText(node: Object): String =
    config getLinkText node.asInstanceOf[CommonNode]

  def getLinkTarget(node: Object, controller: IParseController): Object = {
    if (node eq null)
      return null

    getLinkTarget(node.asInstanceOf[CommonNode], controller)
  }

  /** Override this to return where the node points to. */
  def getLinkTarget(node: CommonNode,
                    controller: IParseController): CommonNode =
    config referenceTarget node
}
