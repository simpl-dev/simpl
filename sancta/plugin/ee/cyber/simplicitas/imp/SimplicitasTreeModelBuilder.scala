// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp


import org.antlr.runtime.tree.CommonTree
import org.eclipse.imp.services.base.TreeModelBuilderBase

import ee.cyber.simplicitas._

/** Base class for tree model builders that determine which
    nodes are shown in the outline view. Override the addToTree method
    to return true on nodes that should be displayed in the outline view.
    The labels for these nodes are calculated by label provider. */
abstract class SimplicitasTreeModelBuilder(config: APluginConfig)
    extends TreeModelBuilderBase {
  override def visitTree(root: Object): Unit =
    if (root ne null) {
      buildTreeModel(root.asInstanceOf[CommonNode])
    }

  private def buildTreeModel(node: CommonNode): Unit = {
    val shouldAdd = addToTree(node)
    
    if (shouldAdd) {
      pushSubItem(node)
    }
    try {
      node.children foreach buildTreeModel
    } finally {
      if (shouldAdd) {
        popSubItem()
      }
    }
  }
  
  def addToTree(node: CommonNode): Boolean =
    config.addToTree(node)
}
