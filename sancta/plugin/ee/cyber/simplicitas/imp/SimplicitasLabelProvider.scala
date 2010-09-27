// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp

import java.util.{HashSet, Set}

import org.eclipse.core.resources.{IFile, IMarker, IResource}
import org.eclipse.imp.editor.ModelTreeNode
import org.eclipse.imp.services.ILabelProvider
import org.eclipse.imp.utils.MarkerUtils
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.jface.viewers.ILabelProviderListener
import org.eclipse.swt.graphics.Image

import ee.cyber.simplicitas._

/** Label provider service. It can be configured by extending the
    APluginConfig class. */
abstract class SimplicitasLabelProvider(config: APluginConfig)
    extends ILabelProvider {
  // Plumbing.
  def getImage(element: Object): Image =
    element match {
      case file: IFile => {
        val img = config.fileImage(file)
        if (img ne null)
          img
        else
          SimplicitasPlugin.getFileImage
      }
      case treeNode: ModelTreeNode =>
        config.treeImage(treeNode.getASTNode.asInstanceOf[CommonNode])
      case tree: CommonNode =>
        config.treeImage(tree)
      case _ => null
    }

  def getText(element: Object): String =
    element match {
      case treeNode: ModelTreeNode =>
        getOutlineText(treeNode.getASTNode.asInstanceOf[CommonNode])
      case tree: CommonNode =>
        getOutlineText(tree)
      case _ => null
    }

  /** Override to provide text for outline node.*/
  def getOutlineText(node: CommonNode): String = {
    if (node eq null)
        return "N/A"
    val label = config treeLabel node
    if (label eq null)
      node.toString
    else
      label
  }

  // dummy methods until I figure out what these listeners are actually for.
  def addListener(listener: ILabelProviderListener): Unit = {}
  def dispose(): Unit = {}
  def isLabelProperty(element: Object, property: String): Boolean = false
  def removeListener(listener: ILabelProviderListener): Unit = {}
}
