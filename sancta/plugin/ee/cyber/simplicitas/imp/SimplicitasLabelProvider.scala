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

object LabelStrings {
  val STATES_DEFAULT_OUTLINE_ITEM =
        SimplicitasPlugin.SIMPLICITAS_DEFAULT_OUTLINE_ITEM
  val STATES_FILE = SimplicitasPlugin.SIMPLICITAS_FILE
  val STATES_FILE_WARNING = SimplicitasPlugin.SIMPLICITAS_FILE_WARNING
  val STATES_FILE_ERROR = SimplicitasPlugin.SIMPLICITAS_FILE_ERROR
}

/** Label provider service. It can be configured by extending the
    APluginConfig class. */
abstract class SimplicitasLabelProvider(config: APluginConfig)
    extends ILabelProvider {
  private object Images {
    val registry = plugin.getImageRegistry
    
    val file = registry.get(LabelStrings.STATES_FILE)
    val fileWithWarning = registry.get(LabelStrings.STATES_FILE_WARNING)
    val fileWithError = registry.get(LabelStrings.STATES_FILE_ERROR)
    
    val defaultOutline = registry.get(LabelStrings.STATES_DEFAULT_OUTLINE_ITEM)
  }

  // Plumbing.
  def getImage(element: Object): Image =
    element match {
      case file: IFile =>
        getFileImage(file)
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

  /** Override to provide different images for files in navigator. */
  def getFileImage(file: IFile): Image = {
    val severity = MarkerUtils.getMaxProblemMarkerSeverity(
      file, IResource.DEPTH_ONE)
    severity match {
      case IMarker.SEVERITY_ERROR => Images.fileWithError
      case IMarker.SEVERITY_WARNING => Images.fileWithWarning
      case _ => Images.file
    }
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

  /**
   * Override to return the current plugin.
   */
  def plugin: org.eclipse.ui.plugin.AbstractUIPlugin

  // dummy methods until I figure out what these listeners are actually for.
  def addListener(listener: ILabelProviderListener): Unit = {}
  def dispose(): Unit = {}
  def isLabelProperty(element: Object, property: String): Boolean = false
  def removeListener(listener: ILabelProviderListener): Unit = {}
}
