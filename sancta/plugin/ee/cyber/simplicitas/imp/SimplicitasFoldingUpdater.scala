// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp

import java.util.{List, HashMap}

import org.eclipse.imp.services.base.FolderBase
import org.eclipse.jface.text.source.Annotation
import org.eclipse.jface.text.Position

import ee.cyber.simplicitas._

/** Base class for the folding updater service. The language developer
    must override the canMakeFoldable method. */
class SimplicitasFoldingUpdater(config: APluginConfig) extends FolderBase {
  def sendVisitorToAST(
      newAnnotations: HashMap[Annotation, Position],
      annotations: List[Annotation],
      ast: Object) = {
    if (ast ne null) {
      makeFoldingAnnotations(ast.asInstanceOf[CommonNode])
    }
  }

  private def makeFoldingAnnotations(node: CommonNode): Unit = {
    val shouldAdd = isFoldable(node)
    
    if (shouldAdd) {
      makeAnnotation(node)
    }
    node.children foreach makeFoldingAnnotations
  }
  
  /** Return true, if this node can be folded in the editor.
   *  This method should be overriden by the language developer.
   *  Default is to make node foldable, if it is added to tree model.
   */
  def isFoldable(node: CommonNode): Boolean =
    config isFoldable node
}
