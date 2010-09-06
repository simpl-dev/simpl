// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp.parser

import org.eclipse.imp.parser.ISourcePositionLocator
import org.eclipse.imp.editor.ModelTreeNode

import ee.cyber.simplicitas._

class SimplicitasSourcePositionLocator(controller: SimplicitasParseController)
    extends ISourcePositionLocator {
  def getPath(node: Object) = {
    // TODO: get path from node.
    val x = controller.getProject.getRawProject.getFile(controller.getPath).getFullPath
    println("getpath(" + node + ") = " + x)
    x
    }

  class Extent(val begin: Int, val end: Int) {
    val length = end - begin;
    def contains(start: Int, stop: Int): Boolean = 
      start >= begin && stop <= end;
  }

  def getLength(node: Object): Int = getExtent(node).length;
  def getEndOffset(node: Object): Int = getExtent(node).end;
  def getStartOffset(node: Object): Int = getExtent(node).begin;


  private def getExtent(entity: Object): Extent = {
//    println("getExtent: " + entity)
    if (entity eq null)
      new Extent(-1, -1)
    else {
//      println("entity class: " + entity.getClass.getName)
      entity match {
        case token: GenericToken =>
          new Extent(token.startIndex, token.endIndex)
        case tree: CommonNode =>
//          println("get tree: " + tree.startIndex + ":" + tree.endIndex)
          new Extent(tree.startIndex, tree.endIndex);
        case modelNode: ModelTreeNode => 
          getExtent(modelNode.getASTNode)
        case any: Object => throw new IllegalArgumentException(
            "Invalid token class: " + any.getClass.getName);
      }
    }
  }

  def findNode(astRoot: Object, offset: Int): Object = 
    findNode(astRoot, offset, offset);

  def findNode(astRoot: Object, start: Int, end: Int): Object = {
    if (astRoot eq null)
      return null;

	val rootExtent = getExtent(astRoot)
	if (rootExtent.contains(start, end)) {
	  // otsime innermost'i
      for (child <- astRoot.asInstanceOf[CommonNode].children) {
        val node = findNode(child, start, end)
        if (node ne null) {
           return node
         }
      }

	  return astRoot // ilmselt olimegi innermost
	}

	return null;
  }
}
