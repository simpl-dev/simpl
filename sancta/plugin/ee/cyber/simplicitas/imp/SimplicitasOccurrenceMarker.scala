// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp

import scala.collection.mutable.ArrayBuffer
import org.eclipse.imp.language.ILanguageService
import org.eclipse.imp.parser.IParseController
import org.eclipse.imp.services.IOccurrenceMarker
import ee.cyber.simplicitas._

/** Base class for occurrence marker service. */
class SimplicitasOccurrenceMarker(config: APluginConfig)
    extends ILanguageService with IOccurrenceMarker {
  def getOccurrencesOf(parseCtrlr: IParseController,
                       entity: Object): java.util.List[Object] = {
    println("get occurrences - 1: " + entity)
    if (entity eq null)
      java.util.Collections.emptyList[Object]
    else {
      java.util.Arrays.asList(
        getOccurrencesOf(
          parseCtrlr, entity.asInstanceOf[CommonNode]).toArray:_*)
    }
  }

  def getKindName() = "References"
  
  def getOccurrencesOf(ctlr: IParseController,
                       node: CommonNode): Seq[CommonNode] = {
    println("get occurrences: " + node)

    var target = config referenceTarget node
    if (target eq null)
      target = node

    val tree = ctlr.getCurrentAst.asInstanceOf[CommonNode]
    val ret = new ArrayBuffer[CommonNode]()

    def addRef(n: CommonNode) {
      if ((target eq n) || (target eq (config referenceTarget n))) {
        ret += n
      }
    }

    tree walkTree addRef
    println("Found occurrences: " + ret)
    ret
  }
}
