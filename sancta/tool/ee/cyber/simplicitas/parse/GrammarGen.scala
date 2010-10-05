// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class GrammarException(msg: String) extends Exception(msg)

class GrammarGen(posMap: Any => List[Int]) {
    def grammargen(tree: Any) {
    }
    def grammarName = "foo"
    def treeSource = "foo"
    def grammarSource = "foo"
}
