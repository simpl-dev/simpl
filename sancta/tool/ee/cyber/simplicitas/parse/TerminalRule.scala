package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import GrammarUtils._

class FragmentRule(pName: String, pTree: List[Any], symbols: STable)
        extends Rule(pName, pTree, symbols) {
    def collectParams() {}

    override def generateClasses() = super.generateClasses()
}

class TerminalRule(pName: String, hidden: Boolean, pTree: List[Any],
        symbols: STable) extends Rule(pName, pTree, symbols) {
    import symbols._

    def collectParams() {}

    override def generateClasses() {
        // Hidden rules do not contribute to the AST and therefore
        // should generate no classes.
        if (!hidden) {
            val newClass = new RClass(name, "case class", body)
            newClass.extend += "TerminalNode"
            newClass.params += new RCParam("text", "String")
            classes(name) = newClass
        }

        super.generateClasses()
    }
}

class LiteralRule(pName: String, text: String, symbols: STable)
        extends Rule(pName, List(List("NODE", stripQuotes(text))), symbols) {
    returnType = "LiteralNode"

    def collectParams() {}
    // Literal rules do not generate any classes.
    override def generateClasses() {}
}