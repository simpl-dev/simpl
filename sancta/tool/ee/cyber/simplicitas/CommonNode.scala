// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas

import util.parsing.input.Positional

/** This trait represents location in the source file.
  * It contains both the absolute location in source file
  * (attributes startIndex and endIndex) and human-readable
  * location (attributes line and column). */
trait SourceLocation {
    /** Character offset where this node starts. */
    def startIndex: Int

    /** Character offset where this node ends. */
    def endIndex: Int

    /** Length of the textual representation of this node. */
    def length = endIndex - startIndex + 1

    /** Source line that contains beginning of this node. */
    def startLine: Int

    /** Source line that contains end of this node. */
    def endLine: Int

    /** Source column that contains beginning of this node. */
    def startColumn: Int

    /** Source column that contains end of this node. */
    def endColumn: Int
}

/** Parent class for the nodes in the abstract representation (AST) of
  * DSL program. It provides access to location of the node in the
  * DSL text and also means for iterating over child nodes of this node.  */
trait CommonNode extends Product with SourceLocation with Positional {
    def childrenNames: Array[String]

    setPos(new MyPosition())

    /** Children of the current node. Note that this list
      * only contains AST nodes, not elements corresponding to concrete
      * syntax (i.e., tokens).  */
    def children: Seq[CommonNode] = {
        val ret = new collection.mutable.ArrayBuffer[CommonNode]
        for (index <- 0 to productArity - 1) {
            val e = productElement(index)
            e match {
                case node: CommonNode =>
                    ret += node
                case _::_ =>
                    ret ++= e.asInstanceOf[List[CommonNode]].filter (_ ne null)
                case _ => ()
            }
        }
        return ret
    }

    /** Walks through all the nodes in the tree (preorder),
      * invoking the function <code>f</code> on each node. */
    def walkTree(f: (CommonNode) => Unit): Unit = {
        // TODO: should it add test for cycles?

        f(this)
        children foreach (_.walkTree(f))
    }

    var startIndex = 0
    var endIndex = 0
    var startLine = 0
    var endLine = 0
    var startColumn = 0
    var endColumn = 0

    /** Parent node of this node. Useful e.g. for traversing scopes. */
    var parent: CommonNode = null

    /** Copies location from another node. */
    def setLocation(loc: SourceLocation): this.type = {
        if (loc ne null) {
            startIndex = loc.startIndex
            startLine = loc.startLine
            startColumn = loc.startColumn

            endIndex = loc.endIndex
            endLine = loc.endLine
            endColumn = loc.endColumn
        }

        this
    }

    /** Copies starting position from another node. */
    def setStart(loc: SourceLocation): this.type = {
        if (loc ne null) {
            startIndex = loc.startIndex
            startLine = loc.startLine
            startColumn = loc.startColumn
        }

        this
    }

    /** Copies endpoint position from another node. */
    def setEnd(loc: SourceLocation): this.type = {
        if (loc ne null) {
            endIndex = loc.endIndex
            endLine = loc.endLine
            endColumn = loc.endColumn
        }

        this
    }

    /** Set starting position. */
    def setStart(index: Int, line: Int, column: Int): this.type = {
        startIndex = index
        startLine = line
        startColumn = column

        this
    }

    /** Set ending position. */
    def setEnd(index: Int, line: Int, column: Int): this.type = {
        endIndex = index
        endLine = line
        endColumn = column

        this
    }

    def updateParents(myParent: CommonNode) {
        parent = myParent
        for (child <- children if child ne null) {
            child updateParents this
        }
    }

    /** Recursively converts this node to java.util.Map.
      * The result will contain only objects reachable through the
      * <code>children</code> property (additional properties will not be
      * included in the map).
      * If the parameter <code>typeAttr</code> is present, then the type
      * of the object will be included as additional attribute in the map.
      * The name of the attribute is the value of <code>typeAttr</code>
      * parameter. If the node already contains child with this name,
      * the child's value will be overriden. This parameter applies recursively
      * to all the children.
      */
    def toJavaMap(typeAttr: String = null): java.util.Map[String, Object] = {
        import java.util.{Map, Set, AbstractMap, HashMap, HashSet}
        type Entry = Map.Entry[String, Object]

        var index = 0
        val byName = new HashMap[String, Entry]()
        for (name <- childrenNames) {
            byName.put(name, new MapEntry(name, index, typeAttr))
            index += 1
        }
        if (typeAttr ne null) {
            byName.put(typeAttr, new TypeEntry(typeAttr, this))
        }
        val entries = new HashSet[Entry](byName.values)
        new AbstractMap[String, Object]() {
            def entrySet(): Set[Map.Entry[String, Object]] =
                entries

            def containsKey(key: String): Boolean =
                byName containsKey key

            def get(key: String): Object = {
                val v = byName get key
                if (v eq null)
                    null
                else
                    v.getValue
            }

            override def put(key: String, value: Object): Object = {
                val entry = new MapEntry(key, 0, typeAttr) {
                    override def getValue() = value
                }
                entries.add(entry)
                val res = byName.put(key, entry)
                if (res eq null)
                    null
                else
                    res.getValue
            }
        }
    }

    private class MapEntry(name: String, index: Int, typeAttr: String)
            extends java.util.Map.Entry[String, Object] {
        private var old: AnyRef = null
        private var value: Object = null

        def getKey() =
            name

        def forJava(v: AnyRef): Object = v match {
            case node: CommonNode => node.toJavaMap(typeAttr)
            case list: List[AnyRef] =>
                java.util.Arrays.asList((list.toArray map forJava):_*)
            case other => other
        }

        def getValue(): Object = {
            val raw = productElement(index) match {
                case v: AnyRef => v
                case _ => return null
            }
            if (raw ne old) {
                value = forJava(raw)
                old = raw
            }
            value
        }

        def setValue(v: Object) =
            throw new UnsupportedOperationException

        override def hashCode() =
            name.hashCode

        override def equals(o: Any) = o match {
            case entry: MapEntry =>
                name == entry.getKey
            case _ => false
        }
    }

    private class TypeEntry(key: String, obj: Object)
            extends java.util.Map.Entry[String, Object] {
        def getKey = key
        def getValue = baseName(obj.getClass.getName)
        override def hashCode = getValue.hashCode
        def setValue(v: Object) =
            throw new UnsupportedOperationException

        def baseName(s: String) = {
            val idx = s.lastIndexOf('.')
            if (idx == -1)
                s
            else
                s.substring(idx + 1)
        }
    }

    private class MyPosition extends scala.util.parsing.input.Position {
        def line = startLine
        def column = startColumn
        def lineContents = "NOT IMPLEMENTED"
    }
}

/** Parent class for all the terminal nodes. */
trait TerminalNode extends CommonNode {
    override def children = Nil // No children on terminals

    /** Returns contents of the terminal node. */
    def text: String
}

/** Generic node class for all the literals that assigned a value
  * (for example, by construct foo="bar"). */
case class LiteralNode(text: String) extends TerminalNode {
    def childrenNames = Array("text");
}

/** Represents token in the DSL source file.
  * Additionally there is automatically generated kind method that
  * returns token kind (from parser). The boolean methods are somewhat
  * orthogonal to token kinds. For example, there can be several token kinds
  * that represent keywords. The methods will have default implementation
  * generated by the parser generator, but the user can override them. */
trait GenericToken extends SourceLocation {
    /** Text that the token contains. */
    def text: String

    /** Whether the token is keyword. */
    def isKeyword: Boolean
    /** Whether the token is ignored by the parser (white space or comment). */
    def isHidden: Boolean
    /** Whether the token is punctuation or operator. */
    def isOperator: Boolean
    /** Whether the token is comment. */
    def isComment: Boolean

    /** Whether this token is of given kind. */
    def ofKind(kind: Any): Boolean
}

/** Represents tkoen in the DSL souroce file. The attribute <code>kind</code>
  * corresponds to token kind from lexer. There will be automatically
  * generated enumeration that will contain all the token kinds present in
  * a given DSL */
trait CommonToken[Kind] extends GenericToken {
    /** Token kind, determined by lexer. */
    def kind: Kind

    def ofKind(k: Any): Boolean =
        return kind == k
}
