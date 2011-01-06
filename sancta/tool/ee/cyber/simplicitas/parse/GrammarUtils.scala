// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer


object Actions {
    type Action = (RuleClass) => Unit
    class ActionSet extends
        collection.mutable.HashMap[String, collection.mutable.Set[Action]]
           with collection.mutable.MultiMap[String, Action]

    def addExtend(cl: String): (RuleClass) => Unit =
        (rule: RuleClass) => rule.extend += cl
}

/** Contains the symbols that are found in the grammar. */
trait SymbolTable {
    /** Grammar rules (both terminal and nonterminal.
      * Indexed by the rule name. */
    def rules: collection.mutable.Map[String, Rule]

    /** Classes that will be generated, indexed by class name. */
    def classes: collection.mutable.Map[String, RuleClass]

    /** Actions that need to be run after generating classes. */
    def actions: Actions.ActionSet

    /** Keywords supported by this language. Map is from keyword
      * to identifier representing the corresponding lexer rule. */
    def keywords: collection.mutable.Map[String, String]

    /** Returns position of the given node.
      * Strictly does not belong to symbol table, but it is convenient to
      * pass it around along with other global-ish information. */
    def getPos: (Any) => List[Int]

    /** Name of the grammar. Also here for convenience. */
    def getGrammarName: String

    /** Returns new unique identifier. */
    def newId: String
}

class GrammarException(msg: String) extends Exception(msg)

object BranchIdentifier {
    /** The initial value for branch identifiers. */
    val empty = new BranchIdentifier(List(0))
}

/**
 * The code tries to ensure that there are no name clashes for rule calls
 * in different branches. For example, this is legal rule:
 *
 * Foo: x=Bar y=Baz | x=Bar z=Bag;
 *
 * Although the variable x is present in both options, it has the same type
 * and can therefore safely be used. This is incorrect:
 *
 * Foo: x=Bar y=Baz | x=Baz z=Bag;
 *
 * Now x is type Bar in one branch and Baz in another.
 *
 * The important point is to determine which uses of variable would clash
 * (variable is used two times in the same branch). For example, consider
 * the following rule (for brevity, here assignments are not used for
 * sub-rule calls):
 *
 * Foo: A ((B C) | (D (E | F)?));
 *
 * In this case, A would clash with all the other rule calls, but B and E,
 * for example, do not clash. However, D and F clash. To determine, which
 * subrule calls clash, each branch in the rule is assigned an identifier.
 * If the same variable us used several times in a rule, then the identifiers
 * are compared to see whether the uses of this variable clash.
 *
 * Branch identifers are lists of integers that represents path to a sub-rule
 * call. For example, identifiers for the previous rule, are:
 *
 * A: [0]
 * B: [0, 0]
 * C: [0, 0]
 * D: [0, 1]
 * E: [0, 1, 0]
 * F: [0, 1, 1]
 *
 * Two sub-rule calls with identifiers I1 and I2 clash if I1 is prefix of
 * I2 or vice versa.
 */
class BranchIdentifier(val branch: List[Int]) {
    /** Returns identifier for the sibling branch. */
    def nextBranch =
        new BranchIdentifier((branch dropRight 1) ++ List(branch.last + 1))

    /** Adds one level to the branch identifier. */
    def extend = new BranchIdentifier(branch ++ List(0))

    /** Returns true, if this branch conflicts with other branch. */
    def conflictsWith(other: BranchIdentifier) =
        (other.branch.zip(branch)) forall
                        ((a: (Int, Int)) => a._1 == a._2)

    override def toString = branch.toString
}

/** Various utility methods useful for grammar generation. */
object GrammarUtils {
    /** Report an error related to node. */
    def error(posMap: Any => List[Int])(node: Any, what: String) =
        throw new GrammarException(posMap(node) match {
                case List(line, col) =>
                    line + ":" + col + ": " + what
                case _ =>
                    what
            })

    /** Make the first character of the string lowercase. */
    def uncapitalize(s: String): String =
        if (s == "")
            ""
        else
            (Character toLowerCase (s charAt 0)) + (s substring 1)

    /** Convenience wrapper for @link Iterable.mkString. */
    def join(i: Iterable[Any]) = i.mkString(", ")

    /** Strips one character from beginning and end of string <code>s</code>. */
    def stripQuotes(s: String) =
        if ((s ne null) && (s != ""))
            s.substring(1, s.length - 1)
        else
            ""

    /** Helper function for generating grammar code. Walks through list of
     * options (pattern in the form of "foo | bar | baz").
     * For each found option, calls the given function and outputs
     * the modifier (*, +, ?, ~) to the grammar output.
     */
    def doOptionList(fun: Any => Unit, lst: List[Any])
            (implicit buf: ArrayBuffer[String]) {
        var first = true

        for ("NODE" :: matches <- lst) {
            if (!first) {
                buf += "\n    | "
            }

            for (m <- matches) {
                m match {
                    // Foo
                    case List("MATCH", ruleCall) =>
                        fun(ruleCall)
                    // !Foo
                    case List("MATCH", "~", ruleCall) =>
                        buf += "~"
                        fun(ruleCall)
                    // Foo+ or Foo*
                    case List("MATCH", modifier: String, ruleCall) =>
                        fun(ruleCall)
                        buf += modifier
                    // ~Foo+ or ~Foo*
                    case List("MATCH", modifier: String, "~", ruleCall) =>
                        buf += "~"
                        fun(ruleCall)
                        buf += modifier
                    case _ =>
                        throw new GrammarException("Invalid syntax tree: " + m)
                }
            }

            first = false
        }
    }
}