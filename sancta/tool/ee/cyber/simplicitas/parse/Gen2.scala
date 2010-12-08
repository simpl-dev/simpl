package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import GrammarUtils._
import Actions._

trait STable {
    def rules: collection.mutable.Map[String, Rule]
    def classes: collection.mutable.Map[String, RClass]
    def actions: ActionSet

    /** Map from kw contents to name of rule. */
    def keywords: collection.mutable.Map[String, String]

    def newId: String
}

class Gen2(getPos: (Any) => List[Int]) {
    object Symbols extends STable {
        val rules = collection.mutable.Map[String, Rule]()
        val classes = collection.mutable.Map[String, RClass]()
        val actions = new ActionSet
        val keywords = collection.mutable.Map[String, String]()

        private var idVal = 0

        def newId = {idVal += 1; "Z" + idVal}
    }

    import Symbols._
    
    var grammarName: String = null
    var grammarPackage: String = null

    def grammargen(tree: Any) {
        tree match {
            case ("grammar" :: nameParts) :: rest =>
                matchGrammarName(nameParts, tree)
                rest.foreach(addRule)
        }

        rules.values.foreach(_.analyze())

        // Do the class generation and type inference.
        for (r <- rules.values) {
            r.generateClasses()
        }

        // Rules are generated, let's run delayed actions
        for ((r, aSet) <- actions) {
            aSet.foreach(_(classes(r)))
        }

        cleanupExtends()

        rules.foreach(println)

        println("Classes: \n" + classes.values.mkString("\n"))
        println("Keywords: " + keywords)
    }

    /** Adds rule to symbol table. */
    def addRule(rule: Any) = rule match {
        case "terminal" :: "hidden" :: (name: String) :: rest =>
            rules(name) = new TerminalRule(name, true, rest, Symbols)
        case "terminal" :: (name: String) :: rest =>
            rules(name) = new TerminalRule(name, false, rest, Symbols)
        case "fragment" :: (name: String) :: rest =>
            rules(name) = new FragmentRule(name, rest, Symbols)
        case "option" :: (name: String) :: rest =>
            rules(name) = new OptionRule(name, rest, Symbols)
        case ":" :: (name: String) :: rest =>
            rules(name) = new NormalRule(name, rest, Symbols)
        case _ =>
            throw new Exception("Invalid rule: " + rule)
    }

    /** Cleans up the extends declarations:
      * Detect cycles
      * Remove "A extends A"
      * If "A extends B with C" and "B extends C" => "A extends B"
      */
    def cleanupExtends() {
        // TODO: detect cycles.

        def reachableSet(items: collection.mutable.Set[String]):
                collection.mutable.Set[String] = {
            val newItems = items.flatMap(
                    (name: String) =>
                        if (classes.contains(name))
                            classes(name).extend
                        else
                            Nil)

            if (newItems.forall(items.contains(_)))  // No new classes
                items
            else
                reachableSet(items ++ newItems)
        }

        for (cl <- classes.values) {
            cl.extend -= cl.name

            for (ext <- cl.extend.toList) {
                if (reachableSet(cl.extend - ext).contains(ext)) {
                    cl.extend -= ext
                }
            }
        }
    }

    /** Parses the full name of the grammar. Fills grammarName and
      * grammarPackage variables. */
    def matchGrammarName(nameParts: List[Any], tree: Any) {
        nameParts.reverse match {
            case (name: String) :: "." :: pname =>
                grammarName = name
                grammarPackage = (pname.reverse foldLeft "")(_+_)
            case List(name: String) => 
                grammarName = name
            case _ =>
                throw new Exception("no grammar name")
//                error(tree, "no grammar name")
        }
    }

    def getTreeSource = {
        val ret = new StringBuilder()
                val treeSrc = new StringBuilder()

        ret append ("package " + grammarPackage + ";\n\n" +
                    "import ee.cyber.simplicitas." +
                        "{CommonNode, CommonToken, TerminalNode, LiteralNode}\n" +
                    "import ee.cyber.simplicitas.parse." +
                        "{ErrorHandler}\n\n")

        for (c <- classes.values) {
            c.generate(ret)
        }

        ret.toString
    }
}