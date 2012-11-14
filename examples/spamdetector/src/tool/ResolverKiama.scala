package ee.cyber.simplicitas.spamdetector

import ee.cyber.simplicitas.CommonNode
import org.kiama.attribution.Attribution._

object ResolverKiama {
    def callGraph(program: Program): Map[String, Set[String]] =
        program.items.map(callGraph).flatten.toMap

    val callGraph: Item => Map[String, Set[String]] =
        attr {
            case Condition(Id(name), expr) =>
                Map(name -> callSet(expr))
            case _ => Map.empty
        }

    val callSet: CommonNode => Set[String] =
        attr {
            case ConditionCall(Id(name)) => Set(name)
            case node =>
                node.children.map(callSet).flatten.toSet
        }
}