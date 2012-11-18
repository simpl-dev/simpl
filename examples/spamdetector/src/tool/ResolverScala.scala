package ee.cyber.simplicitas.spamdetector

import ee.cyber.simplicitas.SourceMessage
import collection.mutable.ArrayBuffer
import collection.mutable.Map

import SourceMessage.error

class ResolverScala {
    val errors = ArrayBuffer[SourceMessage]()
    val conditions = Map[String, Id]()

    def resolveReferences(program: Program) {
        errors.clear()
        conditions.clear()

        collectConditions(program)
        doResolve(program)
        checkCycles(program)
    }

    private def collectConditions(program: Program) {
        program.items foreach {
            case Condition(id @ Id(name), _) =>
                if (conditions contains name) {
                    errors += error("Duplicate condition name: " +
                            name, id)
                } else {
                    conditions += name -> id
                }
            case _ => () // Do nothing
        }
    }

    private def doResolve(program: Program) {
        program walkTree {
            case ConditionCall(id @ Id(name)) =>
                if (conditions contains name) {
                    id.ref = conditions(name)
                } else {
                    errors += error("Condition not found: " +
                            name, id)
                }
            case _ => ()
        }
    }

    private def checkCycles(program: Program) {
        val graph = callGraph(program)

        def check(condToCheck: String, blacklist: Set[String]) {
            val called = graph(condToCheck)
            val intersect = called & blacklist
            if (intersect.isEmpty) {
                called foreach(check(_, blacklist ++ called + condToCheck))
            } else {
                errors += error(
                    "Condition " + condToCheck + " creates endless loop",
                    conditions(condToCheck))
            }
        }

        graph.keys foreach (cond => check(cond, Set(cond)))
    }

    private def callGraph(program: Program) = {
        val calls = Map[String, Set[String]]()

        program.items foreach {
            case cond @ Condition(Id(name), _) =>
                calls += name -> callSet(cond)
            case _ => () // No callsets for rules
        }
        calls
    }

    private def callSet(condition: Condition) = {
        var calls = Set[String]()
        condition walkTree {
            case ConditionCall(Id(calledCond)) =>
                calls += calledCond
            case _ => ()
        }
        calls
    }
}