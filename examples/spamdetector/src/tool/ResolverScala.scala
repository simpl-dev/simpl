package ee.cyber.simplicitas.spamdetector

import ee.cyber.simplicitas.SourceMessage
import collection.mutable.ArrayBuffer

import SourceMessage.error

class ResolverScala {
    val errors = ArrayBuffer[SourceMessage]()
    val conditions = collection.mutable.Map[String, Id]()

    def resolveReferences(program: Program) {
        errors.clear()
        conditions.clear()

        collectConditions(program)
        doResolve(program)
    }

    private def collectConditions(program: Program) {
        program.items foreach {
            case Condition(name, _) =>
                if (conditions contains name.text) {
                    errors += error(
                        "Duplicate condition name: " + name.text, name)
                } else {
                    conditions += name.text -> name
                }
            case _ => () // Do nothing
        }
    }

    private def doResolve(program: Program) {
        program walkTree {
            case ConditionCall(name) =>
                if (conditions contains name.text) {
                    name.ref = conditions(name.text)
                } else {
                    errors += error("Condition not found: " + name.text, name)
                }
            case _ => ()
        }
    }
}