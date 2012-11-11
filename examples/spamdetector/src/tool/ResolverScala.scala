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
}