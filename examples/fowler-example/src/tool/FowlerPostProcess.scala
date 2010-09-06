package ee.cyber.simplicitas.fowlerdsl

import ee.cyber.simplicitas._

/**
 * Validates the program and does additional post-processing,
 * e.g. resolving links. The processing step updates errors
 * in the ctx variable.
 */
class FowlerPostProcess(val ctx: FowlerCtx) {
  import collection.mutable.Map

  def validate() {
    // Resolve links.
    ctx.tree.walkTree(resolve)
  }
  
  def resolve(node: CommonNode) {
    node match {
      case ref: EventRef =>
        resolveLink(ctx.events, "event", ref)
      case ref: CommandRef =>
        resolveLink(ctx.commands, "command", ref)
      case ref: StateRef =>
        resolveLink(ctx.states, "state", ref)
      case _ => ()
    }
  }

  def resolveLink[T <: NamedItem](map: Map[String, T], kind: String,
                                  node: Reference) {
    if (map.contains(node.id.text)) {
      node.ref = map(node.id.text)
    } else {
      ctx.errors += new SourceMessage(
          "Undefined " + kind + ": \"" + node.id.text + "\"",
          SourceMessage.Error, node.id)
    }
  }
}