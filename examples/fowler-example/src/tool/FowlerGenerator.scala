package ee.cyber.simplicitas.fowlerdsl

//import ee.cyber.simplicitas._
import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class FowlerGenerator(destDir: String) extends GeneratorBase(destDir) {
  val templates = getTemplates("Fowler.stg")
  
  def generate(ctx: FowlerCtx) {
    for (machine <- ctx.tree.machines) {
      expandResetEvents(machine)
      
      val args = machine.toJavaMap()
      val template = templates.getInstanceOf("program", args)
      writeFile(machine.name.text + ".java", template.toString)
    }
  }
  
  /** Automatically add transitions to reset events. */
  def expandResetEvents(machine: Machine) {
    def isInitState(state: State) =
      state == machine.initState.ref
    def containsEvent(evt: NamedItem)(transition: Transition) =
      transition.event.ref == evt
    
    for (evt <- machine.resetEvents.events) {
      for (state <- machine.states) {
        if (!isInitState(state) &&
              !state.transitions.exists(containsEvent(evt.ref))) {
          state.transitions = Transition(
            EventRef(evt.id),
            StateRef(machine.initState.id)) :: state.transitions
        }
      }
    }
  }
}

object FowlerMain extends MainBase {
  def main(argv: Array[String]) {
    parseOptions(argv)
    
    val grammar = new FowlerGrammar()

    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)
      
      val ctx = new FowlerCtx(grammar.tree)
      new FowlerPostProcess(ctx).validate()
      checkErrors(ctx.errors)
      new FowlerGenerator(destDir).generate(ctx)
    }
  }
}
