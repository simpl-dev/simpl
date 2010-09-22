package ee.cyber.simplicitas.fowlerdsl

import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.{GenericToken, CommonNode}

import org.eclipse.imp.language.LanguageRegistry
import org.eclipse.swt.SWT


object FowlerConfig {
  val instance = new FowlerConfig()
  val languageId = "fowler_statemachine"
  val language = LanguageRegistry.findLanguage(languageId)
  val pluginId = "fowler_statemachine"

  val colors: Map[Symbol, Tuple3[String, String, Number]] =
    Map.empty
}

class MyGrammar extends FowlerGrammar {
  var ctx: FowlerCtx = null
}

class FowlerConfig extends APluginConfig {
    // parses using some grammar
  def parse(ctx: ParseCtx) {
    val grammar = new MyGrammar()

    if (ctx parse grammar) {
      grammar.ctx = new FowlerCtx(grammar.tree)
      new FowlerPostProcess(grammar.ctx).validate()
      ctx.reportErrors(grammar.ctx.errors)
    }
  }

  // labels for nodes that belong to the tree model
  def treeLabel(node: CommonNode) = node match {
    case command: Command => command.name.text
    case event: Event => event.name.text
    case state: State => "State " + state.name.text
    case _: ResetEvents => "Reset events"
    case _: CommandList => "Commands"
    case _: EventList => "Events"
    case _ => null
  }
  
  override def isFoldable(node: CommonNode) = node match {
    case _: State => true
    case _: CommandList => true
    case _: EventList => true
    case _ => false
  }
  
  override def referenceTarget(node: CommonNode): CommonNode = node match {
    case id: Id =>
      id.parent match {
        case ref: Reference =>
          ref.ref.name
        case item: NamedItem if id == item.name =>
          id // named items name references to itself
        case _ => null
      }
    case ref: Reference => // not sure whether this is required
      ref.ref.name
    case _ => null
  }

  private def getDoc(node: CommonNode) = node match {
    case command: Command => "Command " + command.name.text
    case event: Event => "Event " + event.name.text
    case state: State => "State " + state.name.text
    case _ => null
  }

  override def getDocumentation(node: CommonNode): String =
    node match {
      case id: Id => getDoc(node.parent)
      case _ => getDoc(node)
    }

  override def singleLineCommentPrefix = "//"
  
  override def propose(ctx: ProposalCtx) {
    val fowlerCtx = ctx.grammar.asInstanceOf[MyGrammar].ctx
    
    (ctx.node, ctx.parent) match {
      case (Id(_), EventRef(_)) =>
        addProposals(ctx, fowlerCtx.events.keys)
      case (Id(_), StateRef(_)) =>
        addProposals(ctx, fowlerCtx.states.keys)
      case (Id(_), CommandRef(_)) =>
        addProposals(ctx, fowlerCtx.commands.keys)
      case _ => ()
    }
  }
  
  def addProposals(ctx: ProposalCtx, items: Iterable[String]) {
    for (name <- items) {
      if ((ctx.prefix eq null) ||
            ctx.prefix == "" ||
            name.startsWith(ctx.prefix)) {
        ctx.add(name)
      }
    }
  }
  
  override def runGenerator(dir: String, file: String) {
    FowlerMain.main(Array("--dest", dir, dir + file))
  }
}
