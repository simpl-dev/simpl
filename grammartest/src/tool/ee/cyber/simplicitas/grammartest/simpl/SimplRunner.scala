package ee.cyber.simplicitas.grammartest.simpl;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class SimplRunner(destDir: String) 
        extends GeneratorBase(destDir) {
  val templates = getTemplates("Simpl.stg")
    
  def generate(tree: GrammarDef) {
    val args = tree.toJavaMap()
    writeFile("GeneratedProgram.java", templates.getInstanceOf("program", args))
  }
}
  
object SimplMain extends MainBase {
  def main(argv: Array[String]) {
    parseOptions(argv)
    val grammar = new SimplGrammar()
    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)
      
      new SimplRunner(destDir).generate(grammar.tree)        
    }
  }
}
