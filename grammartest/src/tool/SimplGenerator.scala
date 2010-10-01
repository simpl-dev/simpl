package ee.cyber.simplicitas.grammartest;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class SimplGenerator(destDir: String) 
        extends GeneratorBase(destDir) {
  val templates = getTemplates("Simpl.stg")
    
  def generate(tree: Program) {
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
      
      new SimplGenerator(destDir).generate(grammar.tree)        
    }
  }
}
