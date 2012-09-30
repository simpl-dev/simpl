package ee.cyber.simplicitas.expattributes;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class ExpGenerator(destDir: String) 
        extends GeneratorBase(destDir) {
  val templates = getTemplates("Exp.stg")
    
  def generate(tree: Program) {
    val args = tree.toJavaMap()
    writeFile("GeneratedProgram.java", templates.getInstanceOf("program", args))
  }
}
  
object ExpMain extends MainBase {
  def main(argv: Array[String]) {
    parseOptions(argv)
    val grammar = new ExpGrammar()
    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)
      
      new ExpGenerator(destDir).generate(grammar.tree)        
    }
  }
}
