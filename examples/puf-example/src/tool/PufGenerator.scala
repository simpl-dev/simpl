package ee.cyber.simplicitas.puf;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class PufGenerator(destDir: String) 
        extends GeneratorBase(destDir) {
  val templates = getTemplates("Puf.stg")
    
  def generate(tree: Program) {
    val args = tree.toJavaMap()
    writeFile("GeneratedProgram.java", templates.getInstanceOf("program", args))
  }
}
  
object PufMain extends MainBase {
  def main(argv: Array[String]) {
    parseOptions(argv)
    val grammar = new PufGrammar()
    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)
      
      new PufGenerator(destDir).generate(grammar.tree)        
    }
  }
}
