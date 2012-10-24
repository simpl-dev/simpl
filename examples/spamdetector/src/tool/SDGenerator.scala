package ee.cyber.simplicitas.spamdetector;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class SDGenerator(destDir: String) 
        extends GeneratorBase(destDir) {
  val templates = getTemplates("SD.stg")
    
  def generate(tree: Program) {
    val args = tree.toJavaMap()
    writeFile("GeneratedProgram.java", templates.getInstanceOf("program", args))
  }
}
  
object SDMain extends MainBase {
  def main(argv: Array[String]) {
    parseOptions(argv)
    val grammar = new SDGrammar()
    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)
      
      new SDGenerator(destDir).generate(grammar.tree)        
    }
  }
}
