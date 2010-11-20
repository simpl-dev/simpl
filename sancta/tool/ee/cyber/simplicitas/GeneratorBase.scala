// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas

import java.io.{File, FileWriter}
import org.antlr.stringtemplate.{AttributeRenderer,
            StringTemplate, StringTemplateGroup}

class StringRenderer extends AttributeRenderer {
    def toString(v: Object): String =
        v.asInstanceOf[String]

    def toString(v: Object, formatName: String): String = {
        val str = v.asInstanceOf[String]
        if (str == null || str == "") {
            return ""
        }
        formatName match {
            case "upperFirst" =>
                (Character toUpperCase (str charAt 0)) + str.substring(1)
            case "lowerFirst" =>
                (Character toLowerCase (str charAt 0)) + str.substring(1)
            case "upper" =>
                str.toUpperCase
            case "lower" =>
                str.toLowerCase
            case packageToPath =>
                str.replace('.', File.separatorChar)
        }
    }
}

/**
 * Base class for StringTemplate-based code generators. Contains useful
 * functions for managing files and templates.
 */
class GeneratorBase(destDir: String) {
  var options: List[String] = Nil

  def this() {
    this(".")
  }
  
  /**
   * Creates a named file with given contents. The name of the file
   * is relative to the destDir attribute. Any intermediate directories
   * will be created when necessary.
   * @return full path of the newly created file.
   */
  def writeFile(name: String, contents: String): String = {
    val file = new File(
        (if (destDir eq null) "" else destDir + File.separatorChar) +
          name)
    
    createParentDir(file)
    
    val writer = new FileWriter(file)
    try {
      writer.write(contents) // TODO: force UTF8
    } finally {
      writer.close()
    }
    file.getAbsolutePath
  }

  def writeFile(name: String, contents: StringTemplate): String =
    writeFile(name, contents.toString)
  
  /** Creates a parent directory for this file, if it does not
   * already exist. */
  def createParentDir(file: File) {
    val parent = file.getParentFile
    if (parent ne null) {
      parent.mkdirs()
    }
  }

  def getGroup(reader: java.io.Reader) =
    new StringTemplateGroup(reader)

  /**
   * Reads StringTemplate group from a .stg file.
   */
  def getTemplates(fileName: String) = {
     val in = Thread.currentThread.getContextClassLoader
     				.getResourceAsStream(fileName)
    if (in eq null)
      throw new IllegalArgumentException("Template not found: " + fileName)
    
    val reader = new java.io.InputStreamReader(in, "UTF-8")
    try {
      val t = getGroup(reader)
      t.registerRenderer(classOf[String], new StringRenderer())
      t
    } finally {
      reader.close
    }
  }
}
