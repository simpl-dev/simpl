// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp

import org.eclipse.imp.parser.IParseController
import org.eclipse.imp.services.ITokenColorer
import org.eclipse.jface.resource.StringConverter
import org.eclipse.jface.text.{IRegion, TextAttribute}

import org.eclipse.swt.SWT._
import org.eclipse.swt.graphics.{Color, RGB}
import org.eclipse.swt.widgets.Display

import ee.cyber.simplicitas._

// TODO: this class could be refactored to improve efficiency -- all that
// string-to-rgb decoding and constant reading of the preferences
// doesn't seem too good to me. Although, it could be that the preferences
// service actually caches the information.
class TokenColorerBase(pluginFactory: () => SimplicitasPlugin,
                       config: APluginConfig)
    extends ITokenColorer {
  lazy val plugin = pluginFactory()

  /** Colors are cached so that each color is allocated only once
      to conserve system resources.*/
  override def getColoring(controller: IParseController,
                           o: Object): TextAttribute = {
    if (o eq null)
      return null

    val token = o.asInstanceOf[GenericToken]
    val customColor = config.getTokenColor(token)

    if (customColor ne null)
      getColor(customColor)
    else if (token.isKeyword)
      getColor('keyword)
    else if (token.isOperator)
      getColor('operator)
    else if (token.isComment)
      getColor('comment)
    else
      null
  }

  /** In general, you do not need to touch this. */
  override def calculateDamageExtent(seed: IRegion,
                                     ctlr: IParseController): IRegion =
    seed

  /** Returns text attribute with the given color and font style.
      Use constants from org.eclipse.swt.SWT class. */
  def getColor(id: Symbol): TextAttribute = {
    if (plugin.colorCache.contains(id))
      return plugin.colorCache(id)
    else {
      val attr = new TextAttribute(new Color(Display.getDefault,
          StringConverter.asRGB(findColor(id))), null, getStyle(id))
      plugin.colorCache += (id -> attr)
      return attr
    }
  }

  def getStyle(id: Symbol): Int = {
    val styleKey = plugin.styleKey(id)

    if (plugin.getPreferenceStore.contains(styleKey)) {
      return plugin.getPreferenceStore.getInt(styleKey)
    } else if ((plugin.colorDefs ne null) && plugin.colorDefs.contains(id)) {
       plugin.colorDefs(id)._3.intValue
    } else {
      NORMAL
    }
  }

  def findColor(id: Symbol): String = {
    val colorKey = plugin.colorKey(id)

    if (plugin.getPreferenceStore().contains(colorKey)) {
      return plugin.getPreferenceStore().getString(colorKey)
    } else if (plugin.colorDefs.contains(id)) {
       return plugin.colorDefs(id)._2
    } else {
      return "0,0,0" // default color
    }
  }
}
