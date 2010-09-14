// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp;

import org.eclipse.jface.preference.{PreferencePage, ColorSelector}

import org.eclipse.ui.{IWorkbench, IWorkbenchPreferencePage}
import org.eclipse.swt.layout.{GridData, GridLayout}
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Composite, Control, Label, Button, List}

object SimplicitasPreferencePage {
    /** Stores settings for a given token kind. The previousColor
      * and previousStyle members are used to remember previously set
      * values. These are used for "revert" and "cancel" actions. */
    class TkSettings(
            key: Symbol,
            description: String,
            var previousColor: String,
            val previousStyle: Int)
}

/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 */
class SimplicitasPreferencePage(pluginFactory: () => SimplicitasPlugin)
	extends PreferencePage with IWorkbenchPreferencePage {

    lazy val plugin = pluginFactory()
    
    // Parent control for all the widgets.
    var controls: Composite = null
    
    var tokenKinds: List = null
    var colorSelector: ColorSelector = null
    var italic: Button = null
    var bold: Button = null
    
    val colorDefs = plugin.colorDefs
    
    /** Previous values of colors. */
    val tokenSettings =
        for ((key, (lbl, _, _)) <- colorDefs)
            yield new SimplicitasPreferencePage.TkSettings(key, lbl, null, 0)
                
    
    def init(workbench: IWorkbench) {
        println("PreferencesPage.init(1)")
        setPreferenceStore(plugin.getPreferenceStore)
        initTokenSettings()
    }
    
    def createContents(parent: Composite): Control = {
        controls = new Composite(parent, SWT.NULL)
        controls.setLayout(new GridLayout());
        
        {
            val lbl = new Label(controls, SWT.LEFT)
            lbl.setText("Colors")
            val gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL)
            gd.horizontalSpan = 2
            lbl.setLayoutData(gd)
        }
        
        val editorComposite = makeEditorComposite
        
        tokenKinds = makeTokenKinds(editorComposite)
        
        val styleComposite = makeStyleComposite(editorComposite)

        new Label(styleComposite, SWT.LEFT).setText("Color:")

        colorSelector = new ColorSelector(styleComposite)

        bold = new Button(styleComposite, SWT.CHECK)
        bold.setText("Bold")

        italic = new Button(styleComposite, SWT.CHECK)
        italic.setText("Italic")
        
        controls
    }

    override def performOk() = super.performOk
    
    override def performCancel() = super.performCancel
    
    override def performDefaults() {
        super.performDefaults
    }
    
    def makeTokenKinds(parent: Composite) = {
        val ret = new List(parent,
                SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER)
        
        val gd = new GridData(
                GridData.VERTICAL_ALIGN_BEGINNING |
                GridData.FILL_HORIZONTAL)
        gd.heightHint = convertHeightInCharsToPixels(8)
        ret.setLayoutData(gd)

        for ((key, (lbl, _, _)) <- plugin.colorDefs) {
            ret.add(lbl)
        }

        ret
    }
    
    def makeEditorComposite = {
        val ret = new Composite(controls, SWT.NONE);

        val layout = new GridLayout()
        layout.numColumns = 2
        layout.marginHeight = 0
        layout.marginWidth = 0
        ret.setLayout(layout)
        
        val gd = new GridData(
                GridData.HORIZONTAL_ALIGN_FILL | GridData.FILL_VERTICAL)
        gd.horizontalSpan = 2
        ret.setLayoutData(gd)
        
        ret
    }
    
    def makeStyleComposite(parent: Composite) = {
        val ret = new Composite(parent, SWT.NONE);

        val layout = new GridLayout()
        layout.marginHeight = 0
        layout.marginWidth = 0
        layout.numColumns = 2
        ret.setLayout(layout)

        ret.setLayoutData(new GridData(GridData.FILL_BOTH))
        
        ret
    }
    
    def initTokenSettings() {
        for (tk <- tokenSettings) {
//            tk.previousColor = xx
//            tk.previousStyle = yy 
        }
    }
}