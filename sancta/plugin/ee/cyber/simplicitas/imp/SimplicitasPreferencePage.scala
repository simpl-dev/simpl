// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp;

import org.eclipse.jface.preference.{PreferencePage, ColorSelector}

import org.eclipse.ui.{IWorkbench, IWorkbenchPreferencePage}
import org.eclipse.swt.layout.{GridData, GridLayout}
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Composite, Control, Label, Button, List}


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
    
    def init(workbench: IWorkbench) {
        println("PreferencesPage.init(1)")
        setPreferenceStore(plugin.getPreferenceStore)
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
        
        val editorComposite = new Composite(controls, SWT.NONE);
        {
            val layout = new GridLayout()
            layout.numColumns = 2
            layout.marginHeight = 0
            layout.marginWidth = 0
            editorComposite.setLayout(layout)
            
            val gd = new GridData(
                    GridData.HORIZONTAL_ALIGN_FILL | GridData.FILL_VERTICAL)
            gd.horizontalSpan = 2
            editorComposite.setLayoutData(gd)
        }
        
        tokenKinds = makeTokenKinds(editorComposite)
        
        val styleComposite = new Composite(editorComposite, SWT.NONE);
        {
            val layout = new GridLayout()
            layout.marginHeight = 0
            layout.marginWidth = 0
            layout.numColumns = 2
            styleComposite.setLayout(layout)

            styleComposite.setLayoutData(new GridData(GridData.FILL_BOTH))
        }

        new Label(styleComposite, SWT.LEFT).setText("Color:")

        colorSelector = new ColorSelector(styleComposite)

        bold = new Button(styleComposite, SWT.CHECK)
        bold.setText("Bold")

        italic = new Button(styleComposite, SWT.CHECK)
        italic.setText("Italic")
        
        controls
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
}
