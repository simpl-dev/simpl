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
    
    var controls: Composite = null
    
    def init(workbench: IWorkbench) {
        println("PreferencesPage.init(1)")
        setPreferenceStore(plugin.getPreferenceStore)
    }
    
    def createContents(parent: Composite): Control = {
        controls = new Composite(parent, SWT.NULL)
        controls.setLayout(new GridLayout())
        
        new Label(controls, SWT.LEFT).setText("Colors")
        new List(controls,
                SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER)
        new Label(controls, SWT.LEFT).setText("Color:")
        new ColorSelector(controls)
        new Button(controls, SWT.CHECK).setText("Bold")
        new Button(controls, SWT.CHECK).setText("Italic")
        
        controls
    }
}
