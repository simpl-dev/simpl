// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp;

import org.eclipse.jface.preference.PreferencePage

import org.eclipse.ui.{IWorkbench, IWorkbenchPreferencePage}
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Composite, Control}


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
        println("PreferencesPage.init()")
    }
    
    def createContents(parent: Composite): Control = {
        controls = new Composite(parent, SWT.NULL)
        controls
    }
}
