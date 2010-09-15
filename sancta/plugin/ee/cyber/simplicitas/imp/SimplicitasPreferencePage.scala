// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp;

import org.eclipse.core.runtime.CoreException
import org.eclipse.ui.{IWorkbench, IWorkbenchPreferencePage}
import org.eclipse.ui.editors.text.TextFileDocumentProvider
import org.eclipse.swt.events.{SelectionEvent, SelectionAdapter}
import org.eclipse.swt.layout.{GridData, GridLayout}
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Composite, Control, Label, Button, List}

import org.eclipse.jface.util.{IPropertyChangeListener, PropertyChangeEvent}
import org.eclipse.jface.preference.{PreferencePage, ColorSelector}
import org.eclipse.jface.resource.StringConverter


object SimplicitasPreferencePage {
    /** Stores settings for a given token kind. */
    class TkSettings(
            val key: Symbol,
            val description: String,
            var color: String,
            var style: Int)
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
    
    /** Current settings. */
    val tokenSettings =
        (for ((key, (lbl, _, _)) <- colorDefs)
            yield new SimplicitasPreferencePage.TkSettings(key, lbl, null, 0)
        ).toArray

    
    def init(workbench: IWorkbench) {
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
        
        val editorComposite = makeEditorComposite
        
        tokenKinds = makeTokenKinds(editorComposite)
        
        val styleComposite = makeStyleComposite(editorComposite)

        new Label(styleComposite, SWT.LEFT).setText("Color:")

        colorSelector = new ColorSelector(styleComposite)

        bold = new Button(styleComposite, SWT.CHECK)
        bold.setText("Bold")

        italic = new Button(styleComposite, SWT.CHECK)
        italic.setText("Italic")

        val selectionAdapter = new SelectionAdapter() {
            override def widgetSelected(e: SelectionEvent) {
                colorChanged()
            }
        }
        bold.addSelectionListener(selectionAdapter)
        italic.addSelectionListener(selectionAdapter)
        colorSelector.addListener(new IPropertyChangeListener() {
            def propertyChange(event: PropertyChangeEvent) {
                if (event.getProperty == ColorSelector.PROP_COLORCHANGE) {
                    colorChanged()
                }
            }
        })
        
        // Select the first color.
        readTokenSettings()
        tokenKinds.select(0)
        colorSelected(0)

        controls
    }

    /** Save settings to preferences store and try to refresh open editor
      * windows. */
    override def performOk() = {
        saveTokenSettings()
        refreshEditors()

        super.performOk()
    }

    /** Re-read the settings from preference store and redisplay
      * the controls. */
    override def performDefaults() {
        readTokenDefaults()
        colorSelected(tokenKinds.getSelectionIndex)

        super.performDefaults
    }

    /** User selected new token kind from the list.
      * Update the dependent controls to display the new token settings. */
    def colorSelected(index: Int) {
        val tk = tokenSettings(index)
        colorSelector.setColorValue(StringConverter.asRGB(tk.color))
        bold.setSelection(boldValue(tk.style))
        italic.setSelection(italicValue(tk.style))
    }

    /** User changed settings for a particular token kind. Store the
      * new values in the array. */
    def colorChanged() {
        val tk = tokenSettings(tokenKinds.getSelectionIndex)
        tk.color = StringConverter.asString(colorSelector.getColorValue)
        tk.style = SWT.NORMAL
        if (bold.getSelection)
            tk.style |= SWT.BOLD
        if (italic.getSelection)
            tk.style |= SWT.ITALIC
    }

    /** Try to spread the knowledge that token color settings have changed. */
    def refreshEditors() {
        plugin.colorCache.clear

        // TODO: find some better way to refresh the editors.
        val editors = plugin.getWorkbench.getActiveWorkbenchWindow
                .getActivePage.getEditors
        val docProvider = new TextFileDocumentProvider()

        editors.foreach(editor =>
            try {
                docProvider.connect(editor.getEditorInput)
                val doc = docProvider.getDocument(editor.getEditorInput)
                val wasDirty = editor.isDirty
                if (doc != null) {
                    doc.set(doc.get)
                    if (!wasDirty) {
                        editor.doSave(null)                 
                    }
                }
            } catch {
                case e: CoreException => e.printStackTrace()
            }
        )
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

        ret.addSelectionListener(
                new SelectionAdapter() {
                    override def widgetSelected(e: SelectionEvent) {
                        colorSelected(ret.getSelectionIndex)
                    }
                })

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

    /** Read values for color settings from the Eclipse preference store. */
    def readTokenSettings() {
        val store = plugin.getPreferenceStore

        for (tk <- tokenSettings) {
            tk.color = store.getString(plugin.colorKey(tk.key))
            tk.style = store.getInt(plugin.styleKey(tk.key))
        }
    }

    /** Read default values for color settings. */
    def readTokenDefaults() {
        val store = plugin.getPreferenceStore

        for (tk <- tokenSettings) {
            tk.color = store.getDefaultString(plugin.colorKey(tk.key))
            tk.style = store.getDefaultInt(plugin.styleKey(tk.key))
        }
    }

    /** Save values to Eclipse preference store. */
    def saveTokenSettings() {
        val store = plugin.getPreferenceStore
        
        for (tk <- tokenSettings) {
            store.setValue(plugin.colorKey(tk.key), tk.color)
            store.setValue(plugin.styleKey(tk.key), tk.style.intValue)
        }
    }

    def boldValue(style: Int) = (style & SWT.BOLD) == SWT.BOLD

    def italicValue(style: Int) = (style & SWT.ITALIC) == SWT.ITALIC
}