// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp;

import org.eclipse.imp.language.ServiceFactory

import org.eclipse.core.runtime.CoreException
import org.eclipse.jface.preference.ColorSelector
import org.eclipse.jface.preference.BooleanFieldEditor
import org.eclipse.jface.preference.FieldEditorPreferencePage
import org.eclipse.jface.preference.IPreferenceStore
import org.eclipse.jface.preference.PreferenceConverter
import org.eclipse.jface.resource.StringConverter
import org.eclipse.swt.SWT
import org.eclipse.swt.events.SelectionAdapter
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.graphics.Font
import org.eclipse.swt.graphics.RGB
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.List
import org.eclipse.ui.editors.text.TextFileDocumentProvider
import org.eclipse.ui.IWorkbench
import org.eclipse.ui.IWorkbenchPreferencePage
import org.eclipse.ui.PlatformUI

import org.w3c.dom.css.RGBColor


/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By 
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to 
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 */
class SimplicitasPreferencePage(pluginFactory: () => SimplicitasPlugin)
	extends PreferencePage() 
 		with IWorkbenchPreferencePage {
     
    lazy val plugin = pluginFactory()     

	setPreferenceStore(plugin.getPreferenceStore())
	
	System.err.println("Initializing preferencepage!!!")

	var colorList: List = null
	var colorFieldEditor: ColorSelector = null
	var boldFieldEditor: Button = null
	var italicFieldEditor: Button = null
	val tokens = plugin.colorDefs
 
	/**
	* Array of Strings: 
	* "field description", "field id", "previous color value",
	* "previous style value" */
    var colorListModel = makeColorListModel 
    
    println("colorListModel:")
    for (clm <- colorListModel) {
    	println(clm.toList)
    }
    println("-------------")
   
    def makeColorListModel = {
	    val ret = new Array[Array[String]](tokens.size)

        var i = 0
        tokens foreach {
            case (key, value) =>
            ret(i) = Array(value._1, key.name, null, null)
            i += 1
	    }
        ret
    }
  
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	def createFieldEditors() {
		storeValues()
		createColorPreferences()
		getPreferenceStore().addPropertyChangeListener(this)
	}

	def createColorPreferences() {
		var label = new Label(getFieldEditorParent(), SWT.LEFT);
		label.setText("Colors"); 
		var gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
		gd.horizontalSpan = 2;
		label.setLayoutData(gd);

		// Draw tokens
		var editorComposite = new Composite(getFieldEditorParent(), SWT.NONE);
		var layout = new GridLayout();
		layout.numColumns = 2;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		editorComposite.setLayout(layout);
		gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.FILL_VERTICAL);
		gd.horizontalSpan= 2;
		editorComposite.setLayoutData(gd);		

		colorList = new List(editorComposite, SWT.SINGLE | SWT.V_SCROLL 
					| SWT.H_SCROLL | SWT.BORDER);
		gd = new GridData(GridData.VERTICAL_ALIGN_BEGINNING
					| GridData.FILL_HORIZONTAL);
		gd.heightHint = convertHeightInCharsToPixels(8);
		colorList.setLayoutData(gd);

		// Draw color filed editor
		var stylesComposite = new Composite(editorComposite, SWT.NONE);
		layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.numColumns = 2;
		stylesComposite.setLayout(layout);
		stylesComposite.setLayoutData(new GridData(GridData.FILL_BOTH));

		colorFieldEditor = new ColorSelector(stylesComposite) {
			override def setColorValue(rgb: RGB) {
				println("setColorValue("+ rgb +")")
				super.setColorValue(rgb)
			}
			
			override def updateColorImage() {
				println("updateColorImage")
				super.updateColorImage()
			}
		}
//	  	addField(colorFieldEditor)
    
		addField(new BooleanFieldEditor(
				"FOOBAR", "&An example of a boolean preference",
						stylesComposite));
		
        boldFieldEditor = new Button(stylesComposite, SWT.CHECK)
        boldFieldEditor.setText("Bold")

        italicFieldEditor = new Button(stylesComposite, SWT.CHECK)
        italicFieldEditor.setText("Italic")
        
		colorList.addSelectionListener(
			new SelectionAdapter() {
				override def widgetSelected(e: SelectionEvent) {
					handleColorListSelection
				}
			})
  
        val adapter = new SelectionAdapter() {
			override def widgetSelected(e: SelectionEvent) {
				handleFieldEditorChange
			}
		}
        boldFieldEditor.addSelectionListener(adapter)
        italicFieldEditor.addSelectionListener(adapter)
//		colorFieldEditor.getButton.addSelectionListener(adapter)
	}
 	
	/**
	 * Restore all color preferences to their values when the page was opened.
	 */
	override def performCancel() = {
		for (clm <- colorListModel) {
			if (clm(2) != null && clm(2).length > 0) {
				println("asRGB(" + clm(2) + ")")
				PreferenceConverter.setValue(getPreferenceStore(), clm(1), 
					StringConverter.asRGB(clm(2)))
			}
			if (clm(3) != null && clm(3).length > 0) {
				getPreferenceStore.setValue(clm(1), clm(3).toInt)
			}
		}
		super.performCancel
	}

	/**
	 * When the user applies the preferences, update the set of stored
	 * preferences so that we will fall back to the applied values on Cancel.
	 */
	override def performOk(): Boolean = {
        plugin.colorCache.clear
        storeValues
        val editors = plugin.getWorkbench.getActiveWorkbenchWindow
                .getActivePage.getEditors
        val docProvider = new TextFileDocumentProvider
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
		return super.performOk()
	}
 
	def handleFieldEditorChange() {	
		val key = colorListModel(colorList.getSelectionIndex())(1);
//		colorFieldEditor.setColorValue(
//				PreferenceConverter.getColor(getPreferenceStore(), key))
        var style = SWT.NORMAL
        if (boldFieldEditor.getSelection)
            style |= SWT.BOLD
        if (italicFieldEditor.getSelection)
            style |= SWT.ITALIC
   	    getPreferenceStore.setValue(plugin.getPreferencesKeyForStyle(key),
                                    style)
	}
 
	def handleColorListSelection() {
		println("handleColorListSelection()")
		val key = colorListModel(colorList.getSelectionIndex())(1);
		colorFieldEditor.setColorValue(
				PreferenceConverter.getColor(getPreferenceStore(), key))
		boldFieldEditor.setSelection(getStoredBoldFieldValue(key))
		italicFieldEditor.setSelection(getStoredItalicFieldValue(key))
	}
 
	def getStoredStyleValue(key: String): Int = {
		getPreferenceStore.getInt(plugin.getPreferencesKeyForStyle(key))
	}

	def getStoredBoldFieldValue(key: String): Boolean = {
		getStoredStyleValue(key) == SWT.BOLD || getStoredStyleValue(key) == (
		  SWT.BOLD | SWT.ITALIC)
	}

 	def getStoredItalicFieldValue(key: String): Boolean = {
		getStoredStyleValue(key) >= SWT.ITALIC
	}

 	/**
	 * @see org.eclipse.jface.preference.FieldEditorPreferencePage#initialize()
	 */
	override def initialize() {
 		storeValues
	 	super.initialize()
		for (clm <- colorListModel) {
			colorList.add(clm(0))
		}
  		val store = plugin.getPreferenceStore()
  		tokens foreach {
  			case (key, value) => 
  			  store.setDefault(key.name, value._2)
  			  store.setDefault(plugin.getPreferencesKeyForStyle(key.name),
                        value._3.intValue)
 		}
  		selectFirstColor()
	}
 
	/**
	 * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
	 */
	override def performDefaults() {
		storeValues
		for (clm <- colorListModel) {
			val key = clm(1);
			PreferenceConverter.setValue(getPreferenceStore(), 
					key, PreferenceConverter.getDefaultColor(
							getPreferenceStore(), key))
            val styleKey = plugin.getPreferencesKeyForStyle(key)
            getPreferenceStore.setValue(styleKey, 
                    getPreferenceStore.getDefaultInt(styleKey))
		}
		selectFirstColor()
		super.performDefaults()
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
	 */
	override def dispose() {
		getPreferenceStore().removePropertyChangeListener(this);
		super.dispose()
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	def init(workbench: IWorkbench) {
	}

	/**
	 * Stores the initial values of the color preferences. The preference values
	 * are updated on the fly as the user edits them (instead of only when they 
	 * press "Apply"). We need to store the old values so that we can reset them
	 * when the user chooses "Cancel".
	 */
	def storeValues() {
		for (clm <- colorListModel) {
			clm(2) = getPreferenceStore.getString(clm(1))
			clm(3) = getStoredStyleValue(clm(1)).toString
		}
	}
 
	def selectFirstColor() {
		colorList.getDisplay().asyncExec(new Runnable() {
			def run() {
				if (colorList != null && !colorList.isDisposed()) {
					colorList.select(0)
					handleColorListSelection()
				}
			}
		})
	}
}
