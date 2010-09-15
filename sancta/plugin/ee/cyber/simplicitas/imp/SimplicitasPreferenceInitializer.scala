package ee.cyber.simplicitas.imp

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer

/**
  * This class is responsible for setting up default values for
  * syntax highlighting preferences. */
class SimplicitasPreferenceInitializer(pluginFactory: () => SimplicitasPlugin)
        extends AbstractPreferenceInitializer {

    def initializeDefaultPreferences() {
        println("initializeDefaultPreferences()")

        val plugin = pluginFactory()
        val store = plugin.getPreferenceStore

        for ((key, (desc, color, style)) <- plugin.colorDefs) {
            store.setDefault(plugin.colorKey(key), color)
            store.setDefault(plugin.styleKey(key), style.intValue)
        }
    }
}