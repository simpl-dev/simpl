package ee.cyber.simplicitas.imp

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer

class SimplicitasPreferencesInitializer(pluginFactory: () => SimplicitasPlugin)
        extends AbstractPreferenceInitializer {

    def initializeDefaultPreferences() {
        println("initializeDefaultPreferences");
    }
}