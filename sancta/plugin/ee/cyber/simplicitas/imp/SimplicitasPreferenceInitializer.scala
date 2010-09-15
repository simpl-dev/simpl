package ee.cyber.simplicitas.imp

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer

class SimplicitasPreferenceInitializer(pluginFactory: () => SimplicitasPlugin)
        extends AbstractPreferenceInitializer {

    def initializeDefaultPreferences() {
        println("initializeDefaultPreferences");
    }
}