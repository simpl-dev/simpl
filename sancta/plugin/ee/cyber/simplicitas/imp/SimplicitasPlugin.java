// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp;

import org.eclipse.core.runtime.Platform;
import org.eclipse.imp.runtime.PluginBase;
import org.eclipse.jface.text.TextAttribute;
import org.osgi.framework.BundleContext;

import scala.Symbol;
import scala.Tuple3;

public abstract class SimplicitasPlugin extends PluginBase {
    private static final String SIMPLICITAS_PLUGIN = "simplicitas-plugin";

    static final String SIMPLICITAS_DEFAULT_IMAGE = "simplicitas_default_image";
    static final String SIMPLICITAS_DEFAULT_OUTLINE_ITEM =
                    "simplicitas_default_outline_item";
    static final String SIMPLICITAS_FILE = "simplicitas_file";
    static final String SIMPLICITAS_FILE_WARNING = "simplicitas_file_warning";
    static final String SIMPLICITAS_FILE_ERROR = "simplicitas_file_error";

    public void start(BundleContext context) throws Exception {
        super.start(context);
    }

    // Definitions for image management

    public static final org.eclipse.core.runtime.IPath ICONS_PATH =
        new org.eclipse.core.runtime.Path("icons/");

    protected void initializeImageRegistry(
            org.eclipse.jface.resource.ImageRegistry reg) {
        org.osgi.framework.Bundle bundle = 
            Platform.getBundle(SIMPLICITAS_PLUGIN);

        org.eclipse.core.runtime.IPath path =
            ICONS_PATH.append("simplicitas_default_image.gif");
        org.eclipse.jface.resource.ImageDescriptor imageDescriptor =
            createImageDescriptor(bundle, path);
        reg.put(SIMPLICITAS_DEFAULT_IMAGE, imageDescriptor);

        path = ICONS_PATH.append("simplicitas_default_outline_item.gif");
        imageDescriptor = createImageDescriptor(bundle, path);
        reg.put(SIMPLICITAS_DEFAULT_OUTLINE_ITEM, imageDescriptor);

        path = ICONS_PATH.append("simplicitas_file.gif");
        imageDescriptor = createImageDescriptor(bundle, path);
        reg.put(SIMPLICITAS_FILE, imageDescriptor);

        path = ICONS_PATH.append("simplicitas_file_warning.gif");
        imageDescriptor = createImageDescriptor(bundle, path);
        reg.put(SIMPLICITAS_FILE_WARNING, imageDescriptor);

        path = ICONS_PATH.append("simplicitas_file_error.gif");
        imageDescriptor = createImageDescriptor(bundle, path);
        reg.put(SIMPLICITAS_FILE_ERROR, imageDescriptor);
    }

    public static org.eclipse.jface.resource.ImageDescriptor createImageDescriptor(
            org.osgi.framework.Bundle bundle,
            org.eclipse.core.runtime.IPath path) {
        java.net.URL url = org.eclipse.core.runtime.FileLocator.find(bundle,
                path, null);
        if (url != null) {
            return org.eclipse.jface.resource.ImageDescriptor.createFromURL(url);
        }
        return null;
    }

    // Definitions for image management end

    public scala.collection.mutable.Map<Symbol, TextAttribute> colorCache;

    public scala.collection.Map<Symbol, Tuple3<String, String, Number>> colorDefs;
    
    public String colorKey(Symbol key) {
        return key.name + ".color";
    }

    public String styleKey(Symbol key) {
        return key.name + ".style";
    }
}
