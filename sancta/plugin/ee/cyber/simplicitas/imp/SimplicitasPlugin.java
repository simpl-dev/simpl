// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.imp.runtime.PluginBase;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.swt.graphics.Image;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import scala.Symbol;
import scala.Tuple3;

public abstract class SimplicitasPlugin extends PluginBase {
    private static final String SIMPLICITAS_PLUGIN = "simplicitas-plugin";
    private static final String SIMPLICITAS_FILE = "simplicitas_file";

    private static SimplicitasPlugin INSTANCE = null;

    public static Image getFileImage() {
        return INSTANCE.getImageRegistry().get(SIMPLICITAS_FILE);
    }

    public SimplicitasPlugin() {
        INSTANCE = this;
    }

    public void start(BundleContext context) throws Exception {
        super.start(context);
    }

    protected void initializeImageRegistry(
            org.eclipse.jface.resource.ImageRegistry reg) {
        org.osgi.framework.Bundle bundle = 
            Platform.getBundle(SIMPLICITAS_PLUGIN);

        IPath path = new Path("icons/simplicitas_file.gif");
        ImageDescriptor imageDescriptor = createImageDescriptor(bundle, path);
        reg.put(SIMPLICITAS_FILE, imageDescriptor);
    }

    public static org.eclipse.jface.resource.ImageDescriptor createImageDescriptor(
            org.osgi.framework.Bundle bundle,
            org.eclipse.core.runtime.IPath path) {
        java.net.URL url = FileLocator.find(bundle,
                path, null);
        if (url != null) {
            return ImageDescriptor.createFromURL(url);
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

    public Image addImage(String key, String path, Bundle bundle,
            ImageRegistry registry) {
        Path pathObj = new Path(path);
        ImageDescriptor descriptor = createImageDescriptor(bundle, pathObj);
        registry.put(key, descriptor);
        return registry.get(key);
    }
}
