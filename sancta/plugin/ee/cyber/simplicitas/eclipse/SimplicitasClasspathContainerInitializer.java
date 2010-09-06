// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.eclipse;

import java.io.IOException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.ClasspathContainerInitializer;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.osgi.framework.Bundle;

public class SimplicitasClasspathContainerInitializer extends
        ClasspathContainerInitializer {
    private static final Bundle TOOL_BUNDLE =
        Platform.getBundle("simplicitas-tool");
        
//    private static final IPath TOOL = 
//        pathInBundle(TOOL_BUNDLE, "/simplicitas-tool.jar");
//    private static final IPath PLUGIN = 
//        pathInBundle(SIMPLICITAS_BUNDLE, "/simplicitas-plugin.jar");
    
    public SimplicitasClasspathContainerInitializer() {
        System.out.println("constructor");
    }
    
    @Override
    public void initialize(final IPath containerPath, IJavaProject project)
            throws CoreException {
        System.out.println("ClassPathContainer.initialize("
                + containerPath + ", " + project + ")");
        System.out.println("bundle: " + TOOL_BUNDLE.getLocation());

        final IClasspathEntry[] entries = new IClasspathEntry[] {
                JavaCore.newLibraryEntry(getBundlePath(), null, null)
//                JavaCore.newLibraryEntry(TOOL_BUNDLE.get, null, null),
//                JavaCore.newLibraryEntry(PLUGIN, null, null) 
                };

        JavaCore.setClasspathContainer(
                containerPath,
                new IJavaProject[] {project},
                new IClasspathContainer[] {
                    new IClasspathContainer() {
                        @Override
                        public IPath getPath() {
                            System.out.println("getPath = " + containerPath);
                            return containerPath;
                        }
                        
                        @Override
                        public int getKind() {
                            return IClasspathContainer.K_DEFAULT_SYSTEM;
                        }
                        
                        @Override
                        public String getDescription() {
                            return "Simplicitas library ver xx";
                        }
                        
                        @Override
                        public IClasspathEntry[] getClasspathEntries() {
                            return entries;
                        }
                    }
                },
                null);
    }

    private IPath getBundlePath() {
        try {
            return Path.fromPortableString(
                    FileLocator.getBundleFile(TOOL_BUNDLE)
                    .getAbsolutePath());
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }
    
    private static IPath pathInBundle(Bundle bundle, String path) {
        System.out.println("pathInBundle(" + bundle + ", " + path + ")");
        URL url = FileLocator.find(bundle,
                Path.fromPortableString(path), null);
        if (url != null) {
            try {
                return Path.fromOSString(FileLocator.toFileURL(url).getPath());
            } catch (IOException e) {
                e.printStackTrace();
                return null;
            }
        }
        return null;
    }
    
    {
        System.out.println(Platform.getBundle(
                "simplicitas-plugin"));
    }
}
