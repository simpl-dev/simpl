// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.eclipse;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IActionDelegate;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import ee.cyber.simplicitas.imp.APluginConfig;

/** Base class for generate action, invoked by double-clicking on a
 * DSL file and selecting "Generate". */
public class GenerateAction implements IObjectActionDelegate {

	private IFile file;
	
	// Absolute path in local file system
	private String path;

	private String fileName;
	private APluginConfig config;
	
	public GenerateAction(APluginConfig config) {
		this.config = config;
	}

	/**
	 * @see IObjectActionDelegate#setActivePart(IAction, IWorkbenchPart)
	 */
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
	}

	/**
	 * @see IActionDelegate#run(IAction)
	 * @Override
	 */
	public void run(IAction action) {
	    config.runGenerator(path, fileName);
	    try {
	        file.getProject().refreshLocal(IResource.DEPTH_INFINITE, null);
	    } catch (CoreException e) {
	        throw new RuntimeException(e);
	    }
	}

	/**
	 * @see IActionDelegate#selectionChanged(IAction, ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		if ((selection instanceof IStructuredSelection)) {
			IStructuredSelection structured = (IStructuredSelection) selection;
			Object object = structured.getFirstElement();
			// if the selection is a valid project file, get its name.
			if (object instanceof IFile) {
				file = (IFile) object;
				String filePath = file.getLocation().toOSString();
				fileName = file.getName();
				path = filePath.substring(0,
				        filePath.length() - fileName.length());
			}
		}
	}
}
