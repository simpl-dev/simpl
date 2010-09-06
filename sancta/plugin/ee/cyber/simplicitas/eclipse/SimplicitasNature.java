// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.eclipse;

import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;

public class SimplicitasNature implements IProjectNature {

	/**
	 * ID of this project nature
	 */
	public static final String NATURE_ID =
	    "simplicitas-plugin.simplicitasNature";
	private static final String CLASSPATH_ID =
	    "simplicitas-plugin.SIMPLICITAS_CONTAINER";

	private IProject project;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#configure()
	 */
	public void configure() throws CoreException {
	    System.out.println("nature.configure: " + project);
		IProjectDescription desc = project.getDescription();
		ICommand[] commands = desc.getBuildSpec();

		for (int i = 0; i < commands.length; ++i) {
			if (commands[i].getBuilderName().equals(
			        SimplicitasBuilder.BUILDER_ID)) {
				return;
			}
		}

		ICommand[] newCommands = new ICommand[commands.length + 1];
		System.arraycopy(commands, 0, newCommands, 0, commands.length);
		ICommand command = desc.newCommand();
		command.setBuilderName(SimplicitasBuilder.BUILDER_ID);
		newCommands[newCommands.length - 1] = command;
		desc.setBuildSpec(newCommands);
		project.setDescription(desc, null);

		// Add the classpath entry.
		IJavaProject jp = JavaCore.create(project);
		IClasspathEntry[] newPath = Arrays.copyOf(
		        jp.getRawClasspath(), jp.getRawClasspath().length + 1);
		newPath[newPath.length - 1] = 
		        JavaCore.newContainerEntry(
		                Path.fromPortableString(CLASSPATH_ID));
		jp.setRawClasspath(newPath, null);
		jp.save(null, true);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#deconfigure()
	 */
	public void deconfigure() throws CoreException {
        System.out.println("nature.deconfigure: " + project);

        IProjectDescription description = getProject().getDescription();
		ICommand[] commands = description.getBuildSpec();
		for (int i = 0; i < commands.length; ++i) {
			if (commands[i].getBuilderName().equals(SimplicitasBuilder.BUILDER_ID)) {
				ICommand[] newCommands = new ICommand[commands.length - 1];
				System.arraycopy(commands, 0, newCommands, 0, i);
				System.arraycopy(commands, i + 1, newCommands, i,
						commands.length - i - 1);
				description.setBuildSpec(newCommands);
				project.setDescription(description, null);			
				break;
			}
		}
		// remove the classpath entry.
        IJavaProject jp = JavaCore.create(project);
        ArrayList<IClasspathEntry> newPath = new ArrayList<IClasspathEntry>();
        IClasspathEntry[] oldPath = jp.getRawClasspath();
        IPath toRemove = Path.fromPortableString(CLASSPATH_ID);
        System.out.println("toremove: " + toRemove);
        for (int i = 0; i < oldPath.length; ++i) {
            System.out.println("Looking at: " + oldPath[i].getPath());
            if (!oldPath[i].getPath().equals(toRemove)) {
                newPath.add(oldPath[i]);
            } else {
                System.out.println("removing");
            }
        }
        jp.setRawClasspath(newPath.toArray(new IClasspathEntry[0]), null);
        jp.save(null, true);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#getProject()
	 */
	public IProject getProject() {
		return project;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
	 */
	public void setProject(IProject project) {
		this.project = project;
	}

}
