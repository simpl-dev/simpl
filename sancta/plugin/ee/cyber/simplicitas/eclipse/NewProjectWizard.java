// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.eclipse;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;

import ee.cyber.simplicitas.Wizard;

public class NewProjectWizard extends org.eclipse.jface.wizard.Wizard
        implements INewWizard {
    private WizardNewProjectCreationPage _pageOne;
    
    private Text packageField;
    private Text classField;
    private Text extField;
    private Text idField;
    private Text descriptionField;
    private Combo baseField;
    
    private Map<String, Control> fields;

    public NewProjectWizard() {
    }

    public boolean performFinish() {
        String name = _pageOne.getProjectName();
        URI uri = null;
        if (!_pageOne.useDefaults()) {
            uri = _pageOne.getLocationURI();
        }
        
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        IProject newProject = root.getProject(name);
        if (newProject.exists()) {
            return true;
        }
        
        try {
            Wizard wizard = new Wizard();
            setWizardParameters(wizard);

            newProject.create(getProjectDescription(newProject, uri), null);
            
            if (!newProject.isOpen()) {
                newProject.open(null);
            }

            setClasspath(newProject);

            wizard.target = newProject.getLocation().toPortableString();

            wizard.unzip();
            
            // refresh the results of unzipping.
            newProject.refreshLocal(IResource.DEPTH_INFINITE, null);
        } catch (Wizard.BadParam e) {
            MessageBox box = new MessageBox(_pageOne.getShell(),
                    SWT.ICON_ERROR | SWT.OK);
            box.setMessage(e.getMessage());
            box.open();
            return false;
        } catch (Exception e) {
            // WTF?! The eclipse does not report errors from performFinish
            // method to user but instead silently fails.
            // Also, WTF?! The eclipse does not have normal dialog for
            // reporting an exception with stack trace.
            // TODO: create some dialog that also shows exception stack trace.
            ErrorDialog.openError(_pageOne.getShell(), 
                    "Error",
                    "Failed to create the project",
                    new Status(Status.ERROR, Activator.PLUGIN_ID,
                            e.getMessage(), null));
            return false;
        }

        return true;
    }

    private IProjectDescription getProjectDescription(
            IProject newProject, URI location) {
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        IProjectDescription desc = 
            newProject.getWorkspace().newProjectDescription(
                    newProject.getName());
        URI projectLocation = location;
    
        if (location != null && root.getLocationURI().equals(location)) {
            projectLocation = null;
        }

        desc.setLocationURI(projectLocation);
        desc.setNatureIds(new String[] {
                "org.scala-ide.sdt.core.scalanature",
                "org.eclipse.pde.PluginNature",
                "org.eclipse.jdt.core.javanature"});
        
        return desc;
    }
    
    private void setClasspath(IProject project) throws JavaModelException {
        IJavaProject jp = JavaCore.create(project);
        String name = project.getName();
        
        IClasspathEntry[] cp = new IClasspathEntry[] {
                JavaCore.newSourceEntry(makePath(name, "/src/tool"),
                        new IPath[]{},
                        makePath(name, "/target/classes")),
                JavaCore.newSourceEntry(makePath(name, "/src/plugin"),
                        new IPath[]{},
                        makePath(name, "/target/plugin")),
                JavaCore.newContainerEntry(
                        Path.fromPortableString(
                                "org.eclipse.pde.core.requiredPlugins")),
                JavaCore.newContainerEntry(
                        Path.fromPortableString(
                                "org.scala-ide.sdt.launching.SCALA_CONTAINER")),
                JavaCore.newContainerEntry(
                        Path.fromPortableString(
                                "org.eclipse.jdt.launching.JRE_CONTAINER")),
        };
        
        jp.setRawClasspath(cp, makePath(name, "/bin"), null);
        
        jp.save(null, true);
    }

    private IPath makePath(String name, String path) {
        return Path.fromPortableString("/" + name + path);
    }

    private void setWizardParameters(Wizard wizard) throws Wizard.BadParam {
        wizard.copyDotFiles = false;

        setParam(wizard, ee.cyber.simplicitas.Wizard.PACKAGE,
                packageField.getText());
        setParam(wizard, ee.cyber.simplicitas.Wizard.CLASS,
                classField.getText());
        setParam(wizard, ee.cyber.simplicitas.Wizard.EXT,
                extField.getText());
        setParam(wizard, ee.cyber.simplicitas.Wizard.ID,
                idField.getText());
        setParam(wizard, ee.cyber.simplicitas.Wizard.DESCRIPTION,
                descriptionField.getText());
        setParam(wizard, ee.cyber.simplicitas.Wizard.BASE,
                baseField.getText());
    }

    private void setParam(Wizard wizard, String param, String value)
            throws Wizard.BadParam {
        wizard.set(param, value);
    }

    @Override
    public void init(IWorkbench workbench, IStructuredSelection selection) {
        setWindowTitle("New Simplicitas project");
    }

    @Override
    public void addPages() {
        super.addPages();

        _pageOne = new WizardNewProjectCreationPage(
                "From Scratch Project Wizard");
        _pageOne.setTitle("From Scratch Project");
        _pageOne.setDescription("Create something from scratch.");
        
        addPage(_pageOne);
    }
    
    @Override
    public void createPageControls(Composite pageContainer) {
        super.createPageControls(pageContainer);

        Composite control = (Composite) _pageOne.getControl();
        
        System.out.println("control: " + control);
        
        Group group = new Group(control, SWT.NULL);
        group.setText("Language details");
        group.setLayoutData(new GridData(GridData.FILL_BOTH));
        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        group.setLayout(layout);
        
        fields = new HashMap<String, Control>();

        packageField = createInput(group, Wizard.PACKAGE);
        classField = createInput(group, Wizard.CLASS);
        extField = createInput(group, Wizard.EXT);
        idField = createInput(group, Wizard.ID);
        descriptionField = createInput(group, Wizard.DESCRIPTION);
        baseField = createTemplateField(group, Wizard.BASE);
    }

    private Text createInput(Composite parent, String parameter) {
        Wizard.ParamInfo paramInfo = Wizard.getParamInfo(parameter);

        Label lbl = new Label(parent, SWT.NULL);
        lbl.setText(paramInfo.description);

        Text txt = new Text(parent, SWT.BORDER | SWT.SINGLE);
        txt.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        txt.setText(paramInfo.defaultValue);
        
        fields.put(parameter, txt);
        
        return txt;
    }
    
    private Combo createTemplateField(Composite parent, String parameter) {
        Wizard.ParamInfo paramInfo = Wizard.getParamInfo(parameter);

        Label lbl = new Label(parent, SWT.NULL);
        lbl.setText(paramInfo.description);

        Combo cmb = new Combo(parent, SWT.READ_ONLY);
        cmb.setItems (new String [] {"bean", "empty"});
        cmb.setText(paramInfo.defaultValue);
        
        return cmb;
    }
}
