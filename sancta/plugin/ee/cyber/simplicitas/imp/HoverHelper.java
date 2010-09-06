// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp;


import java.util.List;

import org.eclipse.imp.editor.AnnotationHoverBase;
import org.eclipse.imp.language.ServiceFactory;
import org.eclipse.imp.parser.IParseController;
import org.eclipse.imp.parser.ISourcePositionLocator;
import org.eclipse.imp.services.IDocumentationProvider;
import org.eclipse.imp.services.IHoverHelper;
import org.eclipse.imp.services.IReferenceResolver;
import org.eclipse.imp.services.base.HoverHelperBase;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewer;

// This service should be part of the standard library -- by default,
// it is not configurable by the DSL creator. Therefore, there is no need
// to convert this class to Scala.
public class HoverHelper extends HoverHelperBase implements IHoverHelper {
    IReferenceResolver fResolver = null;

    public String getHoverHelpAt(IParseController parseController,
            ISourceViewer srcViewer, int offset) {
        // If there are any annotations associated with the line that contains
        // the given offset, return those
        try {
            List<Annotation> annotations =
                AnnotationHoverBase.getSourceAnnotationsForLine(
                        srcViewer,
                        srcViewer.getDocument().getLineOfOffset(offset));
            if (annotations != null && annotations.size() > 0) {
                // Some annotations have no text, such as breakpoint annotations;
                // if that's all we have, then don't bother returning it
                String msg = AnnotationHoverBase.formatAnnotationList(annotations);
                if (msg != null) {
                    return msg;
                }
            }
        } catch (BadLocationException e) {
            return "??? (BadLocationException for annotation)";
        }

        // Otherwise, return a message determined directly or indirectly based
        // on the node whose representation occurs at the given offset

        // Get the current AST; no AST implies no message
        Object ast = parseController.getCurrentAst();
        if (ast == null)
            return null;

        // Declare variables used in formulating the message
        Object sourceNode = null; // node at current hover point
        Object targetNode = null; // node referenced from current hover point
        Object helpNode = null; // node for which a help message is to be constructed
        String msg = null; // the help message for helpNode

        // Get the node at the given offset; no node implies no message
        ISourcePositionLocator nodeLocator =
                parseController.getSourcePositionLocator();
        sourceNode = nodeLocator.findNode(ast, offset);
        if (sourceNode == null)
            return null;

        // Check whether there is a reference resolver for the identified
        // source node; if so, attempt to get the node that is referenced by
        // the source node, on the assumption that the referenced node should
        // be the basis for the help message (e.g., as a decl for an identifier)
        if (fResolver == null && fLanguage != null) {
            try {
                fResolver = ServiceFactory.getInstance().getReferenceResolver(
                        fLanguage);
            } catch (Exception e) {
//                StatesPlugin.getInstance().writeErrorMsg(
//                        "Exception getting Reference Resolver service from service factory");
                fResolver = null;
            }
        }
        if (fResolver != null) {
            targetNode = fResolver.getLinkTarget(sourceNode, parseController);
        }

        // If the target node is not null, provide help based on that;
        // otherwise, provide help based on the source node
        if (targetNode != null)
            helpNode = targetNode;
        else
            helpNode = sourceNode;

        // Now need to determine whether the help message should be determined
        // based on the text represented by the node or based on some separate
        // text provided through an IDocumentationProvider

        // Check whether there is a documentation provider for the language;
        // if so, check whether it provides documentation for the help node;
        // if so, return that documentation
        IDocumentationProvider docProvider = null;
        if (fLanguage != null) {
            try {
                docProvider = ServiceFactory.getInstance()
                        .getDocumentationProvider(fLanguage);

            } catch (Exception e) {
//                StatesPlugin.getInstance().writeErrorMsg(
//                        "Exception getting Documentation Provider Service from service factory");
                fResolver = null;
            }
        }
        if (docProvider != null) {
            msg = (docProvider != null) ? docProvider.getDocumentation(
                    helpNode, parseController) : null;
            if (msg != null)
                return msg;
        }

        return null;
    }
}
