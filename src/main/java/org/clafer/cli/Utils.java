package org.clafer.cli;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import joptsimple.OptionSet;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstUtil;
import org.clafer.instance.InstanceClafer;
import org.clafer.javascript.JavascriptFile;
import org.clafer.scope.Scope;


public class Utils {
    public static List<AstClafer> getAllModelClafers(AstModel model) {

        List<AstClafer> result = new ArrayList<AstClafer>();

        List<AstAbstractClafer> abstractChildren = model.getAbstracts();
        result.addAll(abstractChildren);

        for (AstAbstractClafer clafer : abstractChildren)
        {
            result.addAll(getAllConcreteClafers(clafer));
        }

        List<AstConcreteClafer> concreteChildren = model.getChildren();
        result.addAll(concreteChildren);

        for (AstConcreteClafer clafer : concreteChildren)
        {
            result.addAll(getAllConcreteClafers(clafer));
        }

        return result;
    }

    public static List<AstConcreteClafer> getAllConcreteClafers(AstClafer root) {

        List<AstConcreteClafer> result = new ArrayList<AstConcreteClafer>();
        List<AstConcreteClafer> children = root.getChildren();

        result.addAll(children);

        for (AstConcreteClafer clafer : children)
        {
            if (clafer.hasChildren())
            {
                result.addAll(getAllConcreteClafers(clafer));
            }
        }

        return result;
    }

    public static void produceScopeFile(
            List<ClaferNameScopePair> claferScopePairs, String scopesFile) throws FileNotFoundException {
// could be done with JSON serializer, but this way I do not refer to any libraries

        PrintWriter writer = new PrintWriter(scopesFile);
        writer.println("[");

        for (int i = 0; i < claferScopePairs.size(); i++)
        {
            ClaferNameScopePair pair = claferScopePairs.get(i);

            if (pair.name.equals("#clafer#"))
                pair.name = "";

            writer.print("{\"lpqName\": \"" + pair.name + "\", \"scope\": " + pair.scope + "}");

            if (i != claferScopePairs.size() - 1)
            {
                writer.println(",");
            }

        }

        writer.println("]");
        writer.close();

    }

public static InstanceClafer getInstanceValueByName(InstanceClafer[] topClafers, String name) {

        for (int i = 0; i < topClafers.length; i++)
        {
            if (topClafers[i].getType().getName().equals(name))
            {
                return topClafers[i];
            }
        }

        for (int i = 0; i < topClafers.length; i++)
        {
            if (topClafers[i].hasChildren())
            {
                InstanceClafer result = getInstanceValueByName(topClafers[i].getChildren(), name);
                if (result != null)
                    return result;
            }
        }

        return null;
    }

    public static AstConcreteClafer getConcreteClaferChildByName(AstClafer root, String name) {

        List<AstConcreteClafer> children = root.getChildren();

        for (AstConcreteClafer clafer : children)
        {
            if (clafer.getName().equals(name))
            {
                return clafer;
            }
        }

        for (AstConcreteClafer clafer : children)
        {
            if (clafer.hasChildren())
            {
                AstConcreteClafer result = getConcreteClaferChildByName(clafer, name);
                if (result != null)
                    return result;
            }
        }

        return null;
    }

    public static AstClafer getModelChildByName(AstModel model, String name) {

        List<AstAbstractClafer> abstractChildren = model.getAbstracts();

        for (AstAbstractClafer clafer : abstractChildren)
        {
            if (clafer.getName().equals(name))
            {
                return clafer;
            }
        }

        for (AstAbstractClafer clafer : abstractChildren)
        {
            AstClafer foundchild = getConcreteClaferChildByName(clafer, name);
            if (foundchild != null)
            {
                return foundchild;
            }
        }


        List<AstConcreteClafer> concreteChildren = model.getChildren();

        for (AstConcreteClafer clafer : concreteChildren)
        {
            if (clafer.getName().equals(name))
            {
                return clafer;
            }
        }

        for (AstConcreteClafer clafer : concreteChildren)
        {
            AstClafer foundchild = getConcreteClaferChildByName(clafer, name);
            if (foundchild != null)
            {
                return foundchild;
            }
        }

        return null;
    }

    public static void printClafer(InstanceClafer clafer, Appendable out) throws IOException {
        printClafer(clafer, "", out);
    }

    private static void printClafer(InstanceClafer clafer, String indent, Appendable out) throws IOException {
        out.append(indent).append(clafer.getType().getName()).append('$').append(Integer.toString(clafer.getId()));

        if (clafer.getType().getSuperClafer() != null) {
            String name = clafer.getType().getSuperClafer().getName();
            // do not print the default super type "clafer"
            if (! name.equals("#clafer#"))
                out.append(" : ").append(name);
        }

        if(clafer.hasRef()) {
            out.append(" -> " + AstUtil.getInheritedRef(clafer.getType()).getTargetType().getName() + " = ");
            if (clafer.getRef() instanceof InstanceClafer) {
                InstanceClafer refClafer = (InstanceClafer) clafer.getRef();
                out.append(refClafer.getType().getName()).append('$').append(Integer.toString(refClafer.getId()));
            } else
                out.append(clafer.getRef().toString());
        }

        out.append(" \n");
        for (InstanceClafer child : clafer.getChildren()) {
            printClafer(child, indent + "  ", out);
        }
    }
    public static Scope resolveScopes(JavascriptFile javascriptFile, OptionSet options) {
        Scope scope = javascriptFile.getScope();
        if (options.has("scope"))
            scope = scope.toBuilder().defaultScope((int) options.valueOf("scope")).toScope();

        if (options.has("maxint")) {
            int scopeHigh = (int)options.valueOf("maxint");
            int scopeLow = options.has("minint") ? (int)options.valueOf("minint") : -(scopeHigh + 1);

            scope = scope.toBuilder().intLow(scopeLow).intHigh(scopeHigh).toScope();
        }
        else {
            /* setting the default int range */
            int scopeHighDef = 127;
            int scopeLowDef = -(scopeHighDef + 1);
            scope = scope.toBuilder().intLow(scopeLowDef).intHigh(scopeHighDef).toScope();
        }
        return scope;
    }
}

class ClaferNameScopePair implements Comparable<ClaferNameScopePair> {
    int scope;
    String name;

    public ClaferNameScopePair(String name, int scope) {
        this.name = name;
        this.scope = scope;
    }

    @Override
    public int compareTo(ClaferNameScopePair o) {
        return name.compareTo(o.name);
    }
}
