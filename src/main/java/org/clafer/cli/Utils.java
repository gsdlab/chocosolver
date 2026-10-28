package org.clafer.cli;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import joptsimple.OptionSet;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstUtil;
import org.clafer.instance.InstanceClafer;
import org.clafer.javascript.JavascriptFile;
import org.clafer.scope.Scope;
import org.clafer.scope.ScopeBuilder;


public class Utils {
    /**
     * All the clafers of the model, including the nested abstract ones and the
     * type root #clafer#, except the roots of the model.
     */
    public static List<AstClafer> getAllModelClafers(AstModel model) {
        List<AstClafer> result = new ArrayList<AstClafer>(AstUtil.getClafers(model));
        result.remove(model.getAbstractRoot());
        result.remove(model.getRoot());
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

    public static AstClafer getModelChildByName(AstModel model, String name) {
        for (AstClafer clafer : getAllModelClafers(model))
            if (clafer.getName().equals(name))
                return clafer;
        return null;
    }

    public static void printClafer(InstanceClafer clafer, Appendable out) throws IOException {
        printClafer(clafer, "", out);
    }

    private static void printClafer(InstanceClafer clafer, String indent, Appendable out) throws IOException {
        out.append(indent).append(clafer.getType().getName()).append(countSuffix(clafer.getId()));

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
                out.append(refClafer.getType().getName()).append(countSuffix(refClafer.getId()));
            } else if (clafer.getRef() instanceof BigDecimal)
                // Never scientific notation, e.g. 0.0000001 rather than 1E-7.
                out.append(((BigDecimal) clafer.getRef()).toPlainString());
            else
                out.append(clafer.getRef().toString());
        }

        out.append(" \n");
        for (InstanceClafer child : clafer.getChildren()) {
            printClafer(child, indent + "  ", out);
        }
    }

    public static String countSuffix(int count) {
        return count == 0 ? "" : "$" + Integer.toString(count);
    }
    public static String simpleName(String fullName) {
        return fullName.substring(fullName.indexOf('_')+1);
    }

    public static Scope resolveScopes(JavascriptFile javascriptFile, OptionSet options) {
        Scope scope = javascriptFile.getScope();
        if (options.has("scope"))
            scope = scope.toBuilder().defaultScope((int) options.valueOf("scope")).toScope();

        /* overrides the model's decimalScale; a decimal literal with more digits is an error */
        if (options.has("decimal-digits"))
            scope = scope.toBuilder().decimalScale((int) options.valueOf("decimal-digits")).toScope();

        if (options.has("maxint")) {
            int scopeHigh = (int)options.valueOf("maxint");
            int scopeLow = options.has("minint") ? (int)options.valueOf("minint") : -(scopeHigh + 1);

            scope = scope.toBuilder().intLow(scopeLow).intHigh(scopeHigh).toScope();
        }
        else if (javascriptFile.hasIntRange()) {
            /* the integer range of the model, e.g., from clafer's --minint and --maxint */
            if (options.has("minint")) {
                scope = scope.toBuilder().intLow((int)options.valueOf("minint")).toScope();
            }
        }
        else {
            /* setting the default int range */
            int scopeHighDef = 127;
            int scopeLowDef = -(scopeHighDef + 1);
            scope = scope.toBuilder().intLow(scopeLowDef).intHigh(scopeHighDef).toScope();
        }
        return scope;
    }

    /**
     * The scopes of the model, with the scopes of its clafers from its scope file, if it has one.
     */
    public static Scope resolveScopes(File inputFile, JavascriptFile javascriptFile, OptionSet options) throws IOException {
        Scope scope = resolveScopes(javascriptFile, options);
        File scopeFile = scopeFile(inputFile, options);
        if (scopeFile == null)
            return scope;
        if (!scopeFile.exists())
            throw new FileNotFoundException("The scope file does not exist: " + scopeFile.getPath());
        System.out.println("Using the scopes from " + scopeFile.getPath());
        return applyScopeFile(scope, javascriptFile.getModel(), Files.readString(scopeFile.toPath()), scopeFile.getPath());
    }

    /**
     * The scope file given by --scope-file, otherwise the model's .cfr-scope file (model.cfr-scope next to
     * model.cfr or model.js) if it exists, otherwise null.
     */
    public static File scopeFile(File inputFile, OptionSet options) {
        if (options.has("scope-file"))
            return (File) options.valueOf("scope-file");
        String path = inputFile.getPath();
        int dot = path.lastIndexOf('.');
        if (dot > path.lastIndexOf(File.separatorChar))
            path = path.substring(0, dot);
        File file = new File(path + ".cfr-scope");
        return file.exists() ? file : null;
    }

    private static final Pattern ScopeEntry =
            Pattern.compile("\\{\\s*\"lpqName\"\\s*:\\s*\"([^\"]*)\"\\s*,\\s*\"scope\"\\s*:\\s*(\\d+)\\s*\\}");

    /**
     * Sets the scopes of the clafers listed in a .cfr-scope file: a JSON array of
     * {"lpqName": name, "scope": n}, as written by clafer --meta-data, claferIG's and the REPL's saveScopes.
     * A name is a clafer's UID (c0_Cfeatures) or a (partially) qualified name (Cfeatures, Component::Cfeatures,
     * ::Component::Cfeatures), which sets the scopes of all the clafers whose containment paths end with it.
     *
     * @throws IllegalArgumentException if the file is malformed or a name is not of a clafer in the model
     */
    public static Scope applyScopeFile(Scope scope, AstModel model, String json, String fileName) {
        List<AstClafer> clafers = getAllModelClafers(model);
        ScopeBuilder builder = scope.toBuilder();
        Matcher entry = ScopeEntry.matcher(json);
        StringBuilder rest = new StringBuilder();
        int end = 0;
        while (entry.find()) {
            rest.append(json, end, entry.start());
            end = entry.end();
            String name = entry.group(1);
            // the REPL saves the root clafer #clafer# with an empty name
            if (name.isEmpty())
                continue;
            List<AstClafer> named = claferNamed(clafers, name);
            if (named.isEmpty())
                throw new IllegalArgumentException(fileName + ": the model has no clafer " + name);
            for (AstClafer clafer : named)
                builder.setScope(clafer, Integer.parseInt(entry.group(2)));
        }
        rest.append(json.substring(end));
        if (!rest.toString().matches("\\s*\\[[\\s,]*\\]\\s*"))
            throw new IllegalArgumentException(fileName + ": not a list of {\"lpqName\": name, \"scope\": n}");
        return builder.toScope();
    }

    /**
     * The clafers with the given UID, otherwise with the given (partially) qualified name.
     */
    static List<AstClafer> claferNamed(List<AstClafer> clafers, String qName) {
        List<AstClafer> result = new ArrayList<>();
        for (AstClafer clafer : clafers)
            if (clafer.getName().equals(qName))
                result.add(clafer);
        if (!result.isEmpty())
            return result;
        boolean full = qName.startsWith("::");
        List<String> names = new ArrayList<>();
        // an apostrophe in a name is __prime__ in the UID
        for (String name : (full ? qName.substring(2) : qName).split("::"))
            names.add(name.replace("'", "__prime__"));
        for (AstClafer clafer : clafers) {
            List<String> path = containmentPath(clafer);
            if (full ? path.equals(names)
                     : path.size() >= names.size() && path.subList(path.size() - names.size(), path.size()).equals(names))
                result.add(clafer);
        }
        return result;
    }

    /**
     * The names of a clafer and its ancestors, from the top-level one, e.g., [Component, Cfeatures].
     */
    static List<String> containmentPath(AstClafer clafer) {
        List<String> path = new ArrayList<>();
        AstClafer current = clafer;
        while (current != null && !current.getName().startsWith("#")) {
            path.add(0, simpleName(current.getName()));
            // a top-level clafer's parent is the model's root #root#; an abstract clafer can be
            // nested, also under a concrete clafer
            AstClafer parent = current.getParent();
            current = parent == null || (parent instanceof AstConcreteClafer && !parent.hasParent()) ? null : parent;
        }
        return path;
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
