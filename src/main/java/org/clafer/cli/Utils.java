package org.clafer.cli;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import joptsimple.OptionSet;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstPrinter;
import org.clafer.compiler.ClaferOption;
import org.clafer.frontend.ExplicitParser;
import org.clafer.ast.AstUtil;
import org.clafer.instance.InstanceClafer;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.scope.Scope;
import org.clafer.scope.ScopeFile;


public class Utils {
    /**
     * Whether a model file is an explicit model, to be read by
     * {@link ExplicitParser} rather than compiled by clafer: given with
     * --explicit, or a .cfr file whose first line is
     * {@link AstPrinter#ExplicitHeader} (or another {@code //# OPTIONS} line
     * with --explicit).
     */
    public static boolean isExplicitModel(File file, OptionSet options) throws IOException {
        if (options.has("explicit"))
            return true;
        if (!file.getName().endsWith(".cfr"))
            return false;
        try (BufferedReader reader = Files.newBufferedReader(file.toPath())) {
            String firstLine = reader.readLine();
            return firstLine != null && firstLine.startsWith("//# OPTIONS ")
                    && Arrays.asList(firstLine.substring(12).trim().split("\\s+")).contains("--explicit");
        }
    }

    /**
     * Reads a model: an explicit model with {@link ExplicitParser}, otherwise
     * a .js file. An explicit model starts from the default scope (its scopes
     * are in its scope file, see {@link #resolveScopes(File, JavascriptFile, OptionSet)}).
     *
     * @throws IllegalArgumentException with the diagnostics, if the explicit
     * model has errors
     */
    public static JavascriptFile readModel(File file, OptionSet options) throws Exception {
        if (!isExplicitModel(file, options))
            return Javascript.readModel(file);
        ExplicitParser.Result parsed = ExplicitParser.parse(Files.readString(file.toPath()));
        if (!parsed.diagnostics().isEmpty())
            throw new IllegalArgumentException(parsed.diagnostics().stream()
                    .map(d -> file.getPath() + ":" + d).collect(Collectors.joining("\n")));
        return new JavascriptFile(parsed.model(), Scope.builder().toScope(), ClaferOption.Default,
                parsed.objectives(), parsed.assertions());
    }

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
        return resolveScopes(javascriptFile.getScope(), javascriptFile.hasIntRange(), options);
    }

    /**
     * The scope with the command line's scopes and ranges, which override the given ones.
     *
     * @param hasIntRange whether the scope's integers were given, by the model or its scope file;
     * otherwise they are -128..127
     */
    private static Scope resolveScopes(Scope scope, boolean hasIntRange, OptionSet options) {
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
        else if (hasIntRange) {
            /* the integer range of the model, e.g., from clafer's --minint and --maxint, or of its scope file */
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
     * The scopes of the model: the model's, then its scope file's, if it has one, then the command line's.
     */
    public static Scope resolveScopes(File inputFile, JavascriptFile javascriptFile, OptionSet options) throws IOException {
        File scopeFile = scopeFile(inputFile, options);
        if (scopeFile == null)
            return resolveScopes(javascriptFile, options);
        if (!scopeFile.exists())
            throw new FileNotFoundException("The scope file does not exist: " + scopeFile.getPath());
        System.out.println("Using the scopes from " + scopeFile.getPath());
        ScopeFile file = ScopeFile.parse(Files.readString(scopeFile.toPath()), scopeFile.getPath());
        return resolveScopes(file.applyTo(javascriptFile.getScope(), javascriptFile.getModel()),
                javascriptFile.hasIntRange() || file.hasIntRange(), options);
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

    /**
     * Applies a .cfr-scope file (see {@link ScopeFile}) to a scope.
     *
     * @throws IllegalArgumentException if the file is malformed or a name is not of a clafer in the model
     */
    public static Scope applyScopeFile(Scope scope, AstModel model, String json, String fileName) {
        return ScopeFile.parse(json, fileName).applyTo(scope, model);
    }

    /**
     * The clafers with the given UID, otherwise with the given (partially) qualified name.
     */
    static List<AstClafer> claferNamed(List<AstClafer> clafers, String qName) {
        return ScopeFile.claferNamed(clafers, qName);
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
