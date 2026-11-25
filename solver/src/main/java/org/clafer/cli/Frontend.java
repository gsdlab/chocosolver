package org.clafer.cli;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import joptsimple.OptionSet;
import org.clafer.ast.ToAst;
import org.clafer.compiler.ClaferOption;
import org.clafer.frontend.ModelFiles;
import org.clafer.frontend.ModelOptions;
import org.clafer.frontend.ToIr;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.scope.Scope;
import org.clafer.scope.ScopeFile;

/**
 * Reads a {@code .cfr} model that is not an explicit model: with the Java
 * frontend ({@link ToIr}, then {@link ToAst}) in this JVM, or, when the Java
 * frontend cannot take the model, with the Haskell compiler
 * ({@code clafer -k -m choco}), whose {@code .js} file is read with Rhino.
 * <p>
 * The Haskell compiler reads the model when:
 * <ul>
 * <li>{@code --haskell-frontend} is given;</li>
 * <li>the model imports a URL, which the Java frontend does not read;</li>
 * <li>the model has imports and {@code --import-root},
 * {@code --public-imports} or {@code --import-base} restricts them, which only
 * the compiler implements;</li>
 * <li>the model has a {@code [choco| |]} escape, which is JavaScript;</li>
 * <li>the Java frontend reports errors or fails: if the compiler compiles the
 * model, chocosolver solves its output and says that the Java frontend
 * rejected the model; otherwise the Java frontend's errors are the
 * model's.</li>
 * </ul>
 * The model's scopes are those of its explicit model's scope file
 * ({@link org.clafer.frontend.Scopes}), as the compiler writes them with
 * {@code -m clafer --meta-data}.
 */
public final class Frontend {

    private Frontend() {
    }

    /**
     * A model that cannot be read.
     */
    public static class ModelException extends Exception {

        private static final long serialVersionUID = 1L;

        public ModelException(String message) {
            super(message);
        }
    }

    /**
     * A model that imports a URL, which the Java frontend does not read.
     */
    private static final class UrlImportException extends ModelException {

        private static final long serialVersionUID = 1L;

        UrlImportException(String message) {
            super(message);
        }
    }

    /**
     * Reads a {@code .cfr} model with the Java frontend, without a fallback.
     *
     * @param file the model
     * @param imports the imports of the command line
     * @return the model, with the scopes of its explicit model's scope file
     * @throws ModelException with the errors of the model, each at its
     * position in its file
     */
    public static JavascriptFile readWithJava(File file, List<String> imports) throws ModelException {
        ModelFiles files = ModelFiles.read(file.getPath(), imports);
        if (files.importsUrl()) {
            throw new UrlImportException("imports a URL, which the Java frontend does not read");
        }
        ToIr.Result ir = ToIr.toIr(files);
        List<String> errors = ir.messages();
        if (errors.isEmpty()) {
            ToAst.Result built = ToAst.toAst(ir.model());
            // a construct that ToAst refuses can be reached twice, e.g., in a set expression
            errors = built.diagnostics().stream().map(ir.files()::format).distinct().toList();
            if (errors.isEmpty()) {
                Scope scope = ScopeFile.parse(ir.scopes().orElseThrow().scopeFile(), file.getPath() + "-scope")
                        .applyTo(Scope.builder().toScope(), built.model());
                return new JavascriptFile(built.model(), scope, ClaferOption.Default, built.objectives(),
                        built.assertions(), true);
            }
        }
        throw new ModelException(String.join("\n", errors));
    }

    /**
     * The options of the command line that restrict the imports, which the
     * compiler implements and the Java frontend does not.
     */
    private static final List<String> ImportRestrictions = List.of("import-root", "public-imports", "import-base");

    /**
     * Reads a {@code .cfr} model.
     *
     * @param file the model
     * @param options the command line
     * @param out where the fallback to the Haskell compiler is reported
     * @return the model, its scopes, objectives and assertions
     * @throws ModelException with the errors of the model
     */
    public static JavascriptFile read(File file, OptionSet options, PrintStream out)
            throws ModelException, IOException {
        List<String> imports = new ArrayList<>();
        for (Object i : options.valuesOf("import")) {
            imports.add(i.toString());
        }
        if (options.has("haskell-frontend")) {
            return haskell(file, options);
        }
        Optional<String> unsupported = unsupported(file, options, imports);
        if (unsupported.isPresent()) {
            out.println("The model " + unsupported.get() + ": compiling it with the Haskell compiler (clafer)");
            return haskell(file, options);
        }
        List<String> errors;
        try {
            return readWithJava(file, imports);
        } catch (UrlImportException e) {
            out.println("The model " + e.getMessage() + ": compiling it with the Haskell compiler (clafer)");
            return haskell(file, options);
        } catch (ModelException e) {
            errors = List.of(e.getMessage());
        } catch (RuntimeException e) {
            errors = List.of("The Java frontend failed on the model: " + e);
        }
        // the compiler is the reference: a model that it compiles is a model
        JavascriptFile compiled;
        try {
            compiled = haskell(file, options);
        } catch (ModelException | IOException e) {
            throw new ModelException(String.join("\n", errors) + "\nClafer compilation failed. Aborting...");
        }
        out.println("The Java frontend rejected the model, which the Haskell compiler (clafer) compiles;"
                + " please report this problem at https://github.com/gsdlab/chocosolver/issues:");
        for (String error : errors) {
            out.println(error);
        }
        out.println("Solving the Haskell compiler's output.");
        return compiled;
    }

    /**
     * Why the Java frontend cannot read the model, if it cannot.
     */
    private static Optional<String> unsupported(File file, OptionSet options, List<String> imports)
            throws IOException {
        if (imports.stream().anyMatch(ModelFiles::isUrl)) {
            return Optional.of("imports a URL (--import), which the Java frontend does not read");
        }
        String text = Files.readString(file.toPath());
        boolean hasImports = !imports.isEmpty() || !ModelOptions.of(text).imports().isEmpty();
        for (String restriction : ImportRestrictions) {
            if (hasImports && options.has(restriction)) {
                return Optional.of("has imports, which --" + restriction
                        + " restricts, as only the Haskell compiler does");
            }
        }
        if (text.contains("[choco|")) {
            return Optional.of("has a [choco| |] escape, whose JavaScript the Java frontend does not run");
        }
        return Optional.empty();
    }

    /**
     * Compiles the model with the Haskell compiler and reads its {@code .js}
     * file, next to the model.
     *
     * @throws ModelException with the compiler's output, if it does not
     * compile the model or cannot be run
     */
    private static JavascriptFile haskell(File file, OptionSet options) throws ModelException, IOException {
        String fileName = file.getPath();
        File jsFile = new File(fileName.substring(0, fileName.lastIndexOf('.')) + ".js");
        jsFile.delete();
        List<String> command = new ArrayList<>(List.of("clafer", "-k", "-m", "choco"));
        for (Object i : options.valuesOf("import")) {
            command.add("--import=" + i);
        }
        for (Object root : options.valuesOf("import-root")) {
            command.add("--import-root=" + root);
        }
        if (options.has("public-imports")) {
            command.add("--public-imports");
        }
        if (options.has("import-base")) {
            command.add("--import-base=" + options.valueOf("import-base"));
        }
        command.add(fileName);
        String output;
        int exitValue;
        try {
            Process compiler = new ProcessBuilder(command).redirectErrorStream(true).start();
            output = new String(compiler.getInputStream().readAllBytes());
            exitValue = compiler.waitFor();
        } catch (IOException e) {
            throw new ModelException("Could not run the Clafer compiler (clafer): " + e.getMessage());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new ModelException("The Clafer compiler (clafer) was interrupted");
        }
        // clafer exits with 0 also when the model has no Choco output (e.g., temporal operators)
        if (exitValue != 0 || !jsFile.exists()) {
            throw new ModelException(output.strip() + "\nClafer compilation failed. Aborting...");
        }
        try {
            return Javascript.readModel(jsFile);
        } catch (IOException e) {
            throw e;
        } catch (Exception e) {
            throw new ModelException("Unhandled compilation error occured. Please report this problem.\n"
                    + e.getMessage());
        }
    }
}
