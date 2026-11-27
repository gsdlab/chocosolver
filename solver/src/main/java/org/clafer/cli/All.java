package org.clafer.cli;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Function;
import joptsimple.OptionSet;
import org.clafer.alloy.solve.AlloyBackend;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOption;
import org.clafer.compiler.ClaferSearch;
import org.clafer.compiler.ClaferSearchStrategy;
import org.clafer.frontend.ToIr;
import org.clafer.ig.Backend;
import org.clafer.ig.ClaferModel;
import org.clafer.ig.JsonGenerator;
import org.clafer.ig.Repl;
import org.clafer.ig.Solution;
import org.clafer.ig.SolutionView;
import org.clafer.javascript.JavascriptFile;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;

/**
 * claferIG's {@code --all}: saves the solutions, at most {@code -n} of them,
 * to {@code <model>.1.data}, {@code <model>.2.data}, ... (under
 * {@code --savedir}, if given), as text, as JSON ({@code --json}) or as
 * Graphviz graphs ({@code <model>.1.dot}, {@code --dot}), without the REPL.
 * The scopes are those of the model's {@code .cfr-scope} file, if it exists,
 * otherwise the global scope given to {@code --all} (1 by default).
 */
final class All {

    private All() {
    }

    private static final String NoScopeFile = "Using the provided global scope as a `.cfr-scope` file does not"
            + " exist. Use the command `saveScopes` to create one.";

    /**
     * The instances of a static model, which Choco finds.
     */
    static void runStatic(File inputFile, JavascriptFile javascriptFile, OptionSet options) throws Exception {
        int global = (int) options.valueOf("all");
        Scope scope = Utils.resolveScopes(inputFile, javascriptFile, options);
        if (Utils.scopeFile(inputFile, options) == null && !options.has("scope")) {
            System.out.println(NoScopeFile);
            scope = scope.toBuilder().defaultScope(global).toScope();
        }
        ClaferOption option = javascriptFile.getOption();
        if (options.has("search")) {
            option = option.setStrategy((ClaferSearchStrategy) options.valueOf("search"));
        }
        Objective[] objectives = javascriptFile.getObjectives();
        ClaferSearch solver = objectives.length == 0
                ? ClaferCompiler.compile(javascriptFile.getModel(), scope, option)
                : ClaferCompiler.compile(javascriptFile.getModel(), scope, objectives, option);
        IgNames names = IgNames.of(inputFile, javascriptFile, options);
        Saver saver = new Saver(inputFile, options, names::info);
        while (saver.more() && solver.find()) {
            ClaferModel named = names.name(solver.instance());
            saver.save(new Solution.Instance(named, named.toString()));
        }
    }

    /**
     * The traces of a temporal model, which Alloy finds.
     */
    static void runTemporal(File inputFile, ToIr.Result ir, OptionSet options) throws Exception {
        AlloyBackend backend = Temporal.backend(inputFile, ir, options);
        try {
            Repl.Io quiet = new Repl.Io(null, new PrintStream(OutputStream.nullOutputStream()),
                    new PrintStream(OutputStream.nullOutputStream()));
            if (!options.has("scope")) {
                backend.allScopes(new Repl.Io(null, System.out, System.err), (int) options.valueOf("all"));
            }
            backend.prepare();
            Saver saver = new Saver(inputFile, options, backend::jsonInfo);
            while (saver.more()) {
                // no more solutions: the unsat core and the near-miss are not saved
                Backend.Shown shown = backend.next(quiet);
                if (shown == null) {
                    break;
                }
                saver.save(shown.solution());
            }
        } finally {
            backend.quit();
        }
    }

    /**
     * Writes the n-th solution to its file, as claferIG's {@code saveAll}.
     */
    private static final class Saver {

        private final String modelFileName;
        private final String saveDir;
        private final Integer max;
        private final boolean json;
        private final boolean dot;
        private final Function<String, JsonGenerator.Info> infos;
        private int counter = 1;

        Saver(File inputFile, OptionSet options, Function<String, JsonGenerator.Info> infos) {
            this.modelFileName = inputFile.getPath();
            this.saveDir = options.has("savedir") ? (String) options.valueOf("savedir") : null;
            this.max = options.has("n") ? (Integer) options.valueOf("n") : null;
            this.json = options.has("json");
            this.dot = options.has("dot");
            this.infos = infos;
        }

        boolean more() {
            return max == null || counter <= max;
        }

        void save(Solution solution) throws IOException {
            String extension = dot && !json ? ".dot" : ".data";
            Path file = Path.of(modelFileName + "." + counter + extension);
            if (saveDir != null) {
                file = Path.of(saveDir).resolve(file);
            }
            counter++;
            if (file.getParent() != null) {
                Files.createDirectories(file.getParent());
            }
            String text = json ? JsonGenerator.solutionJSON(solution, infos)
                    : dot ? SolutionView.solutionDot(solution)
                    : SolutionView.solutionText(solution);
            Files.writeString(file, text, StandardCharsets.UTF_8);
        }
    }
}
