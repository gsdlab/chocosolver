package org.clafer.cli;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import joptsimple.OptionSet;
import org.clafer.alloy.solve.AlloyBackend;
import org.clafer.frontend.ModelFiles;
import org.clafer.frontend.ToIr;
import org.clafer.ig.Backend;
import org.clafer.ig.JsonGenerator;
import org.clafer.ig.Repl;
import org.clafer.ig.SolutionView;

/**
 * A temporal model, whose solutions are traces: clafersg solves it with Alloy
 * 6 in the JVM, as claferIG does, in the REPL ({@code --repl}) or by
 * printing the traces (up to {@code -n}).
 */
final class Temporal {

    private Temporal() {
    }

    // Alloy and Kodkod log their native libraries' search at INFO, through slf4j-jdk14: only the
    // warnings are shown; the loggers are kept, since java.util.logging holds them weakly
    private static final List<Logger> Quiet = List.of(Logger.getLogger("kodkod"), Logger.getLogger("edu.mit"),
            Logger.getLogger("org.alloytools"));

    // Kodkod warns when it frees a native solver that was not freed before ("finalize called"), which
    // is not a problem of the model: those warnings are dropped
    private static final Logger NativeSolver = Logger.getLogger("kodkod.engine.satlab.NativeSolver");

    static {
        Quiet.forEach(l -> l.setLevel(Level.WARNING));
        NativeSolver.setFilter(r -> r.getMessage() == null || !r.getMessage().contains("finalize called"));
    }

    /**
     * The model, if it is a temporal {@code .cfr} model that the Java
     * frontend reads: a model with errors, or that only the Haskell compiler
     * reads, goes the static way, which reports them.
     */
    static Optional<ToIr.Result> read(File file, OptionSet options) {
        if (!file.getName().endsWith(".cfr") || options.has("haskell-frontend")) {
            return Optional.empty();
        }
        try {
            if (Utils.isExplicitModel(file, options) || Files.readString(file.toPath()).contains("[choco|")) {
                return Optional.empty();
            }
            ToIr.Result ir = load(file, options);
            return ir.messages().isEmpty() && ir.model().options().traceLength().isPresent()
                    ? Optional.of(ir) : Optional.empty();
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    private static ToIr.Result load(File file, OptionSet options) {
        List<String> imports = new ArrayList<>();
        for (Object i : options.valuesOf("import")) {
            imports.add(i.toString());
        }
        return ToIr.toIr(ModelFiles.read(file.getPath(), imports, p -> Optional.empty(),
                Frontend.restrictions(options)));
    }

    static AlloyBackend backend(File file, ToIr.Result first, OptionSet options) throws Exception {
        boolean[] loaded = {false};
        // the first load is the model read already; 'r' reads it again
        AlloyBackend.Loader loader = () -> {
            if (!loaded[0]) {
                loaded[0] = true;
                return first;
            }
            ToIr.Result ir = load(file, options);
            if (!ir.messages().isEmpty()) {
                throw new IllegalArgumentException(String.join("\n", ir.messages()));
            }
            return ir;
        };
        // the integer range of the command line (--maxint, --minint) raises the bitwidth, as the model's does
        int bitwidth = AlloyBackend.Options.Default.bitwidth();
        if (options.has("maxint")) {
            bitwidth = Math.max(bitwidth, AlloyBackend.requiredBitwidth((int) options.valueOf("maxint")));
        }
        if (options.has("minint")) {
            bitwidth = Math.max(bitwidth, AlloyBackend.requiredBitwidth(-(long) (int) options.valueOf("minint") - 1));
        }
        AlloyBackend backend = new AlloyBackend(loader, file.getName(),
                // --json names the instances by their UIDs, as claferIG's
                new AlloyBackend.Options(bitwidth, options.has("useuids") || options.has("json"),
                        options.has("addtypes")));
        // --scope: the default scope of the clafers, as the REPL's 'S' sets it
        if (options.has("scope")) {
            backend.setInitialGlobalScope((int) options.valueOf("scope"));
        }
        return backend;
    }

    /**
     * The REPL of the traces, as claferIG's.
     */
    static void runREPL(File file, ToIr.Result ir, OptionSet options) throws Exception {
        new Repl(backend(file, ir, options), new Repl.Io(
                new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8)), System.out, System.err),
                options.has("diff"), REPL.view(options)).run();
    }

    /**
     * Prints the traces, up to {@code -n}, as claferIG shows them.
     */
    static void runNormal(File file, ToIr.Result ir, OptionSet options, PrintStream out) throws Exception {
        int n = options.has("n") ? (int) options.valueOf("n") : -1;
        AlloyBackend backend = backend(file, ir, options);
        try {
            System.out.println("Instantiating...");
            backend.prepare();
            Repl.Io io = new Repl.Io(null, out, System.err);
            int index = 0;
            while (n < 0 || index < n) {
                Backend.Shown shown = backend.next(io);
                if (shown == null) {
                    break;
                }
                index++;
                if (options.has("json")) {
                    // one line of JSON for each trace, as claferIG's REPL shows it
                    out.println(JsonGenerator.solutionJSON(shown.solution(), backend::jsonInfo));
                } else if (!options.has("noprint")) {
                    out.println(SolutionView.showSolution(index, shown.solution()));
                }
            }
            System.out.println("Generated " + index + " trace(s) within the scopes and the maximum trace length\n");
        } finally {
            backend.quit();
        }
    }
}
