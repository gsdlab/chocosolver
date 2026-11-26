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

    static {
        Quiet.forEach(l -> l.setLevel(Level.WARNING));
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

    private static AlloyBackend backend(File file, ToIr.Result first, OptionSet options) throws Exception {
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
        return new AlloyBackend(loader, file.getName(),
                new AlloyBackend.Options(AlloyBackend.Options.Default.bitwidth(), false, options.has("addtypes")));
    }

    /**
     * The REPL of the traces, as claferIG's.
     */
    static void runREPL(File file, ToIr.Result ir, OptionSet options) throws Exception {
        new Repl(backend(file, ir, options), new Repl.Io(
                new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8)), System.out, System.err),
                false).run();
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
                if (!options.has("noprint")) {
                    out.println(SolutionView.showSolution(index, shown.solution()));
                }
            }
            System.out.println("Generated " + index + " trace(s) within the scopes and the maximum trace length\n");
        } finally {
            backend.quit();
        }
    }
}
