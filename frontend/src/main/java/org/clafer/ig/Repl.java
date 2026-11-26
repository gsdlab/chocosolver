package org.clafer.ig;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.clafer.ig.ClaferModel.Clafer;

/**
 * The one loop of the solutions, whatever the backend: claferIG's
 * {@code CommandLine.hs}, with claferg's commands. A solution is an instance
 * (Choco) or a trace (Alloy). The loop keeps the solutions shown, the most
 * recent first, and their views: the full view or the diff view
 * ({@code d}), the saved files ({@code e}), the Graphviz graph
 * ({@code dot}) and the clafers found ({@code f}); the backend solves and
 * handles its own commands.
 */
public final class Repl {

    /**
     * The input of the commands and the outputs: claferIG prints its
     * solutions on the standard output and its unsat cores on the standard
     * error.
     */
    public record Io(BufferedReader in, PrintStream out, PrintStream err) {
    }

    /**
     * Shows a solution, which becomes the most recent one.
     */
    @FunctionalInterface
    public interface Displayer {

        void display(Backend.Shown shown);
    }

    /**
     * The solutions shown so far, the most recent first: those not saved yet
     * and those saved; the Alloy XML of the most recent; and whether they are
     * shown as the changes from the previous snapshot.
     */
    public static final class Context {

        final List<Solution> saved = new ArrayList<>();
        final List<Solution> unsaved = new ArrayList<>();
        String currentXml;
        boolean diffView;
        Displayer displayer;

        /**
         * Shows a solution that a backend's command found, e.g., claferg's
         * {@code x} and {@code z}, which becomes the most recent one.
         */
        public void display(Backend.Shown shown) {
            displayer.display(shown);
        }

        /**
         * @return how many solutions are shown and not saved
         */
        public int unsavedCount() {
            return unsaved.size();
        }

        /**
         * @return whether a solution is shown
         */
        public boolean hasSolution() {
            return !unsaved.isEmpty() || !saved.isEmpty();
        }

        /**
         * @return the Alloy XML of the most recent solution, or null
         */
        public String currentXml() {
            return currentXml;
        }
    }

    private final Backend backend;
    private final Io io;
    private final Context context = new Context();

    /**
     * @param diffView whether the solutions are shown as their changes
     * ({@code --diff})
     */
    public Repl(Backend backend, Io io, boolean diffView) {
        this.backend = backend;
        this.io = io;
        this.context.diffView = diffView;
        this.context.displayer = this::display;
    }

    /**
     * Runs the loop until {@code q} or the end of the input.
     */
    public void run() throws Exception {
        try {
            backend.start(io);
            next();
            while (true) {
                io.out().print(backend.prompt());
                io.out().flush();
                String line = io.in().readLine();
                if (line == null) {
                    break;
                }
                if (!command(line)) {
                    break;
                }
            }
        } finally {
            backend.quit();
        }
    }

    /**
     * @return whether the loop goes on
     */
    private boolean command(String input) throws Exception {
        String trimmed = input.trim();
        List<String> words = trimmed.isEmpty() ? List.of() : Arrays.asList(trimmed.split("\\s+"));
        String name = words.isEmpty() ? "" : words.get(0);
        switch (name) {
            case "", "n", "next" -> {
                if (words.size() > 1) {
                    return other(input, words);
                }
                next();
            }
            case "q", "quit" -> {
                return false;
            }
            case "h", "help" -> io.out().print(backend.help());
            case "e", "save" -> save();
            case "d", "diff" -> {
                io.out().println(context.diffView
                        ? "The instances and states are shown in full."
                        : "Each next instance or state is shown as its changes from the previous one.");
                context.diffView = !context.diffView;
            }
            case "dot" -> saveGraph();
            case "f", "find" -> {
                if (words.size() != 2) {
                    return other(input, words);
                }
                find(words.get(1));
            }
            case "fork" -> {
                Integer state = words.size() == 2 ? number(words.get(1)) : null;
                if (state == null) {
                    return other(input, words);
                }
                if (!backend.isDynamic()) {
                    io.out().println("The model is static: its instances are not traces.");
                } else {
                    otherSolution(() -> backend.fork(io, state));
                }
            }
            case "config" -> otherSolution(() -> backend.nextConfiguration(io));
            case "check" -> {
                Integer line = null;
                if (words.size() == 2) {
                    try {
                        int n = Integer.parseInt(words.get(1));
                        line = n > 0 ? n : null;
                    } catch (NumberFormatException e) {
                        return other(input, words);
                    }
                } else if (words.size() > 2) {
                    return other(input, words);
                }
                backend.check(io, line, this::display);
            }
            case "alloyInstance" -> io.out().println(context.currentXml == null ? "No instance" : context.currentXml);
            default -> {
                return other(input, words);
            }
        }
        return true;
    }

    private boolean other(String input, List<String> words) throws Exception {
        if (!backend.command(io, input, words, context)) {
            backend.unknown(io, input);
        }
        return true;
    }

    private static Integer number(String s) {
        return s.chars().allMatch(Character::isDigit) && !s.isEmpty() ? Integer.valueOf(s) : null;
    }

    private void next() throws Exception {
        Backend.Shown shown = backend.next(io);
        if (shown != null) {
            display(shown);
        }
    }

    @FunctionalInterface
    private interface Other {

        Backend.Shown get() throws Exception;
    }

    // shows a solution other than the next one: a fork, or another configuration
    private void otherSolution(Other command) throws Exception {
        if (!context.hasSolution()) {
            io.out().println("No solution yet: type 'n' for the first one.");
            return;
        }
        Backend.Shown shown = command.get();
        if (shown != null) {
            display(shown);
        }
    }

    /**
     * Shows a solution, which becomes the most recent one.
     */
    public void display(Backend.Shown shown) {
        int n = backend.number(context);
        Solution previous = latest();
        io.out().print(context.diffView
                ? SolutionView.showSolutionDiff(backend.names(), n, previous, shown.solution()) + "\n"
                : backend.show(n, shown));
        io.out().flush();
        context.unsaved.add(0, shown.solution());
        context.currentXml = shown.xml();
    }

    private Solution latest() {
        return !context.unsaved.isEmpty() ? context.unsaved.get(0)
                : !context.saved.isEmpty() ? context.saved.get(0) : null;
    }

    private void save() throws IOException {
        int counter = context.saved.size();
        for (Solution s : context.unsaved) {
            String saveName = backend.modelFileName() + "." + counter + ".data";
            Files.writeString(Path.of(saveName), SolutionView.solutionText(s), StandardCharsets.UTF_8);
            io.out().println("Saved to " + saveName);
            counter++;
        }
        context.saved.addAll(0, context.unsaved);
        context.unsaved.clear();
    }

    private void saveGraph() throws IOException {
        Solution solution = latest();
        if (solution == null) {
            io.out().println("No solution yet: type 'n' for the first one.");
            return;
        }
        int count = context.unsaved.size() + context.saved.size();
        String saveName = backend.modelFileName() + "." + count + ".dot";
        Files.writeString(Path.of(saveName), context.diffView
                ? SolutionView.solutionDiffDot(backend.names(), solution)
                : SolutionView.solutionDot(solution), StandardCharsets.UTF_8);
        io.out().println("Saved to " + saveName);
    }

    private void find(String name) {
        Solution solution = latest();
        if (solution == null) {
            io.out().println("No instance");
            return;
        }
        for (ClaferModel m : solution.models()) {
            for (Clafer c : m.traverse()) {
                if (c.name().equals(name)) {
                    io.out().println(ClaferModel.show(c));
                    return;
                }
            }
        }
        io.out().println("\"" + name + "\" not found in the model.");
    }
}
