package org.clafer.ig;

import java.util.List;
import java.util.function.UnaryOperator;

/**
 * A solver of a model behind the solution loop ({@link Repl}): Choco for a
 * static model, whose solutions are instances, and Alloy for a temporal
 * model, whose solutions are traces. The loop keeps the solutions shown and
 * their views; the backend solves, and handles its own commands with its own
 * messages.
 */
public interface Backend {

    /**
     * A solution as it is shown, with Alloy's XML of it, if any.
     */
    record Shown(Solution solution, String xml) {
    }

    /**
     * @return whether the model is temporal: its solutions are traces
     */
    boolean isDynamic();

    /**
     * @return the prompt of the loop
     */
    String prompt();

    /**
     * @return the help of the loop's commands
     */
    String help();

    /**
     * Prints what comes before the first solution, which the loop then asks
     * for.
     */
    void start(Repl.Io io) throws Exception;

    /**
     * The next solution, or null once the backend has said why there is none.
     */
    Shown next(Repl.Io io) throws Exception;

    /**
     * {@code fork}: another trace with the states of the last trace before
     * the given state, or null once the backend has said why there is none.
     */
    Shown fork(Repl.Io io, int state) throws Exception;

    /**
     * {@code config}: a solution with another configuration, or null once the
     * backend has said why there is none.
     */
    Shown nextConfiguration(Repl.Io io) throws Exception;

    /**
     * {@code check}: checks the assertions (on the line, if positive), and
     * shows each counterexample with the loop.
     */
    void check(Repl.Io io, Integer line, Repl.Displayer display) throws Exception;

    /**
     * Handles a command of the backend's own.
     *
     * @param words the command and its arguments
     * @return whether it is one
     */
    boolean command(Repl.Io io, String input, List<String> words, Repl.Context context) throws Exception;

    /**
     * Says that the input is no command.
     */
    void unknown(Repl.Io io, String input);

    /**
     * @return the number of the next solution shown
     */
    int number(Repl.Context context);

    /**
     * The full view of the n-th solution, as the backend prints it.
     */
    String show(int n, Shown shown);

    /**
     * @return the least-partially-qualified name of a clafer by its UID, for
     * the diff view
     */
    UnaryOperator<String> names();

    /**
     * @return the name of the model's file, which names the saved files
     */
    String modelFileName();

    /**
     * Releases the solver.
     */
    void quit();
}
