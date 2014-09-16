package org.clafer.javascript;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Set;
import javax.script.ScriptException;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstUtil;
import org.clafer.ast.Asts;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOption;
import org.clafer.compiler.ClaferSearch;
import org.clafer.compiler.ClaferSearchStrategy;
import org.clafer.compiler.ClaferUnsat;
import org.clafer.instance.InstanceModel;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.RhinoException;
import org.mozilla.javascript.Scriptable;

/**
 * The Javascript CLI.
 *
 * @author jimmy
 */
public class JavascriptShell {

    // The Javascript engine.
    private Scriptable engine = Javascript.newEngine();
    // The model. Null if the model is not successfully loaded.
    private AstModel model;
    // The scope. Null if the model is not successfully loaded.
    private Scope scope;
    // The objectives.
    private Objective[] objectives;
    // The solver which solved the previous instance.
    // Null if first instance not yet solved.
    private ClaferSearch solver;
    // The last file successfully loaded.
    private File modelFile;
    // Configurable options.
    private final Options options = new Options();

    public void init() throws IOException {
        Context cxt = Context.enter();
        cxt.setOptimizationLevel(-1);
        try {
            engine.put("rsc", engine, this);
            // solver.js contains the mapping from Javascript to the methods
            // in this class.
            cxt.evaluateReader(engine,
                    new InputStreamReader(JavascriptShell.class.getResourceAsStream("solver.js")),
                    "solver.js", 1, null);
            engine.put("help", engine,
                    "help             display this helpful message\n"
                    + "load()           reload the model\n"
                    + "load(filename)   load a new model\n"
                    + "solve()          find the next solution\n"
                    + "maximize(Clafer) find a solution where Clafer.ref is maximal\n"
                    + "minimize(Clafer) find a solution where Clafer.ref is minimal\n"
                    + "minUnsat()       find the smallest set of unsatisfiable constraints and a near-miss\n"
                    + "unsatCore()      find a small set of mutually unsatisfiable constraints\n"
                    + "stats()          display statistics about the current search\n"
                    + "options          display and modify the compiler options\n"
                    + "exit()           stop the session");
            engine.put("options", engine, options);
        } finally {
            Context.exit();
        }
    }

    /**
     * Returns information about the final Choco CSP.
     *
     * @return information for debugging
     */
    public String internals() {
        return internals(false);
    }

    /**
     * Returns information about the final Choco CSP.
     *
     * @param initial debug initial state or current state
     * @return information for debugging
     */
    public String internals(boolean initial) {
        if (model == null) {
            return "No model. Use \"load(filename)\" to load in a new model.";
        }
        if (initial) {
            return ClaferCompiler.compile(getModel(), scope).getInternalSolver().toString();
        }
        if (solver == null) {
            return "Solve an instance first. Try \"solve()\".";
        }
        return solver.getInternalSolver().toString() + "\n" + solver.getInternalSolver().getMeasures();
    }

    /**
     * Execute Javascript.
     *
     * @param script executable Javascript
     * @return the value of the script, or null if it is a statement
     */
    String eval(String script) {
        Context cxt = Context.enter();
        cxt.setOptimizationLevel(-1);
        try {
            Object obj = cxt.evaluateString(engine, script, "commandline", 1, null);
            if (obj instanceof Messagable) {
                return ((Messagable) obj).toMessage();
            }
            return Context.toString(obj);
        } finally {
            Context.exit();
        }
    }

    /**
     * Reload the previous successful file.
     *
     * @return a message
     * @throws IOException something happened to the file
     */
    public String load() throws IOException {
        if (modelFile == null) {
            return "No model. Use \"load(filename)\" to load in a new model.";
        }
        return load(modelFile);
    }

    /**
     * Load a new file. The file must contain valid Javascript.
     *
     * @param filename the name of the file
     * @return a message
     * @throws IOException something happened to the file
     */
    public String load(String filename) throws IOException {
        return load(new File(filename));
    }

    /**
     * Load a new file. The file must contain valid Javascript.
     *
     * @param file the file
     * @return a message
     * @throws IOException something happened to the file
     */
    public String load(File file) throws IOException {
        model = null;
        scope = null;
        solver = null;
        engine = Javascript.newEngine();
        try {
            Triple<AstModel, Scope, Objective[]> pair = Javascript.readModel(file, engine);
            model = pair.getFst();
            scope = pair.getSnd();
            objectives = pair.getThd();
        } finally {
            init();
        }
        modelFile = file;
        return "Loaded " + AstUtil.getNames(AstUtil.getClafers(model)) + ".";
    }

    private AstModel getModel() {
        if (model == null) {
            throw new JavascriptException("No model. Use \"load(filename)\" to load in a new model.");
        }
        return model;
    }

    /**
     * Find the first instance or next instances upon subsequent invocations.
     *
     * @return an instance
     */
    public Object solve() {
        if (solver == null) {
            if (objectives.length == 0) {
                solver = ClaferCompiler.compile(getModel(), scope);
            } else if (objectives.length == 1) {
                solver = ClaferCompiler.compile(getModel(), scope, objectives[0]);
            } else {
                return "Muliobjective optimization not yet supported.";
            }
        }
        return solver.find() ? solver.instance() : null;
    }

    /**
     * Find an instance where the Clafer's value is maximal.
     *
     * @param clafer maximize this Clafer's value
     * @return the maximal value and the optimal instance
     */
    public Object maximize(AstClafer clafer) {
        if (!clafer.hasRef()) {
            throw new JavascriptException("Cannot maximize " + clafer + ".");
        }
        solver = ClaferCompiler.compile(getModel(), scope,
                Objective.maximize(Asts.sum(Asts.global(clafer))));
        return solver.find() ? solver.instance() : null;

    }

    /**
     * Find an instance where the Clafer's value is minimal.
     *
     * @param clafer minimize this Clafer's value
     * @return the minimal value and the optimal instance
     */
    public Object minimize(AstClafer clafer) {
        if (!clafer.hasRef()) {
            throw new JavascriptException("Cannot minimize " + clafer + ".");
        }
        solver = ClaferCompiler.compile(getModel(), scope,
                Objective.minimize(Asts.sum(Asts.global(clafer))));
        return solver.find() ? solver.instance() : null;
    }

    /**
     * Find the Min-Unsat and near-miss example. The Min-Unsat is empty if the
     * model is satisfiable, in which case the near-miss example is a real
     * instance of the model.
     *
     * @return the Min-Unsat and near-miss example
     */
    public Pair<Set<AstConstraint>, InstanceModel> minUnsat() {
        ClaferUnsat unsat = ClaferCompiler.compileUnsat(getModel(), scope);
        return unsat.minUnsat();
    }

    /**
     * Find the Min-Unsat-Core. Undefined if the model is satisfiable.
     *
     * @return the Min-Unsat-Core
     */
    public Set<AstConstraint> unsatCore() {
        ClaferUnsat unsat = ClaferCompiler.compileUnsat(getModel(), scope);
        return unsat.unsatCore();
    }

    public String stats() {
        if (solver == null) {
            throw new JavascriptException("No solver. Use \"solve()\" to solve the model.");
        }
        return solver.getInternalSolver().getMeasures().toString();
    }

    /**
     * Exit the program.
     *
     * @return a message
     */
    public String exit() {
        // Use interrupts to signal end.
        Thread.currentThread().interrupt();
        return "Exitting ...";
    }

    public static void main(String[] args) throws IOException, ScriptException {
        JavascriptShell context = new JavascriptShell();
        if (args.length > 0) {
            try {
                System.out.println(context.load(args[0]));
            } catch (IOException e) {
                System.out.println("Error: " + getOriginalMessage(e));
            } catch (RhinoException e) {
                System.out.println("Error: " + getOriginalMessage(e));
            }
        } else {
            context.init();
        }

        System.out.println("Type \"help\" for a list of commands.");
        System.out.print("> ");
        System.out.flush();
        BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
        String line;
        while ((line = r.readLine()) != null) {
            line = line.trim();
            // Do not evaluate empty lines.
            if (line.length() > 0) {
                try {
                    // Evaluate the Javascript command.
                    System.out.println(context.eval(line));
                    if (Thread.interrupted()) {
                        // Exit was invoked.
                        break;
                    }
                } catch (RhinoException e) {
                    System.out.println("Error: " + getOriginalMessage(e));
                }
            }
            System.out.print("> ");
            System.out.flush();
        }
        System.out.println();
    }

    /**
     * Find the message in the innermost cause. The Rhino engine tends to have
     * several layers of wrapped exceptions, which do not look great when
     * printing on the command line.
     *
     * @param e the outermost exception
     * @return the innermost message
     */
    private static String getOriginalMessage(Throwable e) {
        if (e.getCause() != null) {
            return getOriginalMessage(e.getCause());
        }
        return e.getMessage();
    }

    public static interface Messagable {

        /**
         * @return a message for the shell
         */
        public String toMessage();
    }

    public static class Options implements Messagable {

        private ClaferOption options = ClaferOption.Default;

        public String preferSmallerInstances() {
            options = options.setStrategy(ClaferSearchStrategy.PreferSmallerInstances);
            return "Updated options.";
        }

        public String preferLargerInstances() {
            options = options.setStrategy(ClaferSearchStrategy.PreferLargerInstances);
            return "Updated options.";
        }

        public String random() {
            options = options.setStrategy(ClaferSearchStrategy.Random);
            return "Updated options.";
        }

        public String basicSymmetryBreaking() {
            options = options.basicSymmetryBreaking();
            return "Updated options.";
        }

        public String fullSymmetryBreaking() {
            options = options.fullSymmetryBreaking();
            return "Updated options.";
        }

        public String basicOptimizations() {
            options = options.basicOptimizations();
            return "Updated options.";
        }

        public String fullOptimizations() {
            options = options.fullOptimizations();
            return "Updated options.";
        }

        // Convenience function for the toString method.
        private static String star(boolean bool) {
            return bool ? " * " : "   ";
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String toMessage() {
            return "(*) marks the current setting\n"
                    + star(options.getStrategy() == ClaferSearchStrategy.PreferSmallerInstances) + "options.preferSmallerInstances() bias the search towards smaller instances\n"
                    + star(options.getStrategy() == ClaferSearchStrategy.PreferLargerInstances) + "options.preferLargerInstances()  bias the search towards larger instances\n"
                    + star(options.getStrategy() == ClaferSearchStrategy.Random) + "options.random()                 search for instances randomly\n"
                    + star(options.isBasicSymmetryBreaking()) + "options.basicSymmetryBreaking()  basic symmetry breaking\n"
                    + star(options.isFullSymmetryBreaking()) + "options.fullSymmetryBreaking()   full symmetry breaking\n"
                    + star(options.isBasicOptimizations()) + "options.basicOptimizations()     basic optimizations\n"
                    + star(options.isFullOptimizations()) + "options.fullOptimizations()      full optimizations";
        }
    }
}
