package org.clafer.javascript;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Set;
import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstUtil;
import org.clafer.scope.Scope;
import org.clafer.collection.Pair;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferObjective;
import org.clafer.compiler.ClaferSolver;
import org.clafer.compiler.ClaferUnsat;
import org.clafer.instance.InstanceModel;

/**
 * The Javascript CLI.
 *
 * @author jimmy
 */
public class JavascriptShell {

    // The Javascript engine.
    private final ScriptEngine engine = Javascript.newEngine();
    // The model. Null if the model is not successfully loaded.
    private AstModel model;
    // The scope. Null if the model is not successfully loaded.
    private Scope scope;
    // The solver which solved the previous instance.
    // Null if first instance not yet solved.
    private ClaferSolver solver;
    // The last file successfully loaded.
    private File modelFile;

    public void init() throws ScriptException {
        engine.put("rsc", this);
        // It's important to set the ScriptEngine.FILENAME since that's
        // what will be printed in error messages.
        engine.put(ScriptEngine.FILENAME, "solver.js");
        // solver.js contains the mapping from Javascript to the methods
        // in this class.
        engine.eval(new InputStreamReader(JavascriptShell.class.getResourceAsStream("solver.js")));
        engine.put(ScriptEngine.FILENAME, "command line");
        engine.put("help",
                "help             display this helpful message\n"
                + "load()           reload the model\n"
                + "load(filename)   load a new model\n"
                + "solve()          find the next solution\n"
                + "maximize(Clafer) find a solution where Clafer.ref is maximal\n"
                + "minimize(Clafer) find a solution where Clafer.ref is minimal\n"
                + "minUnsat()       find the smallest set of unsatisfiable constraints and a near-miss\n"
                + "exit()           stop the session");
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
            return ClaferCompiler.compile(model, scope).getInternalSolver().toString();
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
     * @throws ScriptException
     */
    public Object eval(String script) throws ScriptException {
        return engine.eval(script);
    }

    /**
     * Reload the previous successful file.
     *
     * @return a message
     * @throws FileNotFoundException something happened to the file
     * @throws ScriptException most likely a syntax error in the file
     */
    public String load() throws FileNotFoundException, ScriptException {
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
     * @throws FileNotFoundException something happened to the file
     * @throws ScriptException most likely a syntax error in the file
     */
    public String load(String filename) throws FileNotFoundException, ScriptException {
        return load(new File(filename));
    }

    /**
     * Load a new file. The file must contain valid Javascript.
     *
     * @param file the file
     * @return a message
     * @throws FileNotFoundException something happened to the file
     * @throws ScriptException most likely a syntax error in the file
     */
    public String load(File file) throws FileNotFoundException, ScriptException {
        model = null;
        scope = null;
        solver = null;
        Bindings bindings = engine.getBindings(ScriptContext.ENGINE_SCOPE);
        try {
            engine.setBindings(engine.createBindings(), ScriptContext.ENGINE_SCOPE);
            Pair<AstModel, Scope> pair = Javascript.readModel(file, engine);
            model = pair.getFst();
            scope = pair.getSnd();
        } catch (FileNotFoundException e) {
            engine.setBindings(bindings, ScriptContext.ENGINE_SCOPE);
            throw e;
        } catch (ScriptException e) {
            engine.setBindings(bindings, ScriptContext.ENGINE_SCOPE);
            throw e;
        } finally {
            init();
        }
        modelFile = file;
        return "Loaded " + AstUtil.getNames(AstUtil.getClafers(model)) + ".";
    }

    /**
     * Find the first instance or next instances upon subsequent invocations.
     *
     * @return an instance
     */
    public InstanceModel solve() {
        if (solver == null) {
            if (model == null) {
                throw new JavascriptException("No model. Use \"load(filename)\" to load in a new model.");
            }
            solver = ClaferCompiler.compile(model, scope);
        }
        return solver.find() ? solver.instance() : null;
    }

    /**
     * Find an instance where the Clafer's value is maximal.
     *
     * @param clafer maximize this Clafer's value
     * @return an optimal instance
     */
    public InstanceModel maximize(AstClafer clafer) {
        if (!clafer.hasRef()) {
            throw new JavascriptException("Cannot maximize " + clafer + ".");
        }
        ClaferObjective objective = ClaferCompiler.compileMaximize(model, scope, clafer.getRef());
        return objective.optimal().getSnd();

    }

    /**
     * Find an instance where the Clafer's value is minimal.
     *
     * @param clafer minimize this Clafer's value
     * @return an optimal instance
     */
    public InstanceModel minimize(AstClafer clafer) {
        if (!clafer.hasRef()) {
            throw new JavascriptException("Cannot minimize " + clafer + ".");
        }
        ClaferObjective objective = ClaferCompiler.compileMinimize(model, scope, clafer.getRef());
        return objective.optimal().getSnd();
    }

    /**
     * Find the min Unsat and near-miss example. The min Unsat is empty if the
     * model is satisfiable, in which case the near-miss example is a real
     * instance of the model.
     *
     * @return the min Unsat and near-miss example
     */
    public Pair<Set<AstConstraint>, InstanceModel> minUnsat() {
        ClaferUnsat unsat = ClaferCompiler.compileUnsat(model, scope);
        return unsat.minUnsat();
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
                System.out.println(getOriginalMessage(e));
            } catch (ScriptException e) {
                System.out.println(getOriginalMessage(e));
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
                } catch (ScriptException e) {
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
}
