package org.clafer.javascript;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Set;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;
import org.clafer.ast.AstClafer;
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
 *
 * @author jimmy
 */
public class JavascriptShell {

    private final ScriptEngine engine;
    private AstModel model;
    private Scope scope;
    private ClaferSolver solver;
    private File modelFile;

    public JavascriptShell() {
        this.engine = Javascript.newEngine();
    }

    public void init() throws ScriptException {
        engine.put("rsc", this);
        engine.put(ScriptEngine.FILENAME, "solver.js");
        engine.eval(new InputStreamReader(JavascriptShell.class.getResourceAsStream("solver.js")));
        engine.put(ScriptEngine.FILENAME, "command line");
        engine.put("help", "help : display this helpful message\n"
                + "load() : reload model"
                + "load(filename) : load a new model\n"
                + "solve() : find the next solution\n"
                + "maximize(Clafer) : find a solution where Clafer.ref is maximal\n"
                + "minimize(Clafer) : find a solution where Clafer.ref is minimal\n"
                + "minUnsat() : find the smallest set of unsatisfiable constraints and a near-miss\n"
                + "exit() : stop the session");
    }

    public Object eval(String script) throws ScriptException {
        return engine.eval(script);
    }

    public String load() throws FileNotFoundException, ScriptException {
        if (modelFile == null) {
            return "No model. Use \"load(filename)\" to load in a new model.";
        }
        return load(modelFile);
    }

    public String load(String filename) throws FileNotFoundException, ScriptException {
        return load(new File(filename));
    }

    public String load(File file) throws FileNotFoundException, ScriptException {
        model = null;
        scope = null;
        solver = null;
        try {
            engine.getBindings(ScriptContext.ENGINE_SCOPE).clear();
            Pair<AstModel, Scope> pair = Javascript.readModel(file, engine);
            model = pair.getFst();
            scope = pair.getSnd();
        } finally {
            init();
        }
        modelFile = file;
        return "Loaded " + AstUtil.getNames(AstUtil.getClafers(model)) + ".";
    }

    public InstanceModel solve() {
        if (solver == null) {
            if (model == null) {
                throw new JavascriptException("No model. Use \"load(filename)\" to load in a new model.");
            }
            solver = ClaferCompiler.compile(model, scope);
        }
        return solver.find() ? solver.instance() : null;
    }

    public InstanceModel maximize(AstClafer clafer) {
        if (!clafer.hasRef()) {
            throw new JavascriptException("Cannot maximize " + clafer + ".");
        }
        ClaferObjective objective = ClaferCompiler.compileMaximize(model, scope, clafer.getRef());
        return objective.optimal().getSnd();

    }

    public InstanceModel minimize(AstClafer clafer) {
        if (!clafer.hasRef()) {
            throw new JavascriptException("Cannot minimize " + clafer + ".");
        }
        ClaferObjective objective = ClaferCompiler.compileMinimize(model, scope, clafer.getRef());
        return objective.optimal().getSnd();
    }

    public Pair<Set<String>, InstanceModel> minUnsat() {
        ClaferUnsat unsat = ClaferCompiler.compileUnsat(model, scope);
        return unsat.minUnsat();
    }

    public String exit() {
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
            }
        } else {
            context.init();
        }

        System.out.println("Type \"help\" for list of commands.");
        System.out.print("> ");
        System.out.flush();
        BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
        String line;
        while ((line = r.readLine()) != null) {
            line = line.trim();
            if (line.length() > 0) {
                try {
                    System.out.println(context.eval(line));
                    if (Thread.interrupted()) {
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

    private static String getOriginalMessage(Throwable e) {
        if (e.getCause() != null) {
            return getOriginalMessage(e.getCause());
        }
        return e.getMessage();
    }
}
