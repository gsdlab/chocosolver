package org.clafer.javascript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.Reader;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import org.clafer.ast.AstModel;

/**
 *
 * @author jimmy
 */
public class Javascript {

    private Javascript() {
    }

    public static ScriptEngine newEngine() {
        // The Rhino engine is currently the only supported engine.
        ScriptEngine engine = new ScriptEngineManager().getEngineByName("rhino");
        if (engine == null) {
            throw new IllegalStateException("Missing Rhino Javascript engine.");
        }
        return engine;
    }

    public static AstModel readModel(String in) throws ScriptException {
        return readModel(in, newEngine());
    }

    public static AstModel readModel(File in) throws FileNotFoundException, ScriptException {
        return readModel(in, newEngine());
    }

    public static AstModel readModel(Reader in) throws ScriptException {
        return readModel(in, newEngine());
    }

    public static AstModel readModel(String in, ScriptEngine engine) throws ScriptException {
        return readModel("<unknown>", in, engine);
    }

    public static AstModel readModel(File in, ScriptEngine engine) throws FileNotFoundException, ScriptException {
        return readModel(in.getName(), new FileReader(in), engine);
    }

    public static AstModel readModel(Reader in, ScriptEngine engine) throws ScriptException {
        return readModel("<unknown>", in, engine);
    }

    public static AstModel readModel(String name, String in, ScriptEngine engine) throws ScriptException {
        RhinoContext context = new RhinoContext();
        engine.put("rc", context);
        engine.put(ScriptEngine.FILENAME, "header.js");
        engine.eval(new InputStreamReader(Javascript.class.getResourceAsStream("header.js")));
        engine.put(ScriptEngine.FILENAME, name);
        engine.eval(in);
        return context.getModel();
    }

    public static AstModel readModel(String name, Reader in, ScriptEngine engine) throws ScriptException {
        RhinoContext context = new RhinoContext();
        engine.put("rc", context);
        engine.put(ScriptEngine.FILENAME, "header.js");
        engine.eval(new InputStreamReader(Javascript.class.getResourceAsStream("header.js")));
        engine.put(ScriptEngine.FILENAME, name);
        engine.eval(in);
        return context.getModel();
    }
}
