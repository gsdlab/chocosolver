package org.clafer.javascript;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.InputStreamReader;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import sun.org.mozilla.javascript.RhinoException;

/**
 *
 * @author jimmy
 */
public class RhinoShell {

    public static void main(String[] args) throws Throwable {
        ScriptEngine engine = new ScriptEngineManager().getEngineByMimeType("application/javascript");
        if (engine == null) {
            throw new IllegalStateException("Missing javascript engine.");
        }
        System.out.println(engine.getFactory());
        System.out.println(engine.getFactory().getEngineName());
        if(true) {return;};

        RhinoContext context = new RhinoContext();
        engine.put("rc", context);

        engine.put(ScriptEngine.FILENAME, "header.js");
        engine.eval(new InputStreamReader(RhinoShell.class.getResourceAsStream("header.js")));
        engine.put(ScriptEngine.FILENAME, args[0]);
        engine.eval(new FileReader(new File(args[0])));

        engine.put("rsc", new RhinoShellContext(context));
        engine.put(ScriptEngine.FILENAME, "solver.js");
        engine.eval(new InputStreamReader(RhinoShell.class.getResourceAsStream("solver.js")));

        System.out.print("> ");
        System.out.flush();
        BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
        String line;
        while ((line = r.readLine()) != null) {
            line = line.trim();
            if (line.length() > 0) {
                try {
                    System.out.println(engine.eval(line));
                } catch (ScriptException e) {
                    try {
                        throw e.getCause();
                    } catch (RhinoException re) {
                        System.out.println(re.details());
                    }
                }
            }
            System.out.print("> ");
            System.out.flush();
        }
        System.out.println();
    }
}
