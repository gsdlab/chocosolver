package org.clafer.javascript;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.ImporterTopLevel;
import org.mozilla.javascript.Scriptable;

/**
 * This class provides various methods of loading models, scopes, and objectives
 * via the Javascript API.
 *
 * @author jimmy
 */
public class Javascript {

    private Javascript() {
    }

    /**
     * @return a new Javascript engine.
     */
    public static Scriptable newEngine() {
        Context cxt = Context.enter();
        cxt.setOptimizationLevel(-1);
        try {
            return new ImporterTopLevel(cxt);
        } finally {
            Context.exit();
        }
    }

    public static JavascriptFile readModel(String in) throws IOException {
        return readModel("<unknown>", in, newEngine());
    }

    public static JavascriptFile readModel(File in) throws IOException {
        return readModel(in, newEngine());
    }

    public static JavascriptFile readModel(Reader in) throws IOException {
        return readModel("<unknown>", in, newEngine());
    }

    public static JavascriptFile readModel(File in, Scriptable engine) throws IOException {
        return readModel(in.getName(), in, engine);
    }

    public static JavascriptFile readModel(String name, String in, Scriptable engine) throws IOException {
        JavascriptContext context = new JavascriptContext();
        Context cxt = Context.enter();
        cxt.setOptimizationLevel(-1);
        try {
            engine.put("rc", engine, context);
            cxt.evaluateReader(engine,
                    new InputStreamReader(Javascript.class.getResourceAsStream("header.js")),
                    "header.js", 1, null);
            cxt.evaluateString(engine, in, name, 1, null);
            return new JavascriptFile(
                    context.getModel(),
                    context.getScope(engine),
                    context.getClaferOption(),
                    context.getObjectives(),
                    context.getAssertions());
        } finally {
            Context.exit();
        }
    }

    public static JavascriptFile readModel(String name, File in, Scriptable engine) throws IOException {
        return readModel(name, new FileReader(in), engine);
    }

    public static JavascriptFile readModel(String name, Reader in, Scriptable engine) throws IOException {
        JavascriptContext context = new JavascriptContext();
        Context cxt = Context.enter();
        cxt.setOptimizationLevel(-1);
        try {
            engine.put("rc", engine, context);
            cxt.evaluateReader(engine,
                    new InputStreamReader(Javascript.class.getResourceAsStream("header.js")),
                    "header.js", 1, null);
            cxt.evaluateReader(engine, in, name, 1, null);
            return new JavascriptFile(
                    context.getModel(),
                    context.getScope(engine),
                    context.getClaferOption(),
                    context.getObjectives(),
                    context.getAssertions());
        } finally {
            Context.exit();
        }
    }
}
