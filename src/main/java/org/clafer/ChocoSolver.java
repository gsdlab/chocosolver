package org.clafer;

import choco.kernel.common.logging.ChocoLogging;
import javax.script.ScriptEngine;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import javax.script.ScriptEngineManager;
import java.io.Reader;
import java.lang.reflect.UndeclaredThrowableException;
import java.security.PrivilegedActionException;
import javax.script.ScriptException;
import sun.org.mozilla.javascript.RhinoException;

/**
 *
 * @author jimmy
 */
public class ChocoSolver {

    public static void main(String[] args) throws Throwable {
        try {
            try {
                try {
                    run();
                } catch (UndeclaredThrowableException e) {
                    try {
                        throw e.getCause();
                    } catch (PrivilegedActionException ex) {
                        throw ex.getCause();
                    }
                }
            } catch (ScriptException e) {
                throw e.getCause();
            }
        } catch (RhinoException e) {
            System.err.println(e.getMessage());
        }
    }

    public static void run() throws IOException, ScriptException, NoSuchMethodException {
        ScriptEngine engine = new ScriptEngineManager().getEngineByMimeType("application/javascript");
        if (engine == null) {
            throw new IllegalStateException("Missing javascript engine.");
        }

        engine.put(ScriptEngine.FILENAME, "header.js");
        engine.eval(readAll(ChocoSolver.class.getResourceAsStream("header.js")));
        engine.put(ScriptEngine.FILENAME, "Test.js");
        engine.eval(readAll(new File("/home/jimmy/Programming/clafer/Test.js")));

        Object __clafer__choco__exprs = engine.get("__clafer__choco__exprs");
        boolean __clafer__choco__solution = (Boolean) engine.get("__clafer__choco__solution");
        boolean __clafer__choco__statistics = (Boolean) engine.get("__clafer__choco__statistics");
        boolean __clafer__choco__branching = (Boolean) engine.get("__clafer__choco__branching");
        int __clafer__choco__solve = ((Double) engine.get("__clafer__choco__solve")).intValue();
        if (__clafer__choco__solve <= 0) {
            __clafer__choco__solve = Integer.MAX_VALUE;
        }

        if (__clafer__choco__exprs instanceof Exprs) {
            if (__clafer__choco__branching) {
                ChocoLogging.toSearch();
            } else if(__clafer__choco__statistics) {
                ChocoLogging.toVerbose();
            }

            Exprs expr = (Exprs) __clafer__choco__exprs;

            ExprsSolutions iter = expr.iterator();
            while (__clafer__choco__solve > 0 && iter.hasNext()) {
                String instance = iter.next();
                if (__clafer__choco__solution) {
                    System.out.println(instance);
                }
                __clafer__choco__solve--;
            }
            if (__clafer__choco__statistics) {
                System.out.println(iter.getRuntimeStatistics());
            }
        }
    }

    public static String readAll(File in) throws IOException {
        Reader reader = new FileReader(in);
        try {
            return readAll(reader);
        } finally {
            reader.close();
        }
    }

    public static String readAll(InputStream in) throws IOException {
        return readAll(new InputStreamReader(in));
    }

    public static String readAll(Reader in) throws IOException {
        StringBuilder result = new StringBuilder();
        char[] buffer = new char[1024];
        int l;
        while ((l = in.read(buffer)) != -1) {
            result.append(buffer, 0, l);
        }
        return result.toString();
    }
}
