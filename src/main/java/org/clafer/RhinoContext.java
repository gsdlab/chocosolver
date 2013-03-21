package org.clafer;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import org.clafer.analysis.AnalysisUtil;
import org.clafer.ast.Ast;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import sun.org.mozilla.javascript.JavaScriptException;

/**
 *
 * @author jimmy
 */
public class RhinoContext {

    private final Map<String, Integer> scope = new HashMap<String, Integer>();
    private final AstModel model = Ast.newModel();

    public void setScope(Map<String, Double> scope) {
        for (Entry<String, Double> entry : scope.entrySet()) {
            // Javascript integers are doubles.
            this.scope.put(entry.getKey(), entry.getValue().intValue());
        }
    }

    public int getDefaultScope() {
        Integer defaultScope = scope.get("default");
        if (defaultScope == null) {
            return 1;
        }
        return defaultScope.intValue();
    }

    public int getIntLow() {
        Integer intLow = scope.get("intLow");
        if (intLow == null) {
            return -16;
        }
        return intLow.intValue();
    }

    public int getIntHigh() {
        Integer intHigh = scope.get("intHigh");
        if (intHigh == null) {
            return 16;
        }
        return intHigh.intValue();
    }

    public Scope getScope() {
        Map<AstClafer, Integer> resolvedScope = new HashMap<AstClafer, Integer>();
        Map<String, AstClafer> resolvedClafers = AnalysisUtil.getClafersMap(model);
        for (Entry<String, Integer> entry : scope.entrySet()) {
            String key = entry.getKey();
            if (!key.equals("default") && !key.equals("intLow") && !key.equals("intHigh")) {
                AstClafer clafer = resolvedClafers.get(key);
                if (clafer == null) {
                    throw new IllegalStateException("Cannot set scope for unknown clafer \"" + key + "\"");
                }
                resolvedScope.put(clafer, entry.getValue());
            }
        }
        return new Scope(resolvedScope, getDefaultScope(), getIntLow(), getIntHigh());
    }

    public AstModel getModel() {
        return model;
    }

    public static void main(String[] args) throws Exception {
        try {
            ScriptEngine engine = new ScriptEngineManager().getEngineByMimeType("application/javascript");
            if (engine == null) {
                throw new IllegalStateException("Missing javascript engine.");
            }
            RhinoContext c = new RhinoContext();
            engine.put("cc", c);
            Object person = engine.eval("cc.k(3);");

            System.out.println(c.scope);
        } catch (ScriptException e) {
            JavaScriptException ex = (JavaScriptException) e.getCause();
        }
    }
}
