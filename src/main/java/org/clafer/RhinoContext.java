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
import org.clafer.ast.AstRef;
import sun.org.mozilla.javascript.JavaScriptException;

/**
 *
 * @author jimmy
 */
public class RhinoContext {

    private final Map<String, Integer> scope = new HashMap<String, Integer>();
    private int defaultScope = 1;
    private int intLow = -16;
    private int intHigh = 16;
    private final AstModel model = Ast.newModel();
    private boolean minimize;
    private AstRef objective;

    public void setScope(Map<String, Double> scope) {
        for (Entry<String, Double> entry : scope.entrySet()) {
            // Javascript integers are doubles.
            this.scope.put(entry.getKey(), entry.getValue().intValue());
        }
    }

    public void setDefaultScope(int defaultScope) {
        if (defaultScope < 1) {
            throw new IllegalArgumentException();
        }
        this.defaultScope = defaultScope;
    }

    public void setIntRange(int intLow, int intHigh) {
        this.intLow = intLow;
        this.intHigh = intHigh;
    }

    public boolean hasObjective() {
        return objective != null;
    }

    public AstRef getObjective() {
        return objective;
    }

    public boolean isMinimize() {
        return hasObjective() && minimize;
    }

    public boolean isMaximize() {
        return hasObjective() && !minimize;
    }

    public void setMinimize(AstClafer objective) {
        if (hasObjective()) {
            throw new IllegalStateException("Already has an objective");
        }
        if (!objective.hasRef()) {
            throw new IllegalStateException(objective + " cannot be an objective");
        }
        this.minimize = true;
        this.objective = objective.getRef();
    }

    public void setMaximize(AstClafer objective) {
        if (hasObjective()) {
            throw new IllegalStateException("Already has an objective");
        }
        if (!objective.hasRef()) {
            throw new IllegalStateException(objective + " cannot be an objective");
        }
        this.minimize = false;
        this.objective = objective.getRef();
    }

    public Scope getScope() {
        Map<AstClafer, Integer> resolvedScope = new HashMap<AstClafer, Integer>();
        Map<String, AstClafer> resolvedClafers = AnalysisUtil.getClafersMap(model);
        for (Entry<String, Integer> entry : scope.entrySet()) {
            String key = entry.getKey();
            AstClafer clafer = resolvedClafers.get(key);
            if (clafer == null) {
                throw new IllegalStateException("Cannot set scope for unknown clafer \"" + key + "\"");
            }
            resolvedScope.put(clafer, entry.getValue());
        }
        return new Scope(resolvedScope, defaultScope, intLow, intHigh);
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
