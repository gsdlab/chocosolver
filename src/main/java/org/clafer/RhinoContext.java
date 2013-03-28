package org.clafer;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import org.clafer.analysis.AnalysisUtil;
import org.clafer.ast.Asts;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;

/**
 *
 * @author jimmy
 */
public class RhinoContext {

    private final Map<String, Integer> scope = new HashMap<String, Integer>();
    private int defaultScope = 1;
    private int intLow = -16;
    private int intHigh = 16;
    private final AstModel model = Asts.newModel();
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
        if (intLow > intHigh) {
            throw new IllegalArgumentException();
        }
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
        ScriptEngine engine = new ScriptEngineManager().getEngineByMimeType("application/javascript");
        if (engine == null) {
            throw new IllegalStateException("Missing javascript engine.");
        }
        System.out.print(">");
        BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
        String line;
        while ((line = r.readLine()) != null) {
            line = line.trim();
            if (line.length() > 0) {
                System.out.println(engine.eval(line));
            }
            System.out.print(">");
        }
        System.out.println("done");
    }
}
