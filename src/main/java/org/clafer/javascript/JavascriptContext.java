package org.clafer.javascript;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.Asts;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import org.mozilla.javascript.NativeJavaObject;
import org.mozilla.javascript.Scriptable;

/**
 *
 * @author jimmy
 */
public class JavascriptContext {

    private final Map<String, Integer> scope = new HashMap<>();
    private final List<Objective> objectives = new ArrayList<>(0);
    private int defaultScope = 1;
    private int intLow = -16;
    private int intHigh = 16;
    private final AstModel model = Asts.newModel();

    public void setScope(Map<String, Number> scope) {
        for (Entry<String, Number> entry : scope.entrySet()) {
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

    public Scope getScope(Scriptable engine) {
        Map<AstClafer, Integer> resolvedScope = new HashMap<>();
        for (Entry<String, Integer> entry : scope.entrySet()) {
            String key = entry.getKey();
            Object value = engine.get(key, engine);
            if (!(value instanceof NativeJavaObject)) {
                throw new IllegalStateException(key + " is not a Clafer, found " + value);
            }
            NativeJavaObject object = (NativeJavaObject) value;
            if (!(object.unwrap() instanceof AstClafer)) {
                throw new IllegalStateException(key + " is not a Clafer, found " + object.unwrap());
            }
            AstClafer clafer = (AstClafer) object.unwrap();
            if (clafer == null) {
                throw new IllegalStateException("Cannot set scope for unknown Clafer \"" + key + "\", "
                        + ".");
            }
            resolvedScope.put(clafer, entry.getValue());
        }
        return new Scope(resolvedScope, defaultScope, intLow, intHigh);
    }

    public void addMaximizeObjective(AstSetExpr expr) {
        objectives.add(Objective.maximize(expr));
    }

    public void addMinimizeObjective(AstSetExpr expr) {
        objectives.add(Objective.minimize(expr));
    }

    public Objective[] getObjectives() {
        return objectives.toArray(new Objective[objectives.size()]);
    }

    public AstModel getModel() {
        return model;
    }
}
