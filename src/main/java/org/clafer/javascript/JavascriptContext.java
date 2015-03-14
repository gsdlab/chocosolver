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
import org.clafer.scope.ScopeBuilder;
import org.mozilla.javascript.NativeJavaObject;
import org.mozilla.javascript.Scriptable;

/**
 *
 * @author jimmy
 */
public class JavascriptContext {

    private final AstModel model = Asts.newModel();
    private final Map<String, Integer> scope = new HashMap<>();
    private final ScopeBuilder scopeBuilder = Scope.builder();
    private final List<Objective> objectives = new ArrayList<>(0);

    public AstModel getModel() {
        return model;
    }

    public void setScope(Map<String, Number> scope) {
        for (Entry<String, Number> entry : scope.entrySet()) {
            // Javascript integers are doubles.
            this.scope.put(entry.getKey(), entry.getValue().intValue());
        }
    }

    public void setDefaultScope(int defaultScope) {
        scopeBuilder.defaultScope(defaultScope);
    }

    public void setIntRange(int intLow, int intHigh) {
        scopeBuilder.intLow(intLow).intHigh(intHigh);
    }

    public void setMulRange(int mulLow, int mulHigh) {
        scopeBuilder.mulLow(mulLow).mulHigh(mulHigh);
    }

    public void setStringLength(int stringLength) {
        scopeBuilder.stringLength(stringLength);
    }

    public void setCharRange(char charLow, char charHigh) {
        scopeBuilder.charLow(charLow).charHigh(charHigh);
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
            scopeBuilder.setScope(clafer, entry.getValue());
        }
        return scopeBuilder.toScope();
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
}
