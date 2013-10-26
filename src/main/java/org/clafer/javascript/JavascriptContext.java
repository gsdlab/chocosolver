package org.clafer.javascript;

import java.util.ArrayList;
import org.clafer.scope.Scope;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.ast.Asts;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstUtil;
import org.clafer.objective.Objective;

/**
 *
 * @author jimmy
 */
public class JavascriptContext {

    private final Map<String, Integer> scope = new HashMap<String, Integer>();
    private final List<Objective> objectives = new ArrayList<Objective>(0);
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

    public Scope getScope() {
        Map<AstClafer, Integer> resolvedScope = new HashMap<AstClafer, Integer>();
        Map<String, AstClafer> resolvedClafers = getClafersMap(model);
        for (Entry<String, Integer> entry : scope.entrySet()) {
            String key = entry.getKey();
            AstClafer clafer = resolvedClafers.get(key);
            if (clafer == null) {
                throw new IllegalStateException("Cannot set scope for unknown Clafer \"" + key + "\", "
                        + "known Clafers " + resolvedClafers.keySet() + ".");
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

    private static Map<String, AstClafer> getClafersMap(AstModel model) {
        List<AstClafer> clafers = AstUtil.getClafers(model);
        Map<String, AstClafer> map = new HashMap<String, AstClafer>();
        for (AstClafer clafer : clafers) {
            map.put(clafer.getName(), clafer);
        }
        assert map.size() == clafers.size() : "Duplicate Clafer name";
        return map;
    }
}
