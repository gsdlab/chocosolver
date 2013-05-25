package org.clafer.javascript;

import org.clafer.scope.Scope;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.ast.Asts;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstUtil;

/**
 *
 * @author jimmy
 */
public class JavascriptContext {

    private final Map<String, Integer> scope = new HashMap<String, Integer>();
    private int defaultScope = 1;
    private int intLow = -16;
    private int intHigh = 16;
    private final AstModel model = Asts.newModel();

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

    public Scope getScope() {
        Map<AstClafer, Integer> resolvedScope = new HashMap<AstClafer, Integer>();
        Map<String, AstClafer> resolvedClafers = getClafersMap(model);
        for (Entry<String, Integer> entry : scope.entrySet()) {
            String key = entry.getKey();
            AstClafer clafer = resolvedClafers.get(key);
            if (clafer == null) {
                System.out.println(resolvedClafers);
                throw new IllegalStateException("Cannot set scope for unknown Clafer \"" + key + "\"");
            }
            resolvedScope.put(clafer, entry.getValue());
        }
        return new Scope(resolvedScope, defaultScope, intLow, intHigh);
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
