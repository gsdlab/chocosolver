package org.clafer.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstModel;

/**
 *
 * @author jimmy
 */
public class TypeHierarchyDepthAnalysis {

    public static Map<AstAbstractClafer, Integer> analyze(AstModel model) {
        Map<AstAbstractClafer, Integer> depths = new HashMap<AstAbstractClafer, Integer>();

        for (final AstAbstractClafer clafer : model.getAbstractClafers()) {
            int depth = 0;
            for (AstAbstractClafer sup = clafer; sup.hasSuperClafer(); sup = sup.getSuperClafer()) {
                depth++;
            }
            depths.put(clafer, depth);
        }

        return depths;
    }
}
