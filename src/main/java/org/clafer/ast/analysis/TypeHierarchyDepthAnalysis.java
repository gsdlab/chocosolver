package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
            List<AstAbstractClafer> hierarchy = new ArrayList<AstAbstractClafer>(0);
            for (AstAbstractClafer sup = clafer; sup.hasSuperClafer(); sup = sup.getSuperClafer()) {
                if (hierarchy.contains(sup)) {
                    throw new AnalysisException("Cycle in type hierarchy " + hierarchy);
                }
                hierarchy.add(sup);
            }
            depths.put(clafer, hierarchy.size());
        }

        AnalysisUtil.descendingDepths(model.getAbstractClafers(), depths);

        return depths;
    }
}
