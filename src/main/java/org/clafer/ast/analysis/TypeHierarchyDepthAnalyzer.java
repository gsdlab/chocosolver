package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;

/**
 *
 * @author jimmy
 */
public class TypeHierarchyDepthAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstAbstractClafer, Integer> depthMap = new HashMap<AstAbstractClafer, Integer>();

        for (final AstAbstractClafer clafer : analysis.getAbstractClafers()) {
            List<AstAbstractClafer> hierarchy = new ArrayList<AstAbstractClafer>();
            for (AstAbstractClafer sup = clafer; sup.hasSuperClafer(); sup = sup.getSuperClafer()) {
                if (hierarchy.contains(sup)) {
                    throw new AnalysisException("Cycle in type hierarchy " + hierarchy);
                }
                hierarchy.add(sup);
            }
            depthMap.put(clafer, hierarchy.size());
        }

        List<AstAbstractClafer> abstractClafers = new ArrayList<AstAbstractClafer>(analysis.getAbstractClafers());
        AnalysisUtil.descendingDepths(abstractClafers, depthMap);

        return analysis.setAbstractClafers(abstractClafers).setDepthMap(depthMap);
    }
}
