package org.clafer.tree.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.tree.AbstractClafer;
import org.clafer.tree.ClaferModel;

/**
 *
 * @author jimmy
 */
public class TypeHierarchyDepthAnalysis {

    public static Map<AbstractClafer, Integer> analyze(ClaferModel model) {
        Map<AbstractClafer, Integer> depths = new HashMap<AbstractClafer, Integer>();
        for (AbstractClafer clafer : model.getAbstractClafers()) {
            int depth;
            for (depth = 0; clafer.hasSuperClafer(); depth++) {
                clafer = clafer.getSuperClafer();
            }
            depths.put(clafer, depth);
        }
        return depths;
    }
}
