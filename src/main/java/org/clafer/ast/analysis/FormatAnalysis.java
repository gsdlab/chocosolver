package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.ast.scope.Scope;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;

/**
 *
 * @author jimmy
 */
public class FormatAnalysis {

    /**
     * There is two ways to represent a set. Consider the model:
     * 
     *   parent *
     *     child 3
     * 
     * Suppose parent = {0,2,3}.
     * 
     * If child is in LowGroup, then it must choose the lowest 9 elements in its set.
     * So child = {0,1,2,3,4,5,6,7,8}. This is good for many optimizations like skipCard
     * and works for any case but it makes constant operations navigating down the tree
     * more difficult because we don't know where the children lie during compile time.
     * Abstract clafers cannot have LowGroup.
     * 
     * If child is in ParentGroup, then each childSet is grouped together but there may
     * be gaps between the childSets. A clafer has ParentGroup ONLY IF it has exact cardinality,
     * or it is Abstract, otherwise it isn't worthwhile. One reason why is that ParentGroup
     * does not work with our symmetry breaking rules, but exact cardinalities do not need
     * to be broken. Following the example, child = {0,1,2,6,7,8,9,10,11}.
     */
    public static enum Format {

        LowGroup,
        ParentGroup;
    }

    public static Map<AstClafer, Format> analyze(AstModel model, Scope scope) {
        Map<AstClafer, Format> formats = new HashMap<AstClafer, Format>();
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            analyze(abstractClafer, scope, formats);
        }
        for (AstConcreteClafer topClafer : model.getTopClafers()) {
            analyze(topClafer, scope, formats);
        }
        return formats;
    }

    private static void analyze(AstAbstractClafer clafer, Scope scope, Map<AstClafer, Format> formats) {
        formats.put(clafer, Format.ParentGroup);
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, scope, formats);
        }
    }

    private static void analyze(AstConcreteClafer clafer, Scope scope, Map<AstClafer, Format> formats) {
        formats.put(clafer,
                clafer.getCard().isExact()
                && clafer.hasParent()
                && scope.getScope(clafer) >= clafer.getCard().getHigh() * scope.getScope(clafer.getParent())
                ? Format.ParentGroup : Format.LowGroup);
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, scope, formats);
        }
    }
}
