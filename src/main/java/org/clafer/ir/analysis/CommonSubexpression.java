package org.clafer.ir.analysis;

import java.util.HashSet;
import java.util.Set;
import org.clafer.ir.IrArrayToSet;
import org.clafer.ir.IrElement;
import org.clafer.ir.IrExpr;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrJoinFunction;
import org.clafer.ir.IrJoinRelation;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrSetExpr;

/**
 *
 * @author jimmy
 */
public class CommonSubexpression {

    private CommonSubexpression() {
    }

    public static Set<IrExpr> findCommonSubexpressions(IrModule module) {
        CommonSubexpressionFinder finder = new CommonSubexpressionFinder();
        finder.rewrite(module, null);
        return finder.duplicates;
    }

    private static class CommonSubexpressionFinder extends IrRewriter<Void> {

        private final Set<IrExpr> seen = new HashSet<IrExpr>();
        private final Set<IrExpr> duplicates = new HashSet<IrExpr>();

        @Override
        public IrIntExpr visit(IrElement ir, Void a) {
            rewrite(ir.getArray(), a);
            rewrite(ir.getIndex(), a);
            if (!seen.add(ir)) {
                duplicates.add(ir);
            }
            return ir;
        }

        @Override
        public IrSetExpr visit(IrArrayToSet ir, Void a) {
            rewrite(ir.getArray(), a);
            if (!seen.add(ir)) {
                duplicates.add(ir);
            }
            return ir;
        }

        @Override
        public IrSetExpr visit(IrJoinRelation ir, Void a) {
            rewrite(ir.getTake(), a);
            rewrite(ir.getChildren(), a);
            if (!seen.add(ir)) {
                duplicates.add(ir);
            }
            return ir;
        }

        @Override
        public IrSetExpr visit(IrJoinFunction ir, Void a) {
            rewrite(ir.getTake(), a);
            rewrite(ir.getRefs(), a);
            if (!seen.add(ir)) {
                duplicates.add(ir);
            }
            return ir;
        }
    }
}
