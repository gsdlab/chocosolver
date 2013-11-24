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
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrTraverser;

/**
 *
 * @author jimmy
 */
public class CommonSubexpression {

    private CommonSubexpression() {
    }

    public static Set<IrExpr> findCommonSubexpressions(IrModule module) {
        CommonSubexpressionFinder finder = new CommonSubexpressionFinder();
        finder.traverse(module, null);
        return finder.duplicates;
    }

    private static class CommonSubexpressionFinder extends IrTraverser<Void> {

        private final Set<IrExpr> seen = new HashSet<>();
        private final Set<IrExpr> duplicates = new HashSet<>();

        @Override
        public IrIntExpr visit(IrElement ir, Void a) {
            traverse(ir.getArray(), a);
            traverse(ir.getIndex(), a);
            if (!seen.add(ir)) {
                duplicates.add(ir);
            }
            return ir;
        }

        @Override
        public IrSetExpr visit(IrArrayToSet ir, Void a) {
            traverse(ir.getArray(), a);
            if (!seen.add(ir)) {
                duplicates.add(ir);
            }
            return ir;
        }

        @Override
        public IrSetExpr visit(IrJoinRelation ir, Void a) {
            traverse(ir.getTake(), a);
            traverse(ir.getChildren(), a);
            if (!seen.add(ir)) {
                duplicates.add(ir);
            }
            return ir;
        }

        @Override
        public IrSetExpr visit(IrJoinFunction ir, Void a) {
            traverse(ir.getTake(), a);
            traverse(ir.getRefs(), a);
            if (!seen.add(ir)) {
                duplicates.add(ir);
            }
            return ir;
        }
    }
}
