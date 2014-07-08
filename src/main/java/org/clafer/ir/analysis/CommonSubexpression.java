package org.clafer.ir.analysis;

import java.util.HashSet;
import java.util.Set;
import org.clafer.ir.IrArrayToSet;
import org.clafer.ir.IrCard;
import org.clafer.ir.IrElement;
import org.clafer.ir.IrExpr;
import org.clafer.ir.IrJoinFunction;
import org.clafer.ir.IrJoinRelation;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrTraverser;
import org.clafer.ir.IrVar;

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

        private void check(IrExpr expr) {
            if (!(expr instanceof IrVar) && !seen.add(expr)) {
                duplicates.add(expr);
            }
        }

        @Override
        public Void visit(IrCard ir, Void a) {
            check(ir.getSet());
            return super.visit(ir, a);
        }

        @Override
        public Void visit(IrElement ir, Void a) {
            check(ir);
            return super.visit(ir, a);
        }

        @Override
        public Void visit(IrArrayToSet ir, Void a) {
            check(ir);
            return super.visit(ir, a);
        }

        @Override
        public Void visit(IrJoinRelation ir, Void a) {
            check(ir);
            return super.visit(ir, a);
        }

        @Override
        public Void visit(IrJoinFunction ir, Void a) {
            check(ir);
            return super.visit(ir, a);
        }
    }
}
