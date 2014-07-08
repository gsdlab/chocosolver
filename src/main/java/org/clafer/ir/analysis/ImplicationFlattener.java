package org.clafer.ir.analysis;

import java.util.ArrayList;
import java.util.List;
import org.clafer.ir.IrAnd;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrIfThenElse;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrNotImplies;
import org.clafer.ir.IrRewriter;
import static org.clafer.ir.Irs.*;

/**
 *
 * @author jimmy
 */
public class ImplicationFlattener {

    private ImplicationFlattener() {
    }

    /**
     * Rewrites expressions so that ands are never directly under the implies
     * consequent and if-then-else are rewritten to implications. Some analysis
     * become easier while others become harder.
     *
     * @param module the module to flatten
     * @return the flattened module
     */
    public static IrModule flatten(IrModule module) {
        List<IrBoolExpr> constraints = new ArrayList<>();
        for (IrBoolExpr constraint : module.getConstraints()) {
            constraints.add(flattener.rewrite(constraint, null));
        }
        return new IrModule().addConstraints(constraints);
    }
    private static final IrRewriter<Void> flattener = new IrRewriter<Void>() {
        @Override
        public IrBoolExpr visit(IrImplies ir, Void a) {
            if (ir.getConsequent() instanceof IrAnd) {
                IrAnd and = (IrAnd) ir.getConsequent();
                IrBoolExpr[] inverted = new IrBoolExpr[and.getOperands().length];
                for (int i = 0; i < inverted.length; i++) {
                    inverted[i] = rewrite(implies(ir.getAntecedent(), rewrite(and.getOperands()[i], a)), a);
                }
                return and(inverted);
            }
            return ir;
        }

        @Override
        public IrBoolExpr visit(IrNotImplies ir, Void a) {
            if (ir.getConsequent() instanceof IrAnd) {
                IrAnd and = (IrAnd) ir.getConsequent();
                IrBoolExpr[] inverted = new IrBoolExpr[and.getOperands().length];
                for (int i = 0; i < inverted.length; i++) {
                    inverted[i] = rewrite(notImplies(ir.getAntecedent(), rewrite(and.getOperands()[i], a)), a);
                }
                return and(inverted);
            }
            return ir;
        }

        @Override
        public IrBoolExpr visit(IrIfThenElse ir, Void a) {
            return and(
                    rewrite(implies(ir.getAntecedent(), ir.getConsequent()), a),
                    rewrite(implies(not(ir.getAntecedent()), ir.getAlternative()), a));
        }
    };
}
