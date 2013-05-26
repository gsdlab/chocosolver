package org.clafer.ir.analysis;

import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrCompare;
import static org.clafer.ir.IrCompare.Op.Equal;
import static org.clafer.ir.IrCompare.Op.NotEqual;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.Irs.*;

/**
 *
 * @author jimmy
 */
public class Optimizer {

    private Optimizer() {
    }

    /**
     * Optimize the module.
     *
     * @param module the module to optimize
     * @return the optimized module
     */
    public static IrModule optimize(IrModule module) {
        return module.withConstraints(optimizer.rewrite(and(module.getConstraints()), null));
    }
    private static final IrRewriter<Void> optimizer = new IrRewriter<Void>() {
        @Override
        public IrBoolExpr visit(IrImplies ir, Void a) {
            // Rewrite
            //     !a => !b
            // to
            //     b => a
            if (ir.getAntecedent().isNegative() && ir.getConsequent().isNegative()) {
                return rewrite(implies(not(ir.getConsequent()), not(ir.getAntecedent())), a);
            }
            // Rewrite
            //     !a => b
            // to
            //     a or b
            if (ir.getAntecedent().isNegative()) {
                return rewrite(or(not(ir.getAntecedent()), ir.getConsequent()), a);
            }
            // Rewrite
            //     a => !b
            // to
            //     a + b <= 1
            if (ir.getConsequent().isNegative()) {
                return rewrite(lone(ir.getAntecedent(), not(ir.getConsequent())), a);
            }
            IrBoolExpr antecedent = rewrite(ir.getAntecedent(), a);
            IrBoolExpr consequent = rewrite(ir.getConsequent(), a);
            if (consequent instanceof IrCompare) {
                IrCompare compare = (IrCompare) consequent;
                IrBoolExpr opt = optimizeImplicationCompare(antecedent, compare.getLeft(), compare.getOp(), compare.getRight());
                if (opt == null) {
                    opt = optimizeImplicationCompare(antecedent, compare.getRight(), compare.getOp().reverse(), compare.getLeft());
                }
                if (opt != null) {
                    return opt;
                }
            }
            return implies(antecedent, consequent);
        }
    };

    /**
     * Optimize {@code antecedent => (left `op` right)} where `op` is = or !=.
     */
    private static IrBoolExpr optimizeImplicationCompare(IrBoolExpr antecedent, IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
        IrDomain domain = left.getDomain();
        Integer constant = IrUtil.getConstant(right);
        if (domain.size() == 2 && constant != null) {
            switch (op) {
                case Equal:
                    //   bool => int = 888
                    //     where dom(int) = {-3, 888}
                    // optimize as
                    //   asInt(bool) <= int - (-3)
                    if (domain.getHighBound() == constant.intValue()) {
                        return lessThanEqual(asInt(antecedent),
                                sub(left, domain.getLowBound()));
                    }
                    //   bool => int = -3
                    //     where dom(int) = {-3, 888}
                    // optimize as
                    //   asInt(bool) <= 888 - int
                    //   asInt(bool) + int <= 888
                    if (domain.getLowBound() == constant.intValue()) {
                        return lessThanEqual(add(asInt(antecedent), left),
                                domain.getHighBound());
                    }
                    break;
                case NotEqual:
                    //   bool => int != 888
                    //     where dom(int) = {-3, 888}
                    // optimize as
                    //   asInt(bool) <= 888 - int
                    //   asInt(bool) + int <= 888
                    if (domain.getHighBound() == constant.intValue()) {
                        return lessThanEqual(add(asInt(antecedent), left),
                                domain.getHighBound());
                    }
                    //   bool => int != -3
                    //     where dom(int) = {-3, 888}
                    // optimize as
                    //   asInt(bool) <= int - (-3)
                    if (domain.getLowBound() == constant.intValue()) {
                        return lessThanEqual(asInt(antecedent),
                                sub(left, domain.getLowBound()));
                    }
                    break;
            }
        }
        return null;
    }
}
