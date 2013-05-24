package org.clafer.ir.analysis;

import java.util.ArrayList;
import java.util.List;
import org.clafer.ast.AstCompare;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrCompare;
import static org.clafer.ir.IrCompare.Op.Equal;
import static org.clafer.ir.IrCompare.Op.NotEqual;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrException;
import org.clafer.ir.IrExpr;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.Irs.*;

/**
 *
 * @author jimmy
 */
public class ExprOptimizer {

    private ExprOptimizer() {
    }

    public static IrModule analyze(IrModule module) {
        List<IrBoolExpr> constraints = new ArrayList<IrBoolExpr>();

        for (IrBoolExpr constraint : module.getConstraints()) {
            IrBoolExpr opt = null;
            if (constraint instanceof IrImplies) {
                IrImplies implies = (IrImplies) constraint;
                if (implies.getConsequent() instanceof IrCompare) {
                    IrCompare compare = (IrCompare) implies.getConsequent();
                    opt = optimize(implies.getAntecedent(), compare.getLeft(), compare.getOp(), compare.getRight());
                    if (opt == null) {
                        opt = optimize(implies.getAntecedent(), compare.getRight(), compare.getOp(), compare.getLeft());
                    }
                }
            }
            constraints.add(opt == null ? constraint : opt);
        }
        return module.withConstraints(constraints);
    }

    /**
     * Optimize {@code antecedent => (left `op` right)} where `op` is = or !=.
     */
    private static IrBoolExpr optimize(IrBoolExpr antecedent, IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
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
                    if (domain.getLowBound() == constant.intValue()) {
                        return lessThanEqual(asInt(antecedent),
                                sub(domain.getHighBound(), left));
                    }
                    break;
                case NotEqual:
                    //   bool => int != 888
                    //     where dom(int) = {-3, 888}
                    // optimize as
                    //   asInt(bool) <= 888 - int
                    if (domain.getHighBound() == constant.intValue()) {
                        return lessThanEqual(asInt(antecedent),
                                sub(domain.getLowBound(), left));
                    }
                    //   bool => int != -3
                    //     where dom(int) = {-3, 888}
                    // optimize as
                    //   asInt(bool) <= int - (-3)
                    if (domain.getLowBound() == constant.intValue()) {
                        return lessThanEqual(asInt(antecedent),
                                sub(left, domain.getHighBound()));
                    }
                    break;
            }
        }
        return null;
    }
}
