package org.clafer.ir.analysis;

import org.clafer.ir.IrRewriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.clafer.ir.IrAnd;
import org.clafer.ir.IrBoolConstant;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrCompare;
import static org.clafer.ir.IrCompare.Op.Equal;
import static org.clafer.ir.IrCompare.Op.NotEqual;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrModule;
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
     * Rewrites implications that match a specific pattern to a simpler form.
     *
     * @param module the module to optimize
     * @return the optimized module
     */
    public static IrModule optimizeImplications(IrModule module) {
//        List<IrBoolExpr> constraints = new ArrayList<IrBoolExpr>();
//
//        for (IrBoolExpr constraint : module.getConstraints()) {
//            IrBoolExpr opt = null;
//            if (constraint instanceof IrImplies) {
//                IrImplies implies = (IrImplies) constraint;
//                if (implies.getConsequent() instanceof IrCompare) {
//                    IrCompare compare = (IrCompare) implies.getConsequent();
//                    opt = optimizeImplicationCompare(implies.getAntecedent(), compare.getLeft(), compare.getOp(), compare.getRight());
//                    if (opt == null) {
//                        opt = optimizeImplicationCompare(implies.getAntecedent(), compare.getRight(), compare.getOp().reverse(), compare.getLeft());
//                    }
//                }
//            }
//            constraints.add(opt == null ? constraint : opt);
//        }
//        return module.withConstraints(constraints);
        return module.withConstraints(optimizer.rewrite(and(module.getConstraints()), null));
    }
    private static final IrRewriter<Void> optimizer = new IrRewriter<Void>() {
        @Override
        public IrBoolExpr visit(IrAnd ir, Void a) {
            Map<IrBoolExpr, Set<IrBoolExpr>> implications = new HashMap<IrBoolExpr, Set<IrBoolExpr>>();
            List<IrBoolExpr> opts = new ArrayList<IrBoolExpr>(ir.getOperands().length);
            for (IrBoolExpr operand : ir.getOperands()) {
                if (operand instanceof IrImplies) {
                    IrImplies implies = (IrImplies) operand;
                    Set<IrBoolExpr> consequents = implications.get(implies.getAntecedent());
                    if (consequents == null) {
                        consequents = new HashSet<IrBoolExpr>();
                        implications.put(implies.getAntecedent(), consequents);
                    }
                    consequents.add(implies.getConsequent());
                } else {
                    opts.add(rewrite(operand, a));
                }
            }
            for (Entry<IrBoolExpr, Set<IrBoolExpr>> entry : implications.entrySet()) {
                IrBoolExpr antecedent = entry.getKey();
                Set<IrBoolExpr> consequents = entry.getValue();
                if (!consequents.isEmpty()) {
                    IrBoolExpr notAntecedent = not(antecedent);
                    Set<IrBoolExpr> alternatives = implications.get(notAntecedent);
                    if (alternatives != null && !alternatives.isEmpty()) {
                        IrBoolExpr consequent = and(consequents);
                        IrBoolExpr alternative = and(alternatives);
                        if (consequent.equals(not(alternative))) {
                            opts.add(rewrite(
                                    // Either choice is fine, but prefer to pick
                                    // the antecedent that is not negative because
                                    // it looks nicer and some cases easier to
                                    // enforce.
                                    antecedent.isNegative()
                                    ? ifOnlyIf(notAntecedent, alternative)
                                    : ifOnlyIf(antecedent, consequent), a));
                            consequents.clear();
                            alternatives.clear();
                        }
                    }
                    for(IrBoolExpr consequent : consequents) {
                        opts.add(rewrite(implies(antecedent, consequent), a));
                    }
                }
            }
            return and(opts);
        }

        @Override
        public IrBoolExpr visit(IrImplies ir, Void a) {
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
