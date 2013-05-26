package org.clafer.ir.analysis;

import org.clafer.ir.IrRewriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.clafer.ir.IrAnd;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrCompare;
import static org.clafer.ir.IrCompare.Op.Equal;
import static org.clafer.ir.IrCompare.Op.NotEqual;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrIfOnlyIf;
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
        IrBoolExpr canonical = canonicalizer.rewrite(and(module.getConstraints()), null);
        return module.withConstraints(optimizer.rewrite(canonical, null));
    }
    private static final IrRewriter<Void> canonicalizer = new IrRewriter<Void>() {
        @Override
        public IrBoolExpr visit(IrAnd ir, Void a) {
            Map<IrBoolExpr, Set<IrBoolExpr>> implications = new HashMap<IrBoolExpr, Set<IrBoolExpr>>();
            List<IrBoolExpr> opts = new ArrayList<IrBoolExpr>(ir.getOperands().length);
            for (IrBoolExpr operand : ir.getOperands()) {
                IrBoolExpr opt = rewrite(operand, a);
                if (opt instanceof IrImplies) {
                    IrImplies implies = (IrImplies) opt;
                    Set<IrBoolExpr> consequents = implications.get(implies.getAntecedent());
                    if (consequents == null) {
                        consequents = new HashSet<IrBoolExpr>();
                        implications.put(implies.getAntecedent(), consequents);
                    }
                    consequents.add(implies.getConsequent());
                } else {
                    opts.add(opt);
                }
            }
            for (Entry<IrBoolExpr, Set<IrBoolExpr>> entry : implications.entrySet()) {
                IrBoolExpr antecedent = entry.getKey();
                Set<IrBoolExpr> consequents = entry.getValue();
                if (!consequents.isEmpty()) {
                    IrBoolExpr notAntecedent = not(antecedent);
                    Set<IrBoolExpr> alternatives = implications.get(notAntecedent);
                    if (alternatives != null && !alternatives.isEmpty()) {
                        Iterator<IrBoolExpr> iter = consequents.iterator();
                        while (iter.hasNext()) {
                            IrBoolExpr consequent = iter.next();
                            IrBoolExpr alternative = not(consequent);
                            if (alternatives.remove(alternative)) {
                                opts.add(
                                        // Either choice is fine, but prefer to pick
                                        // the antecedent that is not negative because
                                        // it looks nicer and some cases easier to
                                        // propagate.
                                        antecedent.isNegative()
                                        ? ifOnlyIf(notAntecedent, alternative)
                                        : ifOnlyIf(antecedent, consequent));
                                iter.remove();
                            }
                        }
                    }
                    for (IrBoolExpr consequent : consequents) {
                        opts.add(implies(antecedent, consequent));
                    }
                    consequents.clear();
                }
            }
            return and(opts);
        }

        @Override
        public IrBoolExpr visit(IrCompare ir, Void a) {
            IrIntExpr left = rewrite(ir.getLeft(), a);
            IrIntExpr right = rewrite(ir.getRight(), a);
            if (ir.getOp().isEquality()) {
                IrDomain leftDomain = left.getDomain();
                IrDomain rightDomain = right.getDomain();
                // This canoicalizes the form somewhat so that is easier for the other
                // analysis to find pattern matches.
                if (leftDomain.size() == 2) {
                    // Prefer the one that's closer to 0.
                    int prefer = leftDomain.getLowBound();
                    int unprefer = leftDomain.getHighBound();
                    if (Math.abs(leftDomain.getLowBound()) > Math.abs(leftDomain.getHighBound())) {
                        prefer = leftDomain.getHighBound();
                        unprefer = leftDomain.getLowBound();
                    }
                    Integer rightConstant = IrUtil.getConstant(right);
                    if (rightConstant != null && rightConstant.intValue() == unprefer) {
                        return compare(left, ir.getOp().negate(), prefer);
                    }
                }
                if (rightDomain.size() == 2) {
                    // Prefer the one that's closer to 0.
                    int prefer = rightDomain.getLowBound();
                    int unprefer = rightDomain.getHighBound();
                    if (Math.abs(rightDomain.getLowBound()) > Math.abs(rightDomain.getHighBound())) {
                        prefer = rightDomain.getHighBound();
                        unprefer = rightDomain.getLowBound();
                    }
                    Integer leftConstant = IrUtil.getConstant(left);
                    if (leftConstant != null && leftConstant.intValue() == unprefer) {
                        return compare(prefer, ir.getOp().negate(), right);
                    }
                }
            }
            return compare(left, ir.getOp(), right);
        }
    };
    private static final IrRewriter<Void> optimizer = new IrRewriter<Void>() {
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

        @Override
        public IrBoolExpr visit(IrIfOnlyIf ir, Void a) {
            IrBoolExpr left = rewrite(ir.getLeft(), a);
            IrBoolExpr right = rewrite(ir.getRight(), a);
            if (left instanceof IrCompare) {
                IrCompare compare = (IrCompare) left;
                IrBoolExpr opt = optimizeIfOnlyIfCompare(right, compare.getLeft(), compare.getOp(), compare.getRight());
                if (opt == null) {
                    opt = optimizeIfOnlyIfCompare(right, compare.getRight(), compare.getOp().reverse(), compare.getLeft());
                }
                if (opt != null) {
                    return opt;
                }
            }
            if (right instanceof IrCompare) {
                IrCompare compare = (IrCompare) right;
                IrBoolExpr opt = optimizeIfOnlyIfCompare(left, compare.getLeft(), compare.getOp(), compare.getRight());
                if (opt == null) {
                    opt = optimizeIfOnlyIfCompare(left, compare.getRight(), compare.getOp().reverse(), compare.getLeft());
                }
                if (opt != null) {
                    return opt;
                }
            }
            return ifOnlyIf(left, right);
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
                                sub(domain.getHighBound(), left));
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

    /**
     * Optimize {@code antecedent <=> (left `op` right)} where `op` is = or !=.
     */
    private static IrBoolExpr optimizeIfOnlyIfCompare(IrBoolExpr antecedent, IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
        IrDomain domain = left.getDomain();
        Integer constant = IrUtil.getConstant(right);
        int span = domain.getHighBound() - domain.getLowBound();
        if (domain.size() == 2 && constant != null) {
            switch (op) {
                case Equal:
                    //   bool <=> int = 888
                    //     where dom(int) = {-3, 888}
                    // optimize as
                    //   asInt(bool) * 891 - 3 = int
                    if (domain.getHighBound() == constant.intValue()) {
                        return equal(add(mul(asInt(antecedent), span), domain.getLowBound()), left);
                    }
                    //   bool <=> int = -3
                    //     where dom(int) = {-3, 888}
                    // optimize as
                    //   asInt(bool) * -891 + 888 = int
                    if (domain.getLowBound() == constant.intValue()) {
                        return equal(add(mul(asInt(antecedent), -span), -domain.getHighBound()), left);
                    }
                    break;
                case NotEqual:
                    //   bool <=> int != 888
                    //     where dom(int) = {-3, 888}
                    // optimize as
                    //   asInt(bool) * -891 + 888 = int
                    if (domain.getHighBound() == constant.intValue()) {
                        return equal(add(mul(asInt(antecedent), -span), -domain.getHighBound()), left);
                    }
                    //   bool <=> int != -3
                    //     where dom(int) = {-3, 888}
                    // optimize as
                    //   asInt(bool) * 891 - 3 = int
                    if (domain.getLowBound() == constant.intValue()) {
                        return equal(add(mul(asInt(antecedent), span), domain.getLowBound()), left);
                    }
                    break;
            }
        }
        return null;
    }
}
