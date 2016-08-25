package org.clafer.ir.analysis;

import org.clafer.domain.Domain;
import static org.clafer.domain.Domain.boundDomain;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrCompare;
import static org.clafer.ir.IrCompare.Op.Equal;
import static org.clafer.ir.IrCompare.Op.NotEqual;
import org.clafer.ir.IrIfOnlyIf;
import org.clafer.ir.IrIfThenElse;
import org.clafer.ir.IrImplies;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrLone;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrOffset;
import org.clafer.ir.IrOr;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrUtil;
import org.clafer.ir.IrXor;
import static org.clafer.ir.Irs.add;
import static org.clafer.ir.Irs.and;
import static org.clafer.ir.Irs.equal;
import static org.clafer.ir.Irs.greaterThan;
import static org.clafer.ir.Irs.greaterThanEqual;
import static org.clafer.ir.Irs.ifOnlyIf;
import static org.clafer.ir.Irs.ifThenElse;
import static org.clafer.ir.Irs.implies;
import static org.clafer.ir.Irs.lessThanEqual;
import static org.clafer.ir.Irs.lone;
import static org.clafer.ir.Irs.mul;
import static org.clafer.ir.Irs.not;
import static org.clafer.ir.Irs.offset;
import static org.clafer.ir.Irs.or;
import static org.clafer.ir.Irs.sub;
import static org.clafer.ir.Irs.ternary;
import static org.clafer.ir.Irs.xor;

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
        return optimizer.rewrite(module, null);
    }
    private static final IrRewriter<Void> optimizer = new IrRewriter<Void>() {
        @Override
        public IrBoolExpr visit(IrLone ir, Void a) {
            IrBoolExpr[] operands = rewrite(ir.getOperands(), a);
            if (operands.length == 2) {
                if (operands[0] instanceof IrCompare) {
                    IrBoolExpr antecedent = operands[1];
                    IrCompare compare = (IrCompare) operands[0];
                    if (compare.getOp().isEquality()) {
                        IrBoolExpr opt = optimizeLoneCompare(antecedent, compare.getLeft(), compare.getOp(), compare.getRight());
                        if (opt == null) {
                            opt = optimizeLoneCompare(antecedent, compare.getRight(), compare.getOp(), compare.getLeft());
                        }
                        if (opt != null) {
                            return opt;
                        }
                    }
                }
                if (operands[1] instanceof IrCompare) {
                    IrBoolExpr antecedent = operands[0];
                    IrCompare compare = (IrCompare) operands[1];
                    if (compare.getOp().isEquality()) {
                        IrBoolExpr opt = optimizeLoneCompare(antecedent, compare.getLeft(), compare.getOp(), compare.getRight());
                        if (opt == null) {
                            opt = optimizeLoneCompare(antecedent, compare.getRight(), compare.getOp(), compare.getLeft());
                        }
                        if (opt != null) {
                            return opt;
                        }
                    }
                }
            }
            return changed(ir.getOperands(), operands)
                    ? lone(operands)
                    : ir;
        }

        @Override
        public IrBoolExpr visit(IrOr ir, Void a) {
            IrBoolExpr[] operands = rewrite(ir.getOperands(), a);
            if (operands.length == 2) {
                if (operands[0] instanceof IrCompare) {
                    IrBoolExpr antecedent = operands[1];
                    IrCompare compare = (IrCompare) operands[0];
                    if (compare.getOp().isEquality()) {
                        IrBoolExpr opt = optimizeOrCompare(antecedent, compare.getLeft(), compare.getOp(), compare.getRight());
                        if (opt == null) {
                            opt = optimizeOrCompare(antecedent, compare.getRight(), compare.getOp(), compare.getLeft());
                        }
                        if (opt != null) {
                            return opt;
                        }
                    }
                }
                if (operands[1] instanceof IrCompare) {
                    IrBoolExpr antecedent = operands[0];
                    IrCompare compare = (IrCompare) operands[1];
                    if (compare.getOp().isEquality()) {
                        IrBoolExpr opt = optimizeOrCompare(antecedent, compare.getLeft(), compare.getOp(), compare.getRight());
                        if (opt == null) {
                            opt = optimizeOrCompare(antecedent, compare.getRight(), compare.getOp(), compare.getLeft());
                        }
                        if (opt != null) {
                            return opt;
                        }
                    }
                }
            }
            return changed(ir.getOperands(), operands)
                    ? or(operands)
                    : ir;
        }

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
                if (compare.getOp().isEquality()) {
                    IrBoolExpr opt = optimizeImplicationCompare(antecedent, compare.getLeft(), compare.getOp(), compare.getRight());
                    if (opt == null) {
                        opt = optimizeImplicationCompare(antecedent, compare.getRight(), compare.getOp(), compare.getLeft());
                    }
                    if (opt != null) {
                        return opt;
                    }
                }
            }
            return changed(ir.getAntecedent(), antecedent)
                    || changed(ir.getConsequent(), consequent)
                            ? implies(antecedent, consequent)
                            : ir;
        }

        @Override
        public IrBoolExpr visit(IrIfThenElse ir, Void a) {
            IrBoolExpr antecedent = rewrite(ir.getAntecedent(), a);
            IrBoolExpr consequent = rewrite(ir.getConsequent(), a);
            IrBoolExpr alternative = rewrite(ir.getAlternative(), a);
            if (consequent instanceof IrCompare && alternative instanceof IrCompare) {
                IrCompare compare1 = (IrCompare) consequent;
                IrCompare compare2 = (IrCompare) alternative;
                IrBoolExpr opt = optimizeIfThenElseCompare(antecedent,
                        compare1.getLeft(), compare1.getOp(), compare1.getRight(),
                        compare2.getLeft(), compare2.getOp(), compare2.getRight());
                if (opt != null) {
                    return opt;
                }
            }
            return changed(ir.getAntecedent(), antecedent)
                    || changed(ir.getConsequent(), consequent)
                    || changed(ir.getAlternative(), alternative)
                            ? ifThenElse(antecedent, consequent, alternative)
                            : ir;
        }

        @Override
        public IrBoolExpr visit(IrIfOnlyIf ir, Void a) {
            IrBoolExpr left = rewrite(ir.getLeft(), a);
            IrBoolExpr right = rewrite(ir.getRight(), a);
            if (right instanceof IrCompare) {
                IrCompare compare = (IrCompare) right;
                IrBoolExpr opt = optimizeIfOnlyIfCompare(left, compare.getLeft(), compare.getOp(), compare.getRight());
                if (opt == null) {
                    opt = optimizeIfOnlyIfCompare(left, compare.getRight(), compare.getOp(), compare.getLeft());
                }
                if (opt != null) {
                    return opt;
                }
            }
            return changed(ir.getLeft(), left) || changed(ir.getRight(), right)
                    ? ifOnlyIf(left, right)
                    : ir;
        }

        @Override
        public IrBoolExpr visit(IrXor ir, Void a) {
            IrBoolExpr left = rewrite(ir.getLeft(), a);
            IrBoolExpr right = rewrite(ir.getRight(), a);
            if (right instanceof IrCompare) {
                IrCompare compare = (IrCompare) right;
                IrBoolExpr opt = optimizeXorCompare(left, compare.getLeft(), compare.getOp(), compare.getRight());
                if (opt == null) {
                    opt = optimizeXorCompare(left, compare.getRight(), compare.getOp(), compare.getLeft());
                }
                if (opt != null) {
                    return opt;
                }
            }
            return changed(ir.getLeft(), left) || changed(ir.getRight(), right)
                    ? xor(left, right)
                    : ir;
        }

        @Override
        public IrSetExpr visit(IrOffset ir, Void a) {
            if (ir.getSet() instanceof IrOffset) {
                // Rewrite
                //    offset(offset(set, a), b)
                // to
                //    offset(set, a + b)
                // This optimization is important for going multiple steps up the
                // hierarchy.
                IrOffset innerOffset = (IrOffset) ir.getSet();
                return rewrite(offset(innerOffset.getSet(),
                        ir.getOffset() + innerOffset.getOffset()), a);
            }
            return super.visit(ir, a);
        }
    };

    /**
     * Optimize {@code lone(antecedent, left `op` right)} where `op` is = or !=.
     */
    private static IrBoolExpr optimizeLoneCompare(IrBoolExpr antecedent, IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
        Domain domain = left.getDomain();
        Integer constant = IrUtil.getConstant(right);
        if (domain.size() == 2 && constant != null) {
            switch (op) {
                case Equal:
                    // Rewrite
                    //     lone(bool, int = 888)
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) <= 888 - int
                    //     asInt(bool) + int <= 888
                    if (domain.getHighBound() == constant.intValue()) {
                        return lessThanEqual(add(antecedent, left),
                                domain.getHighBound());
                    }
                    // Rewrite
                    //     lone(bool, int = -3)
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) <= int - (-3)
                    if (domain.getLowBound() == constant.intValue()) {
                        return lessThanEqual(antecedent,
                                sub(left, domain.getLowBound()));
                    }
                    break;
                case NotEqual:
                    // Rewrite
                    //     lone(bool, int != 888)
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) <= int - (-3)
                    if (domain.getHighBound() == constant.intValue()) {
                        return lessThanEqual(antecedent,
                                sub(left, domain.getLowBound()));
                    }
                    // Rewrite
                    //     lone(bool, int != -3)
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) <= 888 - int
                    //     asInt(bool) + int <= 888
                    if (domain.getLowBound() == constant.intValue()) {
                        return lessThanEqual(add(antecedent, left),
                                domain.getHighBound());
                    }
                    break;
            }
        }
        return null;
    }

    /**
     * Optimize {@code antecedent or (left `op` right)} where `op` is = or !=.
     */
    private static IrBoolExpr optimizeOrCompare(IrBoolExpr antecedent, IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
        Domain domain = left.getDomain();
        Integer constant = IrUtil.getConstant(right);
        if (domain.size() == 2 && constant != null) {
            switch (op) {
                case Equal:
                    // Rewrite
                    //     bool or int = 888
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) > (-3) - int
                    //     asInt(bool) + int > (-3)
                    if (domain.getHighBound() == constant.intValue()) {
                        return greaterThan(add(antecedent, left),
                                domain.getLowBound());
                    }
                    // Rewrite
                    //     bool or int = -3
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) > int - 888
                    if (domain.getLowBound() == constant.intValue()) {
                        return greaterThan(antecedent,
                                sub(left, domain.getHighBound()));
                    }
                    break;
                case NotEqual:
                    // Rewrite
                    //     bool or int != 888
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) > int - 888
                    if (domain.getHighBound() == constant.intValue()) {
                        return greaterThan(antecedent,
                                sub(left, domain.getHighBound()));
                    }
                    // Rewrite
                    //     bool or int != -3
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) > (-3) - int
                    //     asInt(bool) + int > (-3)
                    if (domain.getLowBound() == constant.intValue()) {
                        return greaterThan(add(antecedent, left),
                                domain.getLowBound());
                    }
                    break;
            }
        }
        return null;
    }

    /**
     * Optimize {@code antecedent => (left `op` right)} where `op` is = or !=.
     */
    private static IrBoolExpr optimizeImplicationCompare(IrBoolExpr antecedent, IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
        Domain domain = left.getDomain();
        Integer constant = IrUtil.getConstant(right);
        if (domain.size() == 2 && constant != null) {
            switch (op) {
                case Equal:
                    // Rewrite
                    //     bool => int = 888
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) <= int - (-3)
                    if (domain.getHighBound() == constant.intValue()) {
                        return lessThanEqual(antecedent,
                                sub(left, domain.getLowBound()));
                    }
                    // Rewrite
                    //     bool => int = -3
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) <= 888 - int
                    //     asInt(bool) + int <= 888
                    if (domain.getLowBound() == constant.intValue()) {
                        return lessThanEqual(add(antecedent, left),
                                domain.getHighBound());
                    }
                    break;
                case NotEqual:
                    // Rewrite
                    //     bool => int != 888
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) <= 888 - int
                    //     asInt(bool) + int <= 888
                    if (domain.getHighBound() == constant.intValue()) {
                        return lessThanEqual(add(antecedent, left),
                                domain.getHighBound());
                    }
                    // Rewrite
                    //     bool => int != -3
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) <= int - (-3)
                    if (domain.getLowBound() == constant.intValue()) {
                        return lessThanEqual(antecedent,
                                sub(left, domain.getLowBound()));
                    }
                    break;
            }
        }
        return null;
    }

    /**
     * Optimize {@code if b then x = y else x = z} to {@code x = b ? y : z}.
     */
    private static IrBoolExpr optimizeIfThenElseCompare(
            IrBoolExpr condition,
            IrIntExpr left1, IrCompare.Op op1, IrIntExpr right1,
            IrIntExpr left2, IrCompare.Op op2, IrIntExpr right2) {
        if (IrCompare.Op.Equal.equals(op1) && IrCompare.Op.Equal.equals(op2) && left1.equals(left2)) {
            return equal(left1, ternary(condition, right1, right2));
        }
        return null;
    }

    /**
     * Optimize {@code reify <=> (left != max)} to
     * {@code (left <= max - reify) && (left >= max - span * reify)}.
     */
    private static IrBoolExpr optimizeIfOnlyIfCompare(IrBoolExpr reify, IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
        Integer constant = IrUtil.getConstant(right);
        if (constant != null && constant.equals(left.getHighBound())) {
            switch (op) {
                case NotEqual:
                    int span = constant - left.getLowBound();
                    IrBoolExpr expr1 = lessThanEqual(left, sub(constant, reify));
                    IrBoolExpr expr2 = greaterThanEqual(left, sub(constant, mul(span, reify, boundDomain(0, span))));
                    return and(expr1, expr2);
            }
        }
        return null;
    }

    /**
     * Optimize {@code reify ^ (left = max)} to
     * {@code (left <= max - reify) && (left >= max - span * reify)}.
     */
    private static IrBoolExpr optimizeXorCompare(IrBoolExpr reify, IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
        Integer constant = IrUtil.getConstant(right);
        if (constant != null && constant.equals(left.getHighBound())) {
            switch (op) {
                case Equal:
                    int span = constant - left.getLowBound();
                    IrBoolExpr expr1 = lessThanEqual(left, sub(constant, reify));
                    IrBoolExpr expr2 = greaterThanEqual(left, sub(constant, mul(span, reify, boundDomain(0, span))));
                    return and(expr1, expr2);
            }
        }
        return null;
    }
}
