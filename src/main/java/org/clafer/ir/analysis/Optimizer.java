package org.clafer.ir.analysis;

import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;
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
import org.clafer.ir.IrNot;
import org.clafer.ir.IrOffset;
import org.clafer.ir.IrOr;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrUtil;
import org.clafer.ir.IrXor;
import org.clafer.ir.Irs;
import static org.clafer.ir.Irs.Zero;
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

    private static Stream<Compare> compares(IrIntExpr expr) {
        if (expr instanceof IrCompare) {
            IrCompare compare = (IrCompare) expr;
            switch (compare.getOp()) {
                case Equal:
                case NotEqual:
                    return Stream.of(
                            new Compare(compare.getLeft(), compare.getOp(), compare.getRight()),
                            new Compare(compare.getRight(), compare.getOp(), compare.getLeft()));
                default:
                    return Stream.of(
                            new Compare(compare.getLeft(), compare.getOp(), compare.getRight()));
            }
        } else if (expr instanceof IrNot) {
            IrNot not = (IrNot) expr;
            return Stream.of(
                    new Compare(not.getExpr(), IrCompare.Op.Equal, Zero),
                    new Compare(Irs.Zero, IrCompare.Op.Equal, not.getExpr()));
        } else if (expr instanceof IrIfOnlyIf) {
            IrIfOnlyIf ifOnlyIf = (IrIfOnlyIf) expr;
            return Stream.of(
                    new Compare(ifOnlyIf.getLeft(), IrCompare.Op.Equal, ifOnlyIf.getRight()),
                    new Compare(ifOnlyIf.getRight(), IrCompare.Op.Equal, ifOnlyIf.getLeft()));
        } else if (expr instanceof IrXor) {
            IrXor xor = (IrXor) expr;
            return Stream.of(
                    new Compare(xor.getLeft(), IrCompare.Op.NotEqual, xor.getRight()),
                    new Compare(xor.getRight(), IrCompare.Op.NotEqual, xor.getLeft()));
        } else if (expr instanceof IrImplies) {
            IrImplies implies = (IrImplies) expr;
            return Stream.of(
                    new Compare(implies.getAntecedent(), IrCompare.Op.LessThanEqual, implies.getConsequent()));
        }
        return Stream.empty();
    }

    private static final IrRewriter<Void> optimizer = new IrRewriter<Void>() {
        @Override
        public IrBoolExpr visit(IrLone ir, Void a) {
            IrBoolExpr[] operands = rewrite(ir.getOperands(), a);
            if (operands.length == 2) {
                Optional<IrBoolExpr> opt
                        = Stream.concat(
                                compares(operands[0]).map(r -> optimizeLoneCompare(operands[1], r)),
                                compares(operands[1]).map(r -> optimizeLoneCompare(operands[0], r)))
                        .filter(Objects::nonNull)
                        .findFirst();
                if (opt.isPresent()) {
                    return opt.get();
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
                Optional<IrBoolExpr> opt
                        = Stream.concat(
                                compares(operands[0]).map(r -> optimizeOrCompare(operands[1], r)),
                                compares(operands[1]).map(r -> optimizeOrCompare(operands[0], r)))
                        .filter(Objects::nonNull)
                        .findFirst();
                if (opt.isPresent()) {
                    return opt.get();
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

            Optional<IrBoolExpr> opt
                    = compares(consequent).map(c -> optimizeImplicationCompare(antecedent, c))
                    .filter(Objects::nonNull)
                    .findFirst();
            if (opt.isPresent()) {
                return opt.get();
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
            Optional<IrBoolExpr> opt
                    = compares(consequent).flatMap(l
                            -> compares(alternative).map(r
                                    -> optimizeIfThenElseCompare(antecedent, l, r)))
                    .filter(Objects::nonNull)
                    .findFirst();
            if (opt.isPresent()) {
                return opt.get();
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
            Optional<IrBoolExpr> opt
                    = Stream.concat(
                            compares(right).map(r -> optimizeIfOnlyIfCompare(left, r)),
                            compares(left).map(r -> optimizeIfOnlyIfCompare(right, r)))
                    .filter(Objects::nonNull)
                    .findFirst();
            if (opt.isPresent()) {
                return opt.get();
            }
            return changed(ir.getLeft(), left) || changed(ir.getRight(), right)
                    ? ifOnlyIf(left, right)
                    : ir;
        }

        @Override
        public IrBoolExpr visit(IrXor ir, Void a) {
            IrBoolExpr left = rewrite(ir.getLeft(), a);
            IrBoolExpr right = rewrite(ir.getRight(), a);
            Optional<IrBoolExpr> opt
                    = Stream.concat(
                            compares(right).map(r -> optimizeXorCompare(left, r)),
                            compares(left).map(r -> optimizeXorCompare(right, r)))
                    .filter(Objects::nonNull)
                    .findFirst();
            if (opt.isPresent()) {
                return opt.get();
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
    private static IrBoolExpr optimizeLoneCompare(IrBoolExpr antecedent, Compare compare) {
        IrIntExpr left = compare.left;
        IrCompare.Op op = compare.op;
        IrIntExpr right = compare.right;
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
    private static IrBoolExpr optimizeOrCompare(IrBoolExpr antecedent, Compare compare) {
        IrIntExpr left = compare.left;
        IrCompare.Op op = compare.op;
        IrIntExpr right = compare.right;
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
    private static IrBoolExpr optimizeImplicationCompare(IrBoolExpr antecedent, Compare compare) {
        IrIntExpr left = compare.left;
        IrCompare.Op op = compare.op;
        IrIntExpr right = compare.right;
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
            IrBoolExpr condition, Compare compare1, Compare compare2) {
        IrIntExpr left1 = compare1.left;
        IrCompare.Op op1 = compare1.op;
        IrIntExpr right1 = compare1.right;
        IrIntExpr left2 = compare2.left;
        IrCompare.Op op2 = compare2.op;
        IrIntExpr right2 = compare2.right;
        if (IrCompare.Op.Equal.equals(op1) && IrCompare.Op.Equal.equals(op2) && left1.equals(left2)) {
            return equal(left1, ternary(condition, right1, right2));
        }
        return null;
    }

    /**
     * Optimize {@code reify <=> (left != max)} to
     * {@code (left <= max - reify) && (left >= max - span * reify)} and
     * optimize {@code reify <=> (left == min)} to
     * {@code (left >= min + !reify) && (left <= min + span * !reify)} and .
     */
    private static IrBoolExpr optimizeIfOnlyIfCompare(IrBoolExpr reify, Compare compare) {
        IrIntExpr left = compare.left;
        IrCompare.Op op = compare.op;
        IrIntExpr right = compare.right;
        Integer constant = IrUtil.getConstant(right);
        if (constant != null) {
            if (constant.equals(left.getHighBound())) {
                switch (op) {
                    case NotEqual:
                        int span = constant - left.getLowBound();
                        IrBoolExpr expr1 = lessThanEqual(left, sub(constant, reify));
                        IrBoolExpr expr2 = greaterThanEqual(left, sub(constant, mul(span, reify, boundDomain(0, span))));
                        return and(expr1, expr2);
                }
            } else if (constant.equals(left.getLowBound())) {
                switch (op) {
                    case Equal:
                        int span = left.getHighBound() - constant;
                        IrBoolExpr expr1 = greaterThanEqual(left, add(constant, not(reify)));
                        IrBoolExpr expr2 = lessThanEqual(left, add(constant, mul(span, not(reify), boundDomain(0, span))));
                        return and(expr1, expr2);
                }
            }
        }
        return null;
    }

    /**
     * Optimize {@code reify ^ (left = max)} to
     * {@code (left <= max - reify) && (left >= max - span * reify)}.
     */
    private static IrBoolExpr optimizeXorCompare(IrBoolExpr reify, Compare compare) {
        IrIntExpr left = compare.left;
        IrCompare.Op op = compare.op;
        IrIntExpr right = compare.right;
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

    private static class Compare {

        final IrIntExpr left;
        final IrCompare.Op op;
        final IrIntExpr right;

        public Compare(IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
            this.left = left;
            this.op = op;
            this.right = right;
        }
    }
}
