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
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrLone;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrOffset;
import org.clafer.ir.IrOr;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.Irs;
import static org.clafer.ir.Irs.add;
import static org.clafer.ir.Irs.and;
import static org.clafer.ir.Irs.equal;
import static org.clafer.ir.Irs.greaterThan;
import static org.clafer.ir.Irs.greaterThanEqual;
import static org.clafer.ir.Irs.ifOnlyIf;
import static org.clafer.ir.Irs.ifThenElse;
import static org.clafer.ir.Irs.lessThanEqual;
import static org.clafer.ir.Irs.lone;
import static org.clafer.ir.Irs.mul;
import static org.clafer.ir.Irs.not;
import static org.clafer.ir.Irs.offset;
import static org.clafer.ir.Irs.or;
import static org.clafer.ir.Irs.sub;
import static org.clafer.ir.Irs.ternary;

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
        } else if (expr instanceof IrOr) {
            IrOr or = (IrOr) expr;
            if (or.getOperands().length == 2) {
                IrBoolExpr left = or.getOperands()[0];
                IrBoolExpr right = or.getOperands()[1];
                return Stream.of(
                        new Compare(not(left), IrCompare.Op.LessThanEqual, right),
                        new Compare(not(right), IrCompare.Op.LessThanEqual, left)
                );
            }
        } else if (expr instanceof IrIfOnlyIf) {
            IrIfOnlyIf ifOnlyIf = (IrIfOnlyIf) expr;
            return Stream.of(
                    new Compare(ifOnlyIf.getLeft(), IrCompare.Op.Equal, ifOnlyIf.getRight()),
                    new Compare(ifOnlyIf.getRight(), IrCompare.Op.Equal, ifOnlyIf.getLeft()));
        }
        return Stream.empty();
    }

    private static Stream<Compare> moreCompares(IrIntExpr expr) {
        if (expr instanceof IrNot) {
            IrNot not = (IrNot) expr;
            return Stream.of(
                    new Compare(not.getExpr(), IrCompare.Op.Equal, Irs.Zero),
                    new Compare(Irs.Zero, IrCompare.Op.Equal, not.getExpr()));
        }
        return compares(expr);
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
        public IrBoolExpr visit(IrIfThenElse ir, Void a) {
            IrBoolExpr antecedent = rewrite(ir.getAntecedent(), a);
            IrBoolExpr consequent = rewrite(ir.getConsequent(), a);
            IrBoolExpr alternative = rewrite(ir.getAlternative(), a);
            Optional<IrBoolExpr> opt
                    = moreCompares(consequent).flatMap(l
                            -> moreCompares(alternative).map(r
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
        if (domain.size() == 2 && right.isConstant()) {
            int constant = right.getLowBound();
            switch (op) {
                case Equal:
                    // Rewrite
                    //     lone(bool, int = 888)
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) <= 888 - int
                    //     asInt(bool) + int <= 888
                    if (domain.getHighBound() == constant) {
                        return lessThanEqual(add(antecedent, left),
                                domain.getHighBound());
                    }
                    // Rewrite
                    //     lone(bool, int = -3)
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) <= int - (-3)
                    if (domain.getLowBound() == constant) {
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
                    if (domain.getHighBound() == constant) {
                        return lessThanEqual(antecedent,
                                sub(left, domain.getLowBound()));
                    }
                    // Rewrite
                    //     lone(bool, int != -3)
                    //         where dom(int) = {-3, 888}
                    // to
                    //     asInt(bool) <= 888 - int
                    //     asInt(bool) + int <= 888
                    if (domain.getLowBound() == constant) {
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
        if (right.isConstant()) {
            int constant = right.getLowBound();
            if (constant == domain.getHighBound()) {
                int span = constant - domain.getLowBound();
                switch (op) {
                    case Equal:
                        if (domain.size() == 2) {
                            // Optimize antecedent || (left == max) to
                            // antecedent + left > min
                            return greaterThan(add(antecedent, left), domain.getLowBound());
                        }
                        // Optimize antecedent || (left == max) to
                        // left >= max - span * antecedent
                        return greaterThanEqual(left, sub(constant, mul(span, antecedent, boundDomain(0, span))));
                    case NotEqual:
                        if (domain.size() == 2) {
                            // Optimize antecedent || (left != max) to
                            // antecedent > left - max
                            return greaterThan(antecedent, sub(left, constant));
                        }
                        // Optimize antecedent || (left != max) to
                        // left <= max - !antecedent
                        return lessThanEqual(left, sub(constant, not(antecedent)));
                }
            } else if (constant == left.getLowBound()) {
                int span = left.getHighBound() - constant;
                switch (op) {
                    case Equal:
                        if (domain.size() == 2) {
                            // Optimize antecedent || (left == min) to
                            // antecedent > left - max
                            return greaterThan(antecedent, sub(left, domain.getHighBound()));
                        }
                        // Optimize antecedent || (left == min) to
                        // left <= min + span * antecedent
                        return lessThanEqual(left, add(constant, mul(span, antecedent, boundDomain(0, span))));
                    case NotEqual:
                        if (domain.size() == 2) {
                            // Optimize antecedent || (left != min) to
                            // antecedent + left > min
                            return greaterThan(add(antecedent, left), constant);
                        }
                        // Optimize antecedent || (left != min) to
                        // left >= min + !antecedent
                        return greaterThanEqual(left, add(constant, not(antecedent)));
                }
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

    private static IrBoolExpr optimizeIfOnlyIfCompare(IrBoolExpr reify, Compare compare) {
        IrIntExpr left = compare.left;
        IrCompare.Op op = compare.op;
        IrIntExpr right = compare.right;
        if (right.isConstant()) {
            int constant = right.getLowBound();
            if (constant == left.getHighBound()) {
                int span = constant - left.getLowBound();
                switch (op) {
                    case Equal:
                        if (left.getDomain().size() == 2) {
                            // Optimize reify <=> (left == max) to
                            // left = min + span * reify.
                            return equal(left, add(left.getLowBound(), mul(span, reify, boundDomain(0, span))));
                        }
                        // Optimize reify <=> (left == max) to
                        // (left <= max - !reify) && (left >= max - span * !reify).
                        return and(
                                lessThanEqual(left, sub(constant, not(reify))),
                                greaterThanEqual(left, sub(constant, mul(span, not(reify), boundDomain(0, span)))));
                    case NotEqual:
                        if (left.getDomain().size() == 2) {
                            // Optimize reify <=> (left != max) to
                            // left = max - span * reify.
                            return equal(left, sub(constant, mul(span, reify, boundDomain(0, span))));
                        }
                        // Optimize reify <=> (left != max) to
                        // (left <= max - reify) && (left >= max - span * reify).
                        return and(
                                lessThanEqual(left, sub(constant, reify)),
                                greaterThanEqual(left, sub(constant, mul(span, reify, boundDomain(0, span)))));
                }
            } else if (constant == left.getLowBound()) {
                int span = left.getHighBound() - constant;
                switch (op) {
                    case Equal:
                        if (left.getDomain().size() == 2) {
                            // Optimize reify <=> (left == min) to
                            // left = max - span * reify.
                            return equal(left, sub(left.getHighBound(), mul(span, reify, boundDomain(0, span))));
                        }
                        // Optimize reify <=> (left == min) to
                        // (left >= min + !reify) && (left <= min + span * !reify).
                        return and(greaterThanEqual(left, add(constant, not(reify))),
                                lessThanEqual(left, add(constant, mul(span, not(reify), boundDomain(0, span)))));
                    case NotEqual:
                        if (left.getDomain().size() == 2) {
                            // Optimize reify <=> (left != min) to
                            // left = min + span * reify.
                            return equal(left, add(constant, mul(span, reify, boundDomain(0, span))));
                        }
                        // Optimize reify <=> (left != min) to
                        // (left >= min + reify) && (left <= min + span * reify).
                        return and(greaterThanEqual(left, add(constant, reify)),
                                lessThanEqual(left, add(constant, mul(span, reify, boundDomain(0, span)))));
                }
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

        @Override
        public String toString() {
            return left + " " + op.getSyntax() + " " + right;
        }
    }
}
