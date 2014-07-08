package org.clafer.ir.analysis;

import java.util.HashSet;
import java.util.Set;
import org.clafer.collection.Pair;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrCompare;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrVar;
import org.clafer.ir.Irs;

/**
 * Remove constraints that are duplicated. One example is to remove weaker
 * constraints that are already enforced by stronger ones. Another example is
 * due to coalescing, it is possible to end up with multiple cardinality
 * variables for the same set. Remove the duplicates.
 *
 * @author jimmy
 */
public class DuplicateConstraints {

    private DuplicateConstraints() {
    }

    /**
     * Reduce.
     *
     * <ol>
     * <li>a = b & a ≤ b to a = b</li>
     * <li>a ≠ b & a &lt; b to a &lt; b</li>
     * <li>a ≠ b & a ≤ b to a &lt; b</li>
     * <li>a &lt; b & a ≤ b to a &lt; b</li>
     * <li>a ≤ b & b ≤ a to a = b</li>
     * </ol>
     *
     * @param module the module to remove duplicates
     * @return the optimized module
     */
    public static IrModule removeDuplicates(IrModule module) {
        HashSet<Pair<IrIntExpr, IrIntExpr>> equal = new HashSet<>();
        HashSet<Pair<IrIntExpr, IrIntExpr>> notEqual = new HashSet<>();
        HashSet<Pair<IrIntExpr, IrIntExpr>> lessThanEqual = new HashSet<>();
        HashSet<Pair<IrIntExpr, IrIntExpr>> lessThan = new HashSet<>();

        Set<IrBoolExpr> constraints = new HashSet<>(module.getConstraints().size());

        for (IrBoolExpr constraint : module.getConstraints()) {
            if (constraint instanceof IrCompare) {
                IrCompare compare = (IrCompare) constraint;
                IrIntExpr left = compare.getLeft();
                IrIntExpr right = compare.getRight();
                Pair<IrIntExpr, IrIntExpr> pair = new Pair<>(left, right);
                Pair<IrIntExpr, IrIntExpr> converse = new Pair<>(right, left);

                switch (compare.getOp()) {
                    case Equal:
                        lessThanEqual.remove(pair);
                        lessThanEqual.remove(converse);
                        equal.add(pair);
                        equal.remove(converse);
                        break;
                    case NotEqual:
                        if (lessThanEqual.remove(pair)) {
                            lessThan.add(pair);
                        } else if (lessThanEqual.remove(converse)) {
                            lessThan.add(converse);
                        } else if (!lessThan.contains(pair)
                                && !lessThan.contains(converse)) {
                            notEqual.add(pair);
                            notEqual.remove(converse);
                        }
                        break;
                    case LessThan:
                        notEqual.remove(pair);
                        notEqual.remove(converse);
                        lessThanEqual.remove(pair);
                        lessThan.add(pair);
                        break;
                    case LessThanEqual:
                        if (notEqual.remove(pair)) {
                            lessThan.add(pair);
                        } else if (notEqual.remove(converse)) {
                            lessThan.add(converse);
                        } else if (lessThanEqual.remove(converse)) {
                            equal.add(pair);
                        } else if (!equal.contains(pair)
                                && !equal.contains(converse)
                                && !lessThan.contains(pair)) {
                            lessThanEqual.add(pair);
                        }
                        break;
                    default:
                        throw new IllegalStateException();
                }
            } else {
                constraints.add(constraint);
            }
        }

        for (Pair<IrIntExpr, IrIntExpr> p : equal) {
            constraints.add(Irs.equal(p.getFst(), p.getSnd()));
        }
        for (Pair<IrIntExpr, IrIntExpr> p : notEqual) {
            constraints.add(Irs.notEqual(p.getFst(), p.getSnd()));
        }
        for (Pair<IrIntExpr, IrIntExpr> p : lessThan) {
            constraints.add(Irs.lessThan(p.getFst(), p.getSnd()));
        }
        for (Pair<IrIntExpr, IrIntExpr> p : lessThanEqual) {
            constraints.add(Irs.lessThanEqual(p.getFst(), p.getSnd()));
        }

        return new IrModule().addConstraints(constraints);
    }
}
