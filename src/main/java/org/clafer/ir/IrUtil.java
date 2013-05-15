package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.ir.IrDomain.IrBoundDomain;
import solver.constraints.propagators.set.PropIntMemberSet;

/**
 *
 * @author jimmy
 */
public class IrUtil {

    public static <T> T notNull(String message, T t) {
        if (t == null) {
            throw new IrException(message);
        }
        return t;
    }

    public static boolean isTrue(IrConstraint constraint) {
        if (constraint instanceof IrBoolConstraint) {
            IrBoolConstraint boolConstraint = (IrBoolConstraint) constraint;
            return isTrue(boolConstraint.getExpr());
        }
        return false;
    }

    public static boolean isFalse(IrConstraint constraint) {
        if (constraint instanceof IrBoolConstraint) {
            IrBoolConstraint boolConstraint = (IrBoolConstraint) constraint;
            return isFalse(boolConstraint.getExpr());
        }
        return false;
    }

    public static boolean isTrue(IrBoolExpr boolExpr) {
        return Boolean.TRUE.equals(getConstant(boolExpr));
    }

    public static boolean isFalse(IrBoolExpr boolExpr) {
        return Boolean.FALSE.equals(getConstant(boolExpr));
    }

    public static Boolean isConstant(IrBoolExpr boolExpr) {
        return getConstant(boolExpr) != null;
    }

    public static Boolean getConstant(IrBoolExpr boolExpr) {
        if (boolExpr instanceof IrBoolVar) {
            IrBoolVar var = (IrBoolVar) boolExpr;
            return var.isTrue() ? Boolean.TRUE
                    : (var.isFalse() ? Boolean.FALSE : null);
        }
        if (boolExpr instanceof IrNot) {
            IrNot not = (IrNot) boolExpr;
            Boolean constant = getConstant(not.getProposition());
            if (constant != null) {
                // Reverse the boolean
                return constant.booleanValue() ? Boolean.FALSE : Boolean.TRUE;
            }
        }
        return null;
    }

    public static boolean isConstant(IrIntExpr intExpr) {
        return getConstant(intExpr) != null;
    }

    public static Integer getConstant(IrIntExpr intExpr) {
        IrDomain domain = intExpr.getDomain();
        if (domain.size() == 1) {
            return domain.getLowerBound();
        }
        return null;
    }

    public static boolean isConstant(IrSetExpr setExpr) {
        if (setExpr instanceof IrSetVar) {
            IrSetVar var = (IrSetVar) setExpr;
            return var.isConstant();
        }
        return false;
    }

    public static int[] getConstant(IrSetExpr setExpr) {
        if (setExpr instanceof IrSetVar) {
            IrSetVar var = (IrSetVar) setExpr;
            return var.isConstant() ? var.getValue() : null;
        }
        return null;
    }

    public static boolean intersects(IrDomain d1, IrDomain d2) {
        if (d1.isEmpty() || d2.isEmpty()) {
            return false;
        }
        if (d1.getLowerBound() > d2.getUpperBound()) {
            return false;
        }
        if (d1.getUpperBound() < d2.getLowerBound()) {
            return false;
        }
        if (d1.isBounded() && d2.isBounded()) {
            // Bounds are already checked.
            return true;
        }
        if (d1.size() <= d2.size()) {
            TIntIterator iter = d1.iterator();
            while (iter.hasNext()) {
                if (d2.contains(iter.next())) {
                    return true;
                }
            }
        } else {
            TIntIterator iter = d2.iterator();
            while (iter.hasNext()) {
                if (d1.contains(iter.next())) {
                    return true;
                }
            }
        }
        return false;
    }

    public static boolean isSubsetOf(IrDomain sub, IrDomain sup) {
        if (sub.isEmpty()) {
            return true;
        }
        if (sub.size() > sup.size()) {
            return false;
        }
        if (sub.getLowerBound() < sup.getLowerBound()) {
            return false;
        }
        if (sub.getUpperBound() > sup.getUpperBound()) {
            return false;
        }
        if (sub.isBounded() && sup.isBounded()) {
            // Bounds are already checked.
            return true;
        }
        TIntIterator iter = sub.iterator();
        while (iter.hasNext()) {
            if (!sup.contains(iter.next())) {
                return false;
            }
        }
        return true;
    }

    public static IrDomain union(IrDomain d1, IrDomain d2) {
        if (isSubsetOf(d1, d2)) {
            return d2;
        }
        if (isSubsetOf(d2, d1)) {
            return d1;
        }
        if (d1 instanceof IrBoundDomain && d2 instanceof IrBoundDomain) {
            if (d1.getLowerBound() <= d2.getLowerBound()
                    && d1.getUpperBound() >= d2.getLowerBound()) {
                return Irs.boundDomain(d1.getLowerBound(), d2.getUpperBound());
            }
            if (d2.getLowerBound() <= d1.getLowerBound()
                    && d2.getUpperBound() >= d1.getUpperBound()) {
                return Irs.boundDomain(d2.getLowerBound(), d1.getUpperBound());
            }
        }
        TIntHashSet values = new TIntHashSet();
        values.addAll(d1.getValues());
        values.addAll(d2.getValues());
        return Irs.enumDomain(values);
    }
}
