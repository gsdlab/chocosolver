package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.ir.IrDomain.IrBoundDomain;

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

    public static boolean isTrue(IrConstraint c) {
        if (c instanceof IrBoolConstraint) {
            IrBoolConstraint boolConstraint = (IrBoolConstraint) c;
            return isTrue(boolConstraint.getExpr());
        }
        return false;
    }

    public static boolean isFalse(IrConstraint c) {
        if (c instanceof IrBoolConstraint) {
            IrBoolConstraint boolConstraint = (IrBoolConstraint) c;
            return isFalse(boolConstraint.getExpr());
        }
        return false;
    }

    public static boolean isTrue(IrBool b) {
        return IrBoolDomain.TrueDomain.equals(b.getDomain());
    }

    public static boolean isFalse(IrBool b) {
        return IrBoolDomain.FalseDomain.equals(b.getDomain());
    }

    public static Boolean isConstant(IrBool b) {
        return !IrBoolDomain.BoolDomain.equals(b.getDomain());
    }

    public static Boolean getConstant(IrBool b) {
        switch (b.getDomain()) {
            case TrueDomain:
                return Boolean.TRUE;
            case FalseDomain:
                return Boolean.FALSE;
            case BoolDomain:
                return null;
            default:
                throw new IllegalArgumentException();
        }
    }

    public static IrBoolVar asConstant(IrBoolVar b) {
        switch (b.getDomain()) {
            case TrueDomain:
                return Irs.True;
            case FalseDomain:
                return Irs.False;
            case BoolDomain:
                return b;
            default:
                throw new IllegalArgumentException();
        }
    }

    public static boolean isConstant(IrInt i) {
        IrDomain domain = i.getDomain();
        return domain.size() == 1;
    }

    public static Integer getConstant(IrInt i) {
        IrDomain domain = i.getDomain();
        return domain.size() == 1 ? domain.getLowerBound() : null;
    }

    public static IrIntVar asConstant(IrIntVar i) {
        IrDomain domain = i.getDomain();
        return domain.size() == 1 ? Irs.constant(domain.getLowerBound()) : i;
    }

    public static boolean isConstant(IrSet s) {
        IrDomain env = s.getEnv();
        IrDomain ker = s.getKer();
        return env.equals(ker);
    }

    public static int[] getConstant(IrSet s) {
        IrDomain env = s.getEnv();
        IrDomain ker = s.getKer();
        return env.equals(ker) ? ker.getValues() : null;
    }

    public static IrSetVar asConstant(IrSetVar s) {
        IrDomain env = s.getEnv();
        IrDomain ker = s.getKer();
        return env.equals(ker) ? Irs.constant(ker.getValues()) : s;
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

    public static IrDomain unionEnvs(IrSetExpr... sets) {
        if (sets.length == 0) {
            return Irs.EmptyDomain;
        }
        IrDomain domain = sets[0].getEnv();
        for (int i = 1; i < sets.length; i++) {
            domain = union(domain, sets[i].getEnv());
        }
        return domain;
    }

    public static IrDomain unionKers(IrSetExpr... sets) {
        if (sets.length == 0) {
            return Irs.EmptyDomain;
        }
        IrDomain domain = sets[0].getKer();
        for (int i = 1; i < sets.length; i++) {
            domain = union(domain, sets[i].getKer());
        }
        return domain;
    }
}
