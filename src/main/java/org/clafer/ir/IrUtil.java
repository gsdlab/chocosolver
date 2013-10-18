package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import gnu.trove.set.hash.TIntHashSet;
import java.util.ArrayList;
import java.util.List;
import org.clafer.collection.Pair;

/**
 *
 * @author jimmy
 */
public class IrUtil {

    private IrUtil() {
    }

    public static <T> T notNull(String message, T t) {
        if (t == null) {
            throw new IrException(message);
        }
        return t;
    }

    public static boolean isTrue(IrBoolExpr b) {
        return IrBoolDomain.TrueDomain.equals(b.getDomain());
    }

    public static boolean isFalse(IrBoolExpr b) {
        return IrBoolDomain.FalseDomain.equals(b.getDomain());
    }

    public static Boolean isConstant(IrBoolExpr b) {
        return !IrBoolDomain.BoolDomain.equals(b.getDomain());
    }

    public static Boolean getConstant(IrBoolExpr b) {
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

    public static boolean isConstant(IrIntExpr i) {
        IrDomain domain = i.getDomain();
        return domain.size() == 1;
    }

    public static Integer getConstant(IrIntExpr i) {
        IrDomain domain = i.getDomain();
        return domain.size() == 1 ? domain.getLowBound() : null;
    }

    public static IrIntVar asConstant(IrIntVar i) {
        IrDomain domain = i.getDomain();
        return domain.size() == 1 ? Irs.constant(domain.getLowBound()) : i;
    }

    public static boolean isConstant(IrSetExpr s) {
        IrDomain env = s.getEnv();
        IrDomain ker = s.getKer();
        assert IrUtil.isSubsetOf(ker, env);
        return env.size() == ker.size();
    }

    public static int[] getConstant(IrSetExpr s) {
        IrDomain env = s.getEnv();
        IrDomain ker = s.getKer();
        assert IrUtil.isSubsetOf(ker, env);
        return env.size() == ker.size() ? ker.getValues() : null;
    }

    public static IrSetVar asConstant(IrSetVar s) {
        IrDomain env = s.getEnv();
        IrDomain ker = s.getKer();
        assert IrUtil.isSubsetOf(ker, env);
        if (env.size() == ker.size()) {
            return Irs.constant(ker);
        }
        IrDomain card = s.getCard();
        if (card.size() == 1) {
            int constantCard = card.getLowBound();
            if (constantCard == ker.size()) {
                return Irs.constant(ker);
            }
            if (constantCard == env.size()) {
                return Irs.constant(env);
            }
        }
        return s;
    }

    public static IrSetExpr asConstant(IrSetExpr s) {
        IrDomain env = s.getEnv();
        IrDomain ker = s.getKer();
        assert IrUtil.isSubsetOf(ker, env);
        if (env.size() == ker.size()) {
            return Irs.constant(env);
        }
        IrDomain card = s.getCard();
        if (card.size() == 1) {
            int constantCard = card.getLowBound();
            if (constantCard == ker.size()) {
                return Irs.constant(ker);
            }
        }
        return s;
    }

    public static boolean containsAll(int[] values, IrDomain domain) {
        for (int value : values) {
            if (!domain.contains(value)) {
                return false;
            }
        }
        return true;
    }

    public static boolean intersects(IrDomain d1, IrDomain d2) {
        if (d1.isEmpty() || d2.isEmpty()) {
            return false;
        }
        if (d1.getLowBound() > d2.getHighBound()) {
            return false;
        }
        if (d1.getHighBound() < d2.getLowBound()) {
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
        if (sub.getLowBound() < sup.getLowBound()) {
            return false;
        }
        if (sub.getHighBound() > sup.getHighBound()) {
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

    public static IrDomain difference(IrDomain minuend, IrDomain subtrahend) {
        if (!intersects(minuend, subtrahend)) {
            return minuend;
        }
        if (minuend.isBounded() && subtrahend.isBounded()) {
            if (minuend.getLowBound() < subtrahend.getLowBound()
                    && minuend.getHighBound() <= subtrahend.getHighBound()) {
                return Irs.boundDomain(minuend.getLowBound(), subtrahend.getLowBound() - 1);
            }
            if (minuend.getHighBound() > subtrahend.getHighBound()
                    && minuend.getLowBound() >= subtrahend.getLowBound()) {
                return Irs.boundDomain(subtrahend.getHighBound() + 1, minuend.getHighBound());
            }
        }
        TIntHashSet values = new TIntHashSet();
        values.addAll(minuend.getValues());
        values.removeAll(subtrahend.getValues());
        return Irs.enumDomain(values);
    }

    public static IrDomain intersection(IrDomain d1, IrDomain d2) {
        if (isSubsetOf(d1, d2)) {
            return d1;
        }
        if (isSubsetOf(d2, d1)) {
            return d2;
        }
        if (d1.isBounded() && d2.isBounded()) {
            if (d1.getLowBound() <= d2.getLowBound()
                    && d1.getHighBound() >= d2.getLowBound()) {
                return Irs.boundDomain(d2.getLowBound(), d1.getHighBound());
            }
            if (d2.getLowBound() <= d1.getLowBound()
                    && d2.getHighBound() >= d1.getLowBound()) {
                return Irs.boundDomain(d1.getLowBound(), d2.getHighBound());
            }
        }
        TIntHashSet values = new TIntHashSet();
        values.addAll(d1.getValues());
        values.retainAll(d2.getValues());
        return Irs.enumDomain(values);
    }

    public static IrDomain intersectionEnvs(IrSetExpr... sets) {
        if (sets.length == 0) {
            return Irs.EmptyDomain;
        }
        IrDomain domain = sets[0].getEnv();
        for (int i = 1; i < sets.length; i++) {
            domain = intersection(domain, sets[i].getEnv());
        }
        return domain;
    }

    public static IrDomain intersectionKers(IrSetExpr... sets) {
        if (sets.length == 0) {
            return Irs.EmptyDomain;
        }
        IrDomain domain = sets[0].getKer();
        for (int i = 1; i < sets.length; i++) {
            domain = intersection(domain, sets[i].getKer());
        }
        return domain;
    }

    public static IrDomain union(IrDomain d1, IrDomain d2) {
        if (isSubsetOf(d1, d2)) {
            return d2;
        }
        if (isSubsetOf(d2, d1)) {
            return d1;
        }
        if (d1.isBounded() && d2.isBounded()) {
            if (d1.getLowBound() <= d2.getLowBound()
                    && d1.getHighBound() >= d2.getLowBound()) {
                return Irs.boundDomain(d1.getLowBound(), d2.getHighBound());
            }
            if (d2.getLowBound() <= d1.getLowBound()
                    && d2.getHighBound() >= d1.getLowBound()) {
                return Irs.boundDomain(d2.getLowBound(), d1.getHighBound());
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

    public static IrDomain minus(IrDomain domain) {
        if (domain.isEmpty()) {
            return Irs.EmptyDomain;
        }
        if (domain.isBounded()) {
            return Irs.boundDomain(-domain.getHighBound(), -domain.getLowBound());
        }
        int[] values = domain.getValues();
        int[] minusValues = new int[values.length];
        for (int i = 0; i < minusValues.length; i++) {
            minusValues[i] = -values[i];
        }
        return Irs.enumDomain(minusValues);
    }

    public static IrDomain offset(IrDomain domain, int offset) {
        if (domain.isEmpty()) {
            return Irs.EmptyDomain;
        }
        if (domain.isBounded()) {
            return Irs.boundDomain(domain.getLowBound() + offset, domain.getHighBound() + offset);
        }
        int[] values = domain.getValues();
        int[] offsetValues = new int[values.length];
        for (int i = 0; i < offsetValues.length; i++) {
            offsetValues[i] = values[i] + offset;
        }
        return Irs.enumDomain(offsetValues);
    }

    public static IrDomain mask(IrDomain domain, int from, int to) {
        if (from > to) {
            throw new IllegalArgumentException();
        }
        if (to > domain.getHighBound() + 1) {
            throw new IllegalArgumentException();
        }
        if (from == to || domain.isEmpty()) {
            return Irs.EmptyDomain;
        }
        if (domain.isBounded()) {
            return Irs.boundDomain(0, to - from - 1);
        }
        int[] values = domain.getValues();
        TIntList mask = new TIntArrayList();
        for (int i = 0; i < values.length; i++) {
            if (i >= from && i < to) {
                mask.add(i - from);
            }
        }
        return Irs.enumDomain(mask);
    }

    public static Pair<List<IrNop>, List<IrBoolExpr>> partitionNops(List<IrBoolExpr> constraints) {
        List<IrNop> nops = new ArrayList<IrNop>();
        List<IrBoolExpr> nonNops = new ArrayList<IrBoolExpr>(constraints.size());

        for (IrBoolExpr constraint : constraints) {
            if (constraint instanceof IrNop) {
                nops.add((IrNop) constraint);
            } else {
                nonNops.add(constraint);
            }
        }
        return new Pair<List<IrNop>, List<IrBoolExpr>>(nops, nonNops);
    }
}
