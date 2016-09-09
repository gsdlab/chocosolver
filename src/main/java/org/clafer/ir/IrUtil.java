package org.clafer.ir;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrUtil {

    private IrUtil() {
    }

    public static Integer getConstant(IrIntArrayExpr is, int index) {
        Domain domain = is.getDomains()[index];
        return domain.isConstant() ? domain.getLowBound() : null;
    }

    public static int[] getConstant(IrIntExpr[] is) {
        if (is.length == 0) {
            return new int[0];
        }
        if (is[0].getDomain().isConstant()) {
            int[] constant = new int[is.length];
            constant[0] = is[0].getDomain().getLowBound();
            for (int i = 1; i < is.length; i++) {
                if (is[i].getDomain().isConstant()) {
                    constant[i] = is[i].getDomain().getLowBound();
                } else {
                    return null;
                }
            }
            return constant;
        }
        return null;
    }

    public static IrIntExpr asConstant(IrIntExpr i) {
        return i.isConstant() ? Irs.constant(i.getLowBound()) : i;
    }

    public static IrSetExpr asConstant(IrSetExpr s) {
        return s.isConstant() ? Irs.constant(s.getKer()) : s;
    }

    public static IrIntExpr asInt(IrSetExpr set) {
        if (set instanceof IrSingleton) {
            return ((IrSingleton) set).getValue();
        }
        if (set.isConstant() && set.getKer().isConstant()) {
            return Irs.constant(set.getKer().getLowBound());
        }
        return null;
    }

    public static IrIntExpr[] asInts(IrSetArrayExpr sets) {
        if (sets.length() == 0) {
            return new IrIntExpr[0];
        }
        if (sets instanceof IrSetArrayVar) {
            return asInts(((IrSetArrayVar) sets).getArray());
        }
        return null;
    }

    public static IrIntExpr[] asInts(IrSetExpr[] sets) {
        if (sets.length == 0) {
            return new IrIntExpr[0];
        }
        IrIntExpr asInt = asInt(sets[0]);
        if (asInt == null) {
            return null;
        }
        IrIntExpr[] ints = new IrIntExpr[sets.length];
        ints[0] = asInt;
        for (int i = 1; i < sets.length; i++) {
            asInt = asInt(sets[i]);
            if (asInt == null) {
                return null;
            }
            ints[i] = asInt;
        }
        return ints;
    }

    public static IrIntExpr[] asArray(IrIntArrayExpr array) {
        // TODO: remove
        if (array instanceof IrIntArrayVar) {
            return ((IrIntArrayVar) array).getArray();
        }
        IrIntExpr[] asArray = new IrIntExpr[array.length()];
        for (int i = 0; i < asArray.length; i++) {
            asArray[i] = Irs.get(array, i);
        }
        return asArray;
    }

    public static IrSetExpr[] asArray(IrSetArrayExpr array) {
        if (array instanceof IrSetArrayVar) {
            return ((IrSetArrayVar) array).getArray();
        }
        IrSetExpr[] asArray = new IrSetExpr[array.length()];
        for (int i = 0; i < asArray.length; i++) {
            asArray[i] = Irs.get(array, i);
        }
        return asArray;
    }

    public static IrStringExpr asConstant(IrStringExpr s) {
        if (s.getLength().size() != 1) {
            return s;
        }
        int length = s.getLength().getLowBound();
        char[] string = new char[length];
        for (int i = 0; i < length; i++) {
            if (s.getChars()[i].size() != 1) {
                return s;
            }
            int chari = s.getChars()[i].getLowBound();
            if (chari < Character.MIN_VALUE
                    || chari > Character.MAX_VALUE
                    || chari == 0) {
                return s;
            }
            string[i] = (char) chari;
        }
        for (int i = length; i < s.getChars().length; i++) {
            if (s.getChars()[i].size() != 1
                    || s.getChars()[i].getLowBound() != 0) {
                return s;
            }
        }
        return Irs.constant(new String(string));
    }

    public static int maxLength(IrStringExpr... strings) {
        int maxLength = 0;
        for (IrStringExpr string : strings) {
            maxLength = Math.max(maxLength, string.getChars().length);
        }
        return maxLength;
    }

    public static Ordering compare(IrIntExpr a, IrIntExpr b) {
        if (a.equals(b)) {
            return Ordering.EQ;
        }
        Domain da = a.getDomain();
        Domain db = b.getDomain();
        if (da.isConstant() && db.isConstant() && da.getLowBound() == db.getLowBound()) {
            return Ordering.EQ;
        }
        int aLb = da.getLowBound();
        int aUb = da.getHighBound();
        int bLb = db.getLowBound();
        int bUb = db.getHighBound();
        if (aLb > bUb) {
            return Ordering.GT;
        }
        if (aLb >= bUb) {
            return Ordering.GE;
        }
        if (aUb < bLb) {
            return Ordering.LT;
        }
        if (aUb <= bLb) {
            return Ordering.LE;
        }
        return Ordering.UNKNOWN;
    }

    public static Ordering compareString(IrIntExpr[] a, IrIntExpr[] b) {
        return compareString(a, b, 0);
    }

    public static Ordering compareString(IrIntExpr[] a, IrIntExpr[] b, int index) {
        if (index == a.length) {
            return a.length == b.length ? Ordering.EQ : Ordering.LT;
        }
        if (index == b.length) {
            assert a.length != b.length;
            return Ordering.GT;
        }
        Ordering ord = compare(a[index], b[index]);
        switch (ord) {
            case EQ:
                return compareString(a, b, index + 1);
            case LE:
                switch (compareString(a, b, index + 1)) {
                    case LT:
                        return Ordering.LT;
                    case LE:
                    case EQ:
                        return Ordering.LE;
                    default:
                        return Ordering.UNKNOWN;
                }
            case GE:
                switch (compareString(a, b, index + 1)) {
                    case GT:
                        return Ordering.GT;
                    case GE:
                    case EQ:
                        return Ordering.GE;
                    default:
                        return Ordering.UNKNOWN;
                }
            default:
                return ord;
        }
    }

    public static IrIntExpr[] pad(IrIntExpr[] chars, int length) {
        if (length < chars.length) {
            throw new IllegalArgumentException();
        }
        if (length == chars.length) {
            return chars;
        }
        IrIntExpr[] pad = Arrays.copyOf(chars, length);
        Arrays.fill(pad, chars.length, pad.length, Irs.Zero);
        return pad;
    }

    public static Set<IrVar> getVariables(IrBoolExpr constraint) {
        Set<IrVar> variables = new HashSet<>();
        constraint.accept(VariableFinder, variables);
        return variables;
    }

    public static IrModule renameVariables(
            IrModule module,
            Map<IrIntVar, IrIntVar> intRename,
            Map<IrSetVar, IrSetVar> setRename,
            Map<IrStringVar, IrStringVar> stringRename) {
        return new VariableRenamer(intRename, setRename, stringRename).rewrite(module, null);
    }

    public static enum Ordering {

        LT,
        LE,
        GT,
        GE,
        EQ,
        UNKNOWN;
    }

    private static final IrTraverser<Set<IrVar>> VariableFinder
            = new IrTraverser<Set<IrVar>>() {

                @Override
                public Void visit(IrBoolVar ir, Set<IrVar> a) {
                    a.add(ir);
                    return super.visit(ir, a);
                }

                @Override
                public Void visit(IrIntVar ir, Set<IrVar> a) {
                    a.add(ir);
                    return super.visit(ir, a);
                }

                @Override
                public Void visit(IrSetVar ir, Set<IrVar> a) {
                    a.add(ir);
                    return super.visit(ir, a);
                }

                @Override
                public Void visit(IrStringVar ir, Set<IrVar> a) {
                    a.add(ir);
                    return super.visit(ir, a);
                }
            };

    private static class VariableRenamer extends IrRewriter<Void> {

        private final Map<IrIntVar, IrIntVar> intRename;
        private final Map<IrSetVar, IrSetVar> setRename;
        private final Map<IrStringVar, IrStringVar> stringRename;

        VariableRenamer(
                Map<IrIntVar, IrIntVar> intRename,
                Map<IrSetVar, IrSetVar> setRename,
                Map<IrStringVar, IrStringVar> stringRename) {
            this.intRename = intRename;
            this.setRename = setRename;
            this.stringRename = stringRename;
        }

        @Override
        public IrBoolVar visit(IrBoolVar ir, Void a) {
            IrBoolVar var = (IrBoolVar) intRename.get(ir);
            return var == null ? ir : var;
        }

        @Override
        public IrIntVar visit(IrIntVar ir, Void a) {
            IrIntVar var = intRename.get(ir);
            return var == null ? ir : var;
        }

        @Override
        public IrSetVar visit(IrSetVar ir, Void a) {
            IrSetVar var = setRename.get(ir);
            return var == null ? ir : var;
        }

        @Override
        public IrStringVar visit(IrStringVar ir, Void a) {
            IrStringVar var = stringRename.get(ir);
            return var == null ? ir : var;
        }
    };
}
