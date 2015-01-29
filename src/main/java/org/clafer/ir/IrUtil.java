package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.domain.BoolDomain;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author jimmy
 */
public class IrUtil {

    private IrUtil() {
    }

    public static boolean isTrue(IrBoolExpr b) {
        return BoolDomain.TrueDomain.equals(b.getDomain());
    }

    public static boolean isFalse(IrBoolExpr b) {
        return BoolDomain.FalseDomain.equals(b.getDomain());
    }

    public static boolean isConstant(IrBoolExpr b) {
        return !BoolDomain.TrueFalseDomain.equals(b.getDomain());
    }

    public static Boolean getConstant(IrBoolExpr b) {
        switch (b.getDomain()) {
            case TrueDomain:
                return Boolean.TRUE;
            case FalseDomain:
                return Boolean.FALSE;
            case TrueFalseDomain:
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
            case TrueFalseDomain:
                return b;
            default:
                throw new IllegalArgumentException();
        }
    }

    public static boolean isConstant(IrIntExpr i) {
        Domain domain = i.getDomain();
        return domain.size() == 1;
    }

    public static Integer getConstant(IrIntExpr i) {
        Domain domain = i.getDomain();
        return domain.size() == 1 ? domain.getLowBound() : null;
    }

    public static Integer getConstant(IrIntArrayExpr is, int index) {
        Domain domain = is.getDomains()[index];
        return domain.size() == 1 ? domain.getLowBound() : null;
    }

    public static int[] getConstant(IrIntExpr[] is) {
        if (is.length == 0) {
            return new int[0];
        }
        if (is[0].getDomain().size() == 1) {
            int[] constant = new int[is.length];
            constant[0] = is[0].getDomain().getLowBound();
            for (int i = 1; i < is.length; i++) {
                if (is[i].getDomain().size() == 1) {
                    constant[i] = is[i].getDomain().getLowBound();
                } else {
                    return null;
                }
            }
            return constant;
        }
        return null;
    }

    public static IrIntVar asConstant(IrIntVar i) {
        Domain domain = i.getDomain();
        return domain.size() == 1 ? Irs.constant(domain.getLowBound()) : i;
    }

    public static boolean isConstant(IrSetExpr s) {
        Domain env = s.getEnv();
        Domain ker = s.getKer();
        return env.size() == ker.size();
    }

    public static Domain getConstant(IrSetExpr s) {
        Domain env = s.getEnv();
        Domain ker = s.getKer();
        return env.size() == ker.size() ? ker : null;
    }

    public static IrSetVar asConstant(IrSetVar s) {
        Domain env = s.getEnv();
        Domain ker = s.getKer();
        if (env.size() == ker.size()) {
            return Irs.constant(ker);
        }
        Domain card = s.getCard();
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
        Domain env = s.getEnv();
        Domain ker = s.getKer();
        if (env.size() == ker.size()) {
            return Irs.constant(env);
        }
        Domain card = s.getCard();
        if (card.size() == 1) {
            int constantCard = card.getLowBound();
            if (constantCard == ker.size()) {
                return Irs.constant(ker);
            }
        }
        return s;
    }

    public static IrIntExpr asInt(IrSetExpr set) {
        if (set instanceof IrSingleton) {
            return ((IrSingleton) set).getValue();
        }
        Domain constant = getConstant(set);
        if (constant != null && constant.size() == 1) {
            return Irs.constant(constant.getLowBound());
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
        if (da.size() == 1 && db.size() == 1 && da.getLowBound() == db.getLowBound()) {
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
