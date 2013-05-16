package org.clafer.ir;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import org.clafer.Util;
import org.clafer.ir.IrDomain.IrBoundDomain;
import org.clafer.ir.IrDomain.IrEmptyDomain;
import org.clafer.ir.IrDomain.IrEnumDomain;

/**
 * 
 * @author jimmy
 */
public class Irs {

    /**
     * @param var the variable to access
     * @return the variable's boolean value
     */
    public static IrBoolExpr $(IrBoolVar var) {
        return new IrBoolLiteral(var, var.getDomain());
    }

    /**
     * @param vars the variables to access
     * @return the variables' boolean values
     */
    public static IrBoolExpr[] $(IrBoolVar... vars) {
        IrBoolExpr[] exprs = new IrBoolExpr[vars.length];
        for (int i = 0; i < exprs.length; i++) {
            exprs[i] = $(vars[i]);
        }
        return exprs;
    }

    /**
     * @param var the variable to access
     * @return the variable's integer value
     */
    public static IrIntExpr $(IrIntVar var) {
        return new IrIntLiteral(var, var.getDomain());
    }

    /**
     * @param vars the variables to access
     * @return the variables' integer values
     */
    public static IrIntExpr[] $(IrIntVar... vars) {
        IrIntExpr[] exprs = new IrIntExpr[vars.length];
        for (int i = 0; i < exprs.length; i++) {
            exprs[i] = $(vars[i]);
        }
        return exprs;
    }

    /**
     * @param var the variable to access
     * @return the variable's set value
     */
    public static IrSetExpr $(IrSetVar var) {
        return new IrSetLiteral(var, var.getEnv(), var.getKer(), var.getCard());
    }

    /**
     * @param var the variables to access
     * @return the variables' set values
     */
    public static IrSetExpr[] $(IrSetVar... var) {
        IrSetExpr[] exprs = new IrSetExpr[var.length];
        for (int i = 0; i < exprs.length; i++) {
            exprs[i] = $(var[i]);
        }
        return exprs;
    }
    /********************
     * 
     * Boolean
     * 
     ********************/
    public static final IrBoolDomain TrueDomain = IrBoolDomain.TrueDomain;
    public static final IrBoolDomain FalseDomain = IrBoolDomain.FalseDomain;
    public static final IrBoolDomain BoolDomain = IrBoolDomain.BoolDomain;
    public static final IrBoolVar True = new IrBoolConstant(true);
    public static final IrBoolVar False = new IrBoolConstant(false);

    public static IrBoolDomain domain(boolean value) {
        return value ? TrueDomain : FalseDomain;
    }

    public static IrBoolVar constant(boolean value) {
        return value ? True : False;
    }

    public static IrBoolVar bool(String name) {
        return new IrBoolVar(name, BoolDomain);
    }

    public static IrBoolExpr not(IrBoolExpr proposition) {
        Boolean constant = IrUtil.getConstant(proposition);
        if (constant != null) {
            // Reverse the boolean
            return constant.booleanValue() ? $(False) : $(True);
        }
        return proposition.negate();
    }

    public static IrBoolExpr and(IrBoolExpr... operands) {
        List<IrBoolExpr> filter = new ArrayList<IrBoolExpr>(operands.length);
        for (IrBoolExpr operand : operands) {
            if (IrUtil.isFalse(operand)) {
                return $(False);
            }
            if (!IrUtil.isTrue(operand)) {
                filter.add(operand);
            }
        }
        switch (filter.size()) {
            case 0:
                return $(True);
            case 1:
                return filter.get(0);
            default:
                return new IrAnd(filter.toArray(new IrBoolExpr[filter.size()]), BoolDomain);
        }
    }

    public static IrBoolExpr or(IrBoolExpr... operands) {
        List<IrBoolExpr> filter = new ArrayList<IrBoolExpr>(operands.length);
        for (IrBoolExpr operand : operands) {
            if (IrUtil.isTrue(operand)) {
                return $(True);
            }
            if (!IrUtil.isFalse(operand)) {
                filter.add(operand);
            }
        }
        switch (filter.size()) {
            case 0:
                return $(False);
            case 1:
                return filter.get(0);
            default:
                return new IrOr(filter.toArray(new IrBoolExpr[filter.size()]), BoolDomain);
        }
    }

    public static IrBoolExpr implies(IrBoolExpr antecedent, IrBoolExpr consequent) {
        if (IrUtil.isTrue(antecedent)) {
            return consequent;
        }
        if (IrUtil.isFalse(antecedent)) {
            return $(True);
        }
        if (IrUtil.isTrue(consequent)) {
            return $(True);
        }
        if (IrUtil.isFalse(consequent)) {
            return not(antecedent);
        }
        return new IrImplies(antecedent, consequent, BoolDomain);
    }

    public static IrConstraint implies(IrBoolExpr antecedent, IrConstraint consequent) {
        if (IrUtil.isTrue(antecedent)) {
            return consequent;
        }
        if (IrUtil.isFalse(antecedent)) {
            return Tautalogy;
        }
        if (IrUtil.isTrue(consequent)) {
            return Tautalogy;
        }
        if (IrUtil.isFalse(consequent)) {
            return boolConstraint(not(antecedent));
        }
        return new IrHalfReification(antecedent, consequent);
    }

    public static IrBoolExpr ifOnlyIf(IrBoolExpr left, IrBoolExpr right) {
        if (IrUtil.isTrue(left)) {
            return right;
        }
        if (IrUtil.isFalse(left)) {
            return not(right);
        }
        if (IrUtil.isTrue(right)) {
            return left;
        }
        if (IrUtil.isFalse(right)) {
            return not(left);
        }
        return new IrIfOnlyIf(left, right, BoolDomain);
    }

    public static IrBoolExpr between(IrIntExpr var, int low, int high) {
        IrDomain domain = var.getDomain();
        if (domain.getLowerBound() >= low && domain.getUpperBound() <= high) {
            return $(True);
        }
        if (domain.getLowerBound() > high || domain.getUpperBound() < low) {
            return $(False);
        }
        return new IrBetween(var, low, high, BoolDomain);
    }

    public static IrBoolExpr notBetween(IrIntExpr var, int low, int high) {
        IrDomain domain = var.getDomain();
        if (domain.getLowerBound() >= low && domain.getUpperBound() <= high) {
            return $(False);
        }
        if (domain.getLowerBound() > high || domain.getUpperBound() < low) {
            return $(True);
        }
        return new IrNotBetween(var, low, high, BoolDomain);
    }

    public static IrBoolExpr compare(IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
        IrDomain leftDomain = left.getDomain();
        IrDomain rightDomain = right.getDomain();
        switch (op) {
            case Equal:
                if (leftDomain.size() == 1 && rightDomain.size() == 1) {
                    return $(constant(leftDomain.getLowerBound() == rightDomain.getLowerBound()));
                }
                break;
            case NotEqual:
                if (leftDomain.size() == 1 && rightDomain.size() == 1) {
                    return $(constant(leftDomain.getLowerBound() != rightDomain.getLowerBound()));
                }
                break;
            case LessThan:
                if (leftDomain.getUpperBound() < rightDomain.getLowerBound()) {
                    return $(True);
                }
                if (leftDomain.getLowerBound() >= rightDomain.getUpperBound()) {
                    return $(False);
                }
                break;
            case LessThanEqual:
                if (leftDomain.getUpperBound() <= rightDomain.getLowerBound()) {
                    return $(True);
                }
                if (leftDomain.getLowerBound() > rightDomain.getUpperBound()) {
                    return $(False);
                }
                break;
            case GreaterThan:
                if (leftDomain.getLowerBound() > rightDomain.getUpperBound()) {
                    return $(True);
                }
                if (leftDomain.getUpperBound() <= rightDomain.getLowerBound()) {
                    return $(False);
                }
                break;
            case GreaterThanEqual:
                if (leftDomain.getLowerBound() >= rightDomain.getUpperBound()) {
                    return $(True);
                }
                if (leftDomain.getUpperBound() < rightDomain.getLowerBound()) {
                    return $(False);
                }
                break;
            default:
                throw new IllegalArgumentException("Unknown op: " + op);
        }
        return new IrCompare(left, op, right, BoolDomain);
    }

    public static IrBoolExpr equal(IrIntExpr left, int right) {
        return equal(left, $(constant(right)));
    }

    public static IrBoolExpr equal(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.Equal, right);
    }

    public static IrBoolExpr equality(IrSetExpr left, IrSetEquality.Op op, IrSetExpr right) {
        switch (op) {
            case Equal:
                if (left.equals(right)) {
                    return $(True);
                }
                break;
            case NotEqual:
                if (left.equals(right)) {
                    return $(False);
                }
                break;
            default:
                throw new IllegalArgumentException();
        }
        return new IrSetEquality(left, op, right, BoolDomain);
    }

    public static IrBoolExpr equal(IrSetExpr left, IrSetExpr right) {
        return equality(left, IrSetEquality.Op.Equal, right);
    }

    public static IrBoolExpr notEqual(IrIntExpr left, int right) {
        return notEqual(left, $(constant(right)));
    }

    public static IrBoolExpr notEqual(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.NotEqual, right);
    }

    public static IrBoolExpr notEqual(IrSetExpr left, IrSetExpr right) {
        return equality(left, IrSetEquality.Op.NotEqual, right);
    }

    public static IrBoolExpr lessThan(IrIntExpr left, int right) {
        return lessThan(left, $(constant(right)));
    }

    public static IrBoolExpr lessThan(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.LessThan, right);
    }

    public static IrBoolExpr lessThanEqual(IrIntExpr left, int right) {
        return lessThanEqual(left, $(constant(right)));
    }

    public static IrBoolExpr lessThanEqual(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.LessThanEqual, right);
    }

    public static IrBoolExpr greaterThan(IrIntExpr left, int right) {
        return greaterThan(left, $(constant(right)));
    }

    public static IrBoolExpr greaterThan(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.GreaterThan, right);
    }

    public static IrBoolExpr greaterThanEqual(IrIntExpr left, int right) {
        return greaterThanEqual(left, $(constant(right)));
    }

    public static IrBoolExpr greaterThanEqual(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.GreaterThanEqual, right);
    }

    public static IrBoolExpr member(IrIntExpr element, IrSetExpr set) {
        if (IrUtil.isSubsetOf(element.getDomain(), set.getKer())) {
            return $(True);
        }
        if (!IrUtil.intersects(element.getDomain(), set.getEnv())) {
            return $(False);
        }
        return new IrMember(element, set, BoolDomain);
    }

    public static IrBoolExpr notMember(IrIntExpr element, IrSetExpr set) {
        if (!IrUtil.intersects(element.getDomain(), set.getEnv())) {
            return $(True);
        }
        if (IrUtil.isSubsetOf(element.getDomain(), set.getKer())) {
            return $(False);
        }
        return new IrNotMember(element, set, BoolDomain);
    }
    /********************
     * 
     * Integers
     * 
     ********************/
    public static IrIntVar Zero = new IrIntConstant(0);
    public static IrIntVar One = new IrIntConstant(1);

    public static IrIntVar boundInt(String name, int low, int high) {
        if (low == high) {
            return constant(low);
        }
        return new IrIntVar(name, boundDomain(low, high));
    }

    public static IrIntVar enumInt(String name, int[] values) {
        if (values.length == 1) {
            return constant(values[0]);
        }
        return new IrIntVar(name, enumDomain(values));
    }

    public static IrIntVar constant(int value) {
        switch (value) {
            case 0:
                return Zero;
            case 1:
                return One;
            default:
                return new IrIntConstant(value);
        }
    }

    public static IrIntExpr card(IrSetExpr set) {
        IrDomain domain = set.getCard();
        if (domain.size() == 1) {
            return $(constant(domain.getLowerBound()));
        }
        return new IrCard(set, domain);
    }

    public static IrIntExpr arithm(IrArithm.Op op, IrIntExpr... operands) {
        int constants;
        Deque<IrIntExpr> filter = new LinkedList<IrIntExpr>();
        IrDomain domain = operands[0].getDomain();
        int low = domain.getLowerBound();
        int high = domain.getUpperBound();
        switch (op) {
            case Add:
                constants = 0;
                for (IrIntExpr operand : operands) {
                    Integer constant = IrUtil.getConstant(operand);
                    if (constant != null) {
                        constants += constant.intValue();
                    } else {
                        filter.add(operand);
                    }
                }
                if (constants != 0) {
                    filter.add($(constant(constants)));
                }
                if (filter.isEmpty()) {
                    return $(Zero);
                }
                if (filter.size() == 1) {
                    return filter.getFirst();
                }
                for (int i = 1; i < operands.length; i++) {
                    domain = operands[i].getDomain();
                    low += domain.getLowerBound();
                    high += domain.getUpperBound();
                }
                domain = boundDomain(low, high);
                break;
            case Sub:
                constants = 0;
                for (int i = 1; i < operands.length; i++) {
                    IrIntExpr operand = operands[i];
                    Integer constant = IrUtil.getConstant(operand);
                    if (constant != null) {
                        constants += constant.intValue();
                    } else {
                        filter.add(operand);
                    }
                }
                Integer head = IrUtil.getConstant(operands[0]);
                if (head != null && filter.isEmpty()) {
                    return $(constant(head - constants));
                }
                filter.addFirst(operands[0]);
                if (constants != 0) {
                    filter.add($(constant(constants)));
                }
                if (filter.size() == 1) {
                    return filter.getFirst();
                }
                for (int i = 1; i < operands.length; i++) {
                    domain = operands[i].getDomain();
                    low -= domain.getUpperBound();
                    high -= domain.getLowerBound();
                }
                domain = boundDomain(low, high);
                break;
            case Mul:
                constants = 1;
                for (IrIntExpr operand : operands) {
                    Integer constant = IrUtil.getConstant(operand);
                    if (constant != null) {
                        constants *= constant.intValue();
                    } else {
                        filter.add(operand);
                    }
                }
                if (constants == 0) {
                    return $(Zero);
                }
                if (constants != 1) {
                    filter.add($(constant(constants)));
                }
                if (filter.isEmpty()) {
                    return $(One);
                }
                if (filter.size() == 1) {
                    return filter.getFirst();
                }
                for (int i = 1; i < operands.length; i++) {
                    domain = operands[i].getDomain();
                    low = Util.min(low * domain.getLowerBound(), low * domain.getUpperBound(),
                            high * domain.getLowerBound(), high * domain.getUpperBound());
                    high = Util.max(low * domain.getLowerBound(), low * domain.getUpperBound(),
                            high * domain.getLowerBound(), high * domain.getUpperBound());
                }
                domain = boundDomain(low, high);
                break;
            case Div:
                // TODO: optimize
                filter.addAll(Arrays.asList(operands));
                if (filter.size() == 1) {
                    return filter.getFirst();
                }
                for (int i = 1; i < operands.length; i++) {
                    domain = operands[i].getDomain();
                    low = Util.min(low / domain.getLowerBound(), low / domain.getUpperBound(),
                            high * domain.getLowerBound(), high * domain.getUpperBound());
                    high = Util.max(low / domain.getLowerBound(), low / domain.getUpperBound(),
                            high / domain.getLowerBound(), high / domain.getUpperBound());
                }
                domain = boundDomain(low, high);
            default:
                throw new IllegalArgumentException();
        }
        return new IrArithm(op, filter.toArray(new IrIntExpr[filter.size()]), domain);
    }

    public static IrIntExpr add(IrIntExpr left, int right) {
        return add(left, $(constant(right)));
    }

    public static IrIntExpr add(IrIntExpr... operands) {
        return arithm(IrArithm.Op.Add, operands);
    }

    public static IrIntExpr sub(IrIntExpr left, int right) {
        return sub(left, $(constant(right)));
    }

    public static IrIntExpr sub(IrIntExpr... operands) {
        return arithm(IrArithm.Op.Sub, operands);
    }

    public static IrIntExpr mul(IrIntExpr left, int right) {
        return mul(left, $(constant(right)));
    }

    public static IrIntExpr mul(IrIntExpr... operands) {
        return arithm(IrArithm.Op.Mul, operands);
    }

    public static IrIntExpr div(IrIntExpr left, int right) {
        return div(left, $(constant(right)));
    }

    public static IrIntExpr div(IrIntExpr... operands) {
        return arithm(IrArithm.Op.Div, operands);
    }

    public static IrIntExpr element(IrIntExpr[] array, IrIntExpr index) {
        Integer constant = IrUtil.getConstant(index);
        if (constant != null) {
            return array[constant.intValue()];
        }
        TIntIterator iter = index.getDomain().iterator();
        assert iter.hasNext();

        IrDomain domain = array[iter.next()].getDomain();
        int low = domain.getLowerBound();
        int high = domain.getUpperBound();
        while (iter.hasNext()) {
            domain = array[iter.next()].getDomain();
            low = Math.min(low, domain.getLowerBound());
            high = Math.max(high, domain.getUpperBound());
        }
        domain = boundDomain(low, high);
        return new IrElement(array, index, domain);
    }
    /********************
     * 
     * Set
     * 
     ********************/
    public static final IrDomain EmptyDomain = new IrEmptyDomain();
    public static final IrDomain ZeroDomain = new IrEnumDomain(new int[]{0});
    public static final IrDomain OneDomain = new IrEnumDomain(new int[]{1});
    public static final IrSetVar EmptySet = new IrSetConstant(EmptyDomain);

    public static IrDomain boundDomain(int low, int high) {
        return new IrBoundDomain(low, high);
    }

    public static IrDomain enumDomain(int... values) {
        if (values.length == 1) {
            if (values[0] == 0) {
                return ZeroDomain;
            }
            if (values[0] == 1) {
                return OneDomain;
            }
        }
        return enumDomain(new TIntHashSet(values));
    }

    public static IrDomain enumDomain(TIntCollection values) {
        return enumDomain(new TIntHashSet(values));
    }

    // TODO: collect enum over an interval into a bound domain
    public static IrDomain enumDomain(TIntHashSet values) {
        switch (values.size()) {
            case 0:
                return EmptyDomain;
            case 1:
                int value = values.iterator().next();
                return boundDomain(value, value);
            default:
                int[] array = values.toArray();
                Arrays.sort(array);
                return new IrEnumDomain(array);
        }
    }

    public static IrSetVar set(String name, int lowEnv, int highEnv) {
        return set(name, boundDomain(lowEnv, highEnv), EmptyDomain);
    }

    public static IrSetVar set(String name, int lowEnv, int highEnv, int lowKer, int highKer) {
        return set(name, boundDomain(lowEnv, highEnv), boundDomain(lowKer, highKer));
    }

    public static IrSetVar set(String name, int lowEnv, int highEnv, int[] ker) {
        return set(name, boundDomain(lowEnv, highEnv), enumDomain(ker));
    }

    public static IrSetVar set(String name, int[] env) {
        return set(name, enumDomain(env), EmptyDomain);
    }

    public static IrSetVar set(String name, int[] env, int lowKer, int highKer) {
        return set(name, enumDomain(env), boundDomain(lowKer, highKer));
    }

    public static IrSetVar set(String name, int[] env, int[] ker) {
        return set(name, enumDomain(env), enumDomain(ker));
    }

    public static IrSetVar set(String name, IrDomain env, IrDomain ker) {
        return set(name, env, ker, boundDomain(ker.size(), env.size()));
    }

    public static IrSetVar set(String name, IrDomain env, IrDomain ker, IrDomain card) {
        if (env.equals(ker)) {
            return constant(ker);
        }
        return new IrSetVar(name, env, ker, card);
    }

    public static IrSetVar constant(int[] value) {
        return constant(enumDomain(value));
    }

    public static IrSetVar constant(IrDomain value) {
        return new IrSetConstant(value);
    }

    public static IrSetExpr singleton(IrIntExpr value) {
        Integer constant = IrUtil.getConstant(value);
        if (constant != null) {
            return $(constant(new int[]{constant.intValue()}));
        }
        return new IrSingleton(value, value.getDomain(), EmptyDomain);
    }

    public static IrSetExpr arrayToSet(IrIntExpr[] array) {
        switch (array.length) {
            case 0:
                return $(EmptySet);
            case 1:
                return singleton(array[0]);
            default:
                IrDomain env = array[0].getDomain();
                for (int i = 1; i < array.length; i++) {
                    env = IrUtil.union(env, array[i].getDomain());
                }
                TIntSet values = new TIntHashSet();
                for (IrIntExpr i : array) {
                    Integer constant = IrUtil.getConstant(i);
                    if (constant != null) {
                        values.add(constant.intValue());
                    }
                }
                IrDomain ker = Irs.enumDomain(values);
                // TODO: every set expr function should do this
                if (env.size() == ker.size()) {
                    return $(constant(env));
                }
                IrDomain card = Irs.boundDomain(1, Math.min(array.length, env.size()));
                return new IrArrayToSet(array, env, ker, card);
        }
    }

    public static IrSetExpr join(IrSetExpr take, IrSetExpr[] children) {
        int[] constant = IrUtil.getConstant(take);
        if (constant != null) {
            IrSetExpr[] to = new IrSetExpr[constant.length];
            for (int i = 0; i < to.length; i++) {
                to[i] = children[constant[i]];
            }
            return union(to);
        }

        // Compute env
        IrDomain env = IrUtil.unionEnvs(children);

        // Compute ker
        TIntIterator iter = take.getKer().iterator();
        IrDomain ker;
        if (iter.hasNext()) {
            IrDomain domain = children[iter.next()].getEnv();
            while (iter.hasNext()) {
                domain = IrUtil.union(domain, children[iter.next()].getEnv());
            }
            ker = domain;
        } else {
            ker = Irs.EmptyDomain;
        }

        // Compute card
        IrDomain takeEnv = take.getEnv();
        IrDomain takeKer = take.getKer();
        IrDomain takeCard = take.getCard();
        int index = 0;
        int[] childrenLowCards = new int[takeEnv.size() - takeKer.size()];
        int[] childrenHighCards = new int[takeEnv.size() - takeKer.size()];
        int cardLow = 0, cardHigh = 0;

        iter = takeEnv.iterator();
        while (iter.hasNext()) {
            int val = iter.next();
            IrDomain childDomain = children[val].getCard();
            if (takeKer.contains(val)) {
                cardLow += childDomain.getLowerBound();
                cardHigh += childDomain.getUpperBound();
            } else {
                childrenLowCards[index] = childDomain.getLowerBound();
                childrenHighCards[index] = childDomain.getUpperBound();
                index++;
            }
        }
        assert index == childrenLowCards.length;
        assert index == childrenHighCards.length;

        Arrays.sort(childrenLowCards);
        Arrays.sort(childrenHighCards);

        for (int i = 0; i < takeCard.getLowerBound() - takeKer.size(); i++) {
            cardLow += childrenLowCards[i];
        }
        for (int i = 0; i < takeCard.getUpperBound() - takeKer.size(); i++) {
            cardHigh += childrenHighCards[childrenHighCards.length - 1 - i];
        }
        cardLow = Math.max(cardLow, ker.size());
        cardHigh = Math.min(cardHigh, env.size());
        IrDomain card = boundDomain(cardLow, cardHigh);

        return new IrJoin(take, children, env, ker, card);
    }

    public static IrSetExpr joinRef(IrSetExpr take, IrIntExpr[] refs) {
        int[] constant = IrUtil.getConstant(take);
        if (constant != null) {
            IrIntExpr[] to = new IrIntExpr[constant.length];
            for (int i = 0; i < to.length; i++) {
                to[i] = refs[constant[i]];
            }
            return arrayToSet(to);
        }

        // Compute env
        TIntIterator iter = take.getEnv().iterator();
        IrDomain env;
        if (iter.hasNext()) {
            IrDomain domain = refs[iter.next()].getDomain();
            while (iter.hasNext()) {
                domain = IrUtil.union(domain, refs[iter.next()].getDomain());
            }
            env = domain;
        } else {
            env = Irs.EmptyDomain;
        }

        // Compute ker
        iter = take.getKer().iterator();
        TIntHashSet values = new TIntHashSet(0);
        while (iter.hasNext()) {
            Integer constantRef = IrUtil.getConstant(refs[iter.next()]);
            if (constantRef != null) {
                values.add(constantRef.intValue());
            }
        }
        IrDomain ker = values.isEmpty() ? Irs.EmptyDomain : new IrEnumDomain(values.toArray());

        // Compute card
        IrDomain takeCard = take.getCard();
        int lowTakeCard = takeCard.getLowerBound();
        int highTakeCard = takeCard.getUpperBound();
        IrDomain card = lowTakeCard == 0
                ? boundDomain(Math.max(0, ker.size()), Math.min(highTakeCard, env.size()))
                : boundDomain(Math.max(1, ker.size()), Math.min(highTakeCard, env.size()));

        return new IrJoinRef(take, refs, env, ker, card);
    }

    public static IrSetExpr union(IrSetExpr... operands) {
        switch (operands.length) {
            case 0:
                return $(EmptySet);
            case 1:
                return operands[0];
            default:
                IrDomain env = IrUtil.unionEnvs(operands);
                IrDomain ker = IrUtil.unionKers(operands);
                IrDomain operandCard = operands[0].getCard();
                int low = operandCard.getLowerBound();
                int high = operandCard.getUpperBound();
                for (int i = 1; i < operands.length; i++) {
                    operandCard = operands[i].getCard();
                    low = Math.max(low, operandCard.getLowerBound());
                    high += operandCard.getUpperBound();
                }
                IrDomain card = boundDomain(
                        Math.max(low, ker.size()),
                        Math.min(high, env.size()));
                return new IrUnion(operands, env, ker, card);
        }
    }
    /**
     * Constraints
     */
    public static final IrBoolConstraint Tautalogy = new IrBoolConstraint($(True));
    public static final IrBoolConstraint FalseTautalogy = new IrBoolConstraint($(False));

    public static IrBoolConstraint boolConstraint(IrBoolExpr expr) {
        if (IrUtil.isTrue(expr)) {
            return Tautalogy;
        }
        if (IrUtil.isFalse(expr)) {
            return FalseTautalogy;
        }
        return new IrBoolConstraint(expr);
    }

    public static IrConstraint boolChannel(IrBoolExpr[] bools, IrSetExpr set) {
        IrDomain env = set.getEnv();
        IrDomain ker = set.getKer();
        boolean entailed = true;
        for (int i = 0; i < bools.length; i++) {
            Boolean constant = IrUtil.getConstant(bools[i]);
            if (Boolean.TRUE.equals(constant)) {
                if (!env.contains(i)) {
                    return FalseTautalogy;
                }
                if (!ker.contains(i)) {
                    entailed = false;
                }
            } else if (Boolean.FALSE.equals(constant)) {
                if (ker.contains(i)) {
                    return FalseTautalogy;
                }
                if (env.contains(i)) {
                    entailed = false;
                }
            } else {
                entailed = false;
            }
        }
        if (entailed) {
            return Tautalogy;
        }
        return new IrBoolChannel(bools, set);
    }

    public static IrIntChannel intChannel(IrIntExpr[] ints, IrSetExpr[] sets) {
        return new IrIntChannel(ints, sets);
    }

    public static IrConstraint sort(IrIntExpr... array) {
        List<IrIntExpr> filter = new ArrayList<IrIntExpr>();
        for (int i = 0; i < array.length - 1; i++) {
            if (array[i].getDomain().getUpperBound() > array[i + 1].getDomain().getLowerBound()) {
                filter.add(array[i]);
            }
        }
        if (array.length > 0) {
            filter.add(array[array.length - 1]);
        }
        switch (filter.size()) {
            case 0:
            case 1:
                return Tautalogy;
//            case 2:
//                return boolConstraint(lessThanEqual(filter.get(0), filter.get(1)));
            default:
                return new IrSortInts(filter.toArray(new IrIntExpr[filter.size()]));
        }
    }

    public static IrConstraint sort(IrIntExpr[]... strings) {
        if (strings.length < 2) {
            return Tautalogy;
        }
        return new IrSortStrings(strings);
    }

    public static IrConstraint allDifferent(IrIntExpr[] ints) {
        if (ints.length < 2) {
            return Tautalogy;
        }
        return new IrAllDifferent(ints);
    }

    public static IrConstraint selectN(IrBoolExpr[] bools, IrIntExpr n) {
        boolean entailed = true;
        IrDomain nDomain = n.getDomain();
        for (int i = 0; i < bools.length; i++) {
            Boolean constant = IrUtil.getConstant(bools[i]);
            if (Boolean.TRUE.equals(constant)) {
                if (i >= nDomain.getUpperBound()) {
                    return FalseTautalogy;
                }
                if (i >= nDomain.getLowerBound()) {
                    entailed = false;
                }
            } else if (Boolean.FALSE.equals(constant)) {
                if (i < nDomain.getLowerBound()) {
                    return FalseTautalogy;
                }
                if (i < nDomain.getUpperBound()) {
                    entailed = false;
                }
            } else {
                entailed = false;
            }
        }
        if (entailed) {
            return Tautalogy;
        }
        return new IrSelectN(bools, n);
    }
}
