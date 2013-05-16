package org.clafer.ir;

import gnu.trove.TIntCollection;
import gnu.trove.set.hash.TIntHashSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import org.clafer.ir.IrDomain.IrBoundDomain;
import org.clafer.ir.IrDomain.IrEmptyDomain;
import org.clafer.ir.IrDomain.IrEnumDomain;
import solver.constraints.IntConstraintFactory;

/**
 * 
 * @author jimmy
 */
public class Irs {

    /********************
     * 
     * Boolean
     * 
     ********************/
    public static final IrBoolVar True = new IrBoolVar("True", Boolean.TRUE);
    public static final IrBoolVar False = new IrBoolVar("False", Boolean.FALSE);

    public static IrBoolVar constant(boolean value) {
        return value ? True : False;
    }

    public static IrBoolVar bool(String name) {
        return new IrBoolVar(name);
    }

    public static IrBoolExpr not(IrBoolExpr proposition) {
        Boolean constant = IrUtil.getConstant(proposition);
        if (constant != null) {
            // Reverse the boolean
            return constant.booleanValue() ? False : True;
        }
        return proposition.opposite();
    }

    public static IrBoolExpr and(IrBoolExpr... operands) {
        List<IrBoolExpr> filter = new ArrayList<IrBoolExpr>(operands.length);
        for (IrBoolExpr operand : operands) {
            if (IrUtil.isFalse(operand)) {
                return False;
            }
            if (!IrUtil.isTrue(operand)) {
                filter.add(operand);
            }
        }
        switch (filter.size()) {
            case 0:
                return True;
            case 1:
                return filter.get(0);
            default:
                return new IrAnd(filter.toArray(new IrBoolExpr[filter.size()]));
        }
    }

    public static IrBoolExpr implies(IrBoolExpr antecedent, IrBoolExpr consequent) {
        if (IrUtil.isTrue(antecedent)) {
            return consequent;
        }
        if (IrUtil.isFalse(antecedent)) {
            return True;
        }
        if (IrUtil.isTrue(consequent)) {
            return True;
        }
        if (IrUtil.isFalse(consequent)) {
            return not(antecedent);
        }
        return new IrImplies(antecedent, consequent);
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
        return new IrIfOnlyIf(left, right);
    }

    public static IrBoolExpr between(IrIntExpr var, int low, int high) {
        IrDomain domain = var.getDomain();
        if (domain.getLowerBound() >= low && domain.getUpperBound() <= high) {
            return True;
        }
        if (domain.getLowerBound() > high || domain.getUpperBound() < low) {
            return False;
        }
        return new IrBetween(var, low, high);
    }

    public static IrBoolExpr notBetween(IrIntExpr var, int low, int high) {
        IrDomain domain = var.getDomain();
        if (domain.getLowerBound() >= low && domain.getUpperBound() <= high) {
            return False;
        }
        if (domain.getLowerBound() > high || domain.getUpperBound() < low) {
            return True;
        }
        return new IrNotBetween(var, low, high);
    }

    public static IrBoolExpr compare(IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
        IrDomain leftDomain = left.getDomain();
        IrDomain rightDomain = right.getDomain();
        switch (op) {
            case Equal:
                if (leftDomain.size() == 1 && rightDomain.size() == 1) {
                    return constant(leftDomain.getLowerBound() == rightDomain.getLowerBound());
                }
                break;
            case NotEqual:
                if (leftDomain.size() == 1 && rightDomain.size() == 1) {
                    return constant(leftDomain.getLowerBound() != rightDomain.getLowerBound());
                }
                break;
            case LessThan:
                if (leftDomain.getUpperBound() < rightDomain.getLowerBound()) {
                    return True;
                }
                if (leftDomain.getLowerBound() >= rightDomain.getUpperBound()) {
                    return False;
                }
                break;
            case LessThanEqual:
                if (leftDomain.getUpperBound() <= rightDomain.getLowerBound()) {
                    return True;
                }
                if (leftDomain.getLowerBound() > rightDomain.getUpperBound()) {
                    return False;
                }
                break;
            case GreaterThan:
                if (leftDomain.getLowerBound() > rightDomain.getUpperBound()) {
                    return True;
                }
                if (leftDomain.getUpperBound() <= rightDomain.getLowerBound()) {
                    return False;
                }
                break;
            case GreaterThanEqual:
                if (leftDomain.getLowerBound() >= rightDomain.getUpperBound()) {
                    return True;
                }
                if (leftDomain.getUpperBound() < rightDomain.getLowerBound()) {
                    return False;
                }
                break;
            default:
                throw new IllegalArgumentException("Unknown op: " + op);
        }
        return new IrCompare(left, op, right);
    }

    public static void main(String[] args) {
    }

    public static IrBoolExpr equal(IrIntExpr left, int right) {
        return equal(left, constant(right));
    }

    public static IrBoolExpr equal(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.Equal, right);
    }

    /**
     * If
     *   env(set) = {a, b, c, ..., x, y}
     *   ker(set) = {a, b, c, ..., x}
     *   domain = {a, b, c, ..., x, z}
     * 
     * then return z. Otherwise return null.
     */
    private static Integer missingEnv(IrSetExpr set, IrDomain domain) {
        IrDomain env = set.getEnv();
        if (env.size() != domain.size()) {
            return null;
        }
        IrDomain ker = set.getKer();
        if (ker.size() != domain.size() - 1) {
            return null;
        }
        if (!IrUtil.isSubsetOf(ker, domain)) {
            return null;
        }
        // Find the missing env.
        if (ker.isEmpty()) {
            return domain.getLowerBound();
        }
        if (domain.getLowerBound() < ker.getLowerBound()) {
            return domain.getLowerBound();
        }
        if (domain.getUpperBound() > ker.getUpperBound()) {
            return domain.getUpperBound();
        }
        for (int i : domain.getValues()) {
            if (!ker.contains(i)) {
                return i;
            }
        }
        // Should not happen because ker(set) ⊂ domain
        throw new IllegalStateException();
    }

    /**
     * If
     *   env(set) = {a, b, c, ..., x, y}
     *   ker(set) = {a, b, c, ..., x}
     *   domain = {a, b, c, ..., x}
     * 
     * then return y. Otherwise return null.
     */
    private static Integer extraEnv(IrSetExpr set, IrDomain domain) {
        IrDomain env = set.getEnv();
        if (env.size() != domain.size() + 1) {
            return null;
        }
        IrDomain ker = set.getKer();
        if (ker.size() != domain.size()) {
            return null;
        }
        if (!IrUtil.isSubsetOf(ker, domain)) {
            return null;
        }
        // Find the extra env.
        if (domain.isEmpty()) {
            return env.getLowerBound();
        }
        if (env.getLowerBound() < domain.getLowerBound()) {
            return env.getLowerBound();
        }
        if (env.getUpperBound() > domain.getUpperBound()) {
            return env.getUpperBound();
        }
        for (int i : env.getValues()) {
            if (!domain.contains(i)) {
                return i;
            }
        }
        // Should not happen because domain = ker(set) ⊂ env(set)
        throw new IllegalStateException();
    }

    public static IrBoolExpr equality(IrSetExpr left, IrSetEquality.Op op, IrSetExpr right) {
        switch (op) {
            case Equal:
                if (left.equals(right)) {
                    return True;
                }
                if (IrUtil.isConstant(left)) {
                    /**
                     * What is this optimization?
                     * 
                     * For example:
                     *   Variables:
                     *     set
                     *       env(set) = {1}
                     *       ker(set) = {}
                     * 
                     *   Constraint:
                     *     set = {1}
                     * 
                     *   Optimize by replacing the constraint with:
                     *     1 ∈ set
                     * 
                     * Note that this (and the next) optimization are only effective
                     * if membership propagators are more efficient, which likely in
                     * practice is neglible. These two optimizations might not be
                     * worthwhile.
                     * 
                     * In the our Clafer encoding into Choco, this optimization often
                     * occurs on the right-hand side of an implication. Special membership
                     * propagators for constant int might have a small improved efficiency.
                     */
                    Integer missingEnv = missingEnv(right, left.getKer());
                    if (missingEnv != null) {
                        return member(constant(missingEnv.intValue()), right);
                    }
                    /**
                     * What is this optimization?
                     * 
                     * For example:
                     *   Variables:
                     *     set
                     *       env(set) = {1}
                     *       ker(set) = {}
                     * 
                     *   Constraint:
                     *     set = {}
                     * 
                     *   Optimize by replacing the constraint with:
                     *     1 ∉ set
                     */
                    Integer extraEnv = extraEnv(right, left.getKer());
                    if (extraEnv != null) {
                        return notMember(constant(extraEnv.intValue()), right);
                    }
                }
                if (IrUtil.isConstant(right)) {
                    Integer missingEnv = missingEnv(left, right.getKer());
                    if (missingEnv != null) {
                        return member(constant(missingEnv.intValue()), left);
                    }
                    Integer extraEnv = extraEnv(left, right.getKer());
                    if (extraEnv != null) {
                        return notMember(constant(extraEnv.intValue()), left);
                    }
                }
                break;
            case NotEqual:
                if (left.equals(right)) {
                    return False;
                }
                if (IrUtil.isConstant(left)) {
                    Integer missingEnv = missingEnv(right, left.getKer());
                    if (missingEnv != null) {
                        return notMember(constant(missingEnv.intValue()), right);
                    }
                    Integer extraEnv = extraEnv(right, left.getKer());
                    if (extraEnv != null) {
                        return member(constant(extraEnv.intValue()), right);
                    }
                }
                if (IrUtil.isConstant(right)) {
                    Integer missingEnv = missingEnv(left, right.getKer());
                    if (missingEnv != null) {
                        return notMember(constant(missingEnv.intValue()), left);
                    }
                    Integer extraEnv = extraEnv(left, right.getKer());
                    if (extraEnv != null) {
                        return member(constant(extraEnv.intValue()), left);
                    }
                }
                break;
            default:
                throw new IllegalArgumentException();
        }
        return new IrSetEquality(left, op, right);
    }

    public static IrBoolExpr equal(IrSetExpr left, IrSetExpr right) {
        return equality(left, IrSetEquality.Op.Equal, right);
    }

    public static IrBoolExpr notEqual(IrIntExpr left, int right) {
        return notEqual(left, constant(right));
    }

    public static IrBoolExpr notEqual(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.NotEqual, right);
    }

    public static IrBoolExpr notEqual(IrSetExpr left, IrSetExpr right) {
        return equality(left, IrSetEquality.Op.NotEqual, right);
    }

    public static IrBoolExpr lessThan(IrIntExpr left, int right) {
        return lessThan(left, constant(right));
    }

    public static IrBoolExpr lessThan(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.LessThan, right);
    }

    public static IrBoolExpr lessThanEqual(IrIntExpr left, int right) {
        return lessThanEqual(left, constant(right));
    }

    public static IrBoolExpr lessThanEqual(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.LessThanEqual, right);
    }

    public static IrBoolExpr greaterThan(IrIntExpr left, int right) {
        return greaterThan(left, constant(right));
    }

    public static IrBoolExpr greaterThan(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.GreaterThan, right);
    }

    public static IrBoolExpr greaterThanEqual(IrIntExpr left, int right) {
        return greaterThanEqual(left, constant(right));
    }

    public static IrBoolExpr greaterThanEqual(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.GreaterThanEqual, right);
    }

    public static IrBoolExpr member(IrIntExpr element, IrSetExpr set) {
        if (IrUtil.isSubsetOf(element.getDomain(), set.getKer())) {
            return True;
        }
        if (!IrUtil.intersects(element.getDomain(), set.getEnv())) {
            return False;
        }
        return new IrMember(element, set);
    }

    public static IrBoolExpr notMember(IrIntExpr element, IrSetExpr set) {
        if (!IrUtil.intersects(element.getDomain(), set.getEnv())) {
            return True;
        }
        if (IrUtil.isSubsetOf(element.getDomain(), set.getKer())) {
            return False;
        }
        return new IrNotMember(element, set);
    }
    /********************
     * 
     * Integers
     * 
     ********************/
    public static IrIntExpr Zero = constant(0);
    public static IrIntExpr One = constant(1);

    public static IrIntVar constant(int value) {
        return boundInt(Integer.toString(value), value, value);
    }

    public static IrIntVar boundInt(String name, int low, int high) {
        return new IrIntVar(name, boundDomain(low, high));
    }

    public static IrIntVar enumInt(String name, int[] values) {
        return new IrIntVar(name, enumDomain(values));
    }

    public static IrIntExpr card(IrSetExpr set) {
        IrDomain card = set.getCard();
        if (card.size() == 1) {
            return constant(card.getLowerBound());
        }
        return new IrCard(set);
    }

    public static IrIntExpr arithm(IrArithm.Op op, IrIntExpr... operands) {
        int constants;
        Deque<IrIntExpr> filter = new LinkedList<IrIntExpr>();
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
                    filter.add(constant(constants));
                }
                if (filter.isEmpty()) {
                    return Zero;
                }
                if (filter.size() == 1) {
                    return filter.getFirst();
                }
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
                    return constant(head - constants);
                }
                filter.addFirst(operands[0]);
                if (constants != 0) {
                    filter.add(constant(constants));
                }
                if (filter.size() == 1) {
                    return filter.getFirst();
                }
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
                    return Zero;
                }
                if (constants != 1) {
                    filter.add(constant(constants));
                }
                if (filter.isEmpty()) {
                    return One;
                }
                if (filter.size() == 1) {
                    return filter.getFirst();
                }
                break;
            case Div:
                // TODO: optimize
                filter.addAll(Arrays.asList(operands));
                if (filter.size() == 1) {
                    return filter.getFirst();
                }
                break;
            default:
                throw new IllegalArgumentException();
        }
        return new IrArithm(op, filter.toArray(new IrIntExpr[filter.size()]));
    }

    public static IrIntExpr add(IrIntExpr left, int right) {
        return add(left, constant(right));
    }

    public static IrIntExpr add(IrIntExpr... operands) {
        return arithm(IrArithm.Op.Add, operands);
    }

    public static IrIntExpr sub(IrIntExpr left, int right) {
        return sub(left, constant(right));
    }

    public static IrIntExpr sub(IrIntExpr... operands) {
        return new IrArithm(IrArithm.Op.Sub, operands);
    }

    public static IrIntExpr mul(IrIntExpr left, int right) {
        return mul(left, constant(right));
    }

    public static IrIntExpr mul(IrIntExpr... operands) {
        return arithm(IrArithm.Op.Mul, operands);
    }

    public static IrIntExpr div(IrIntExpr left, int right) {
        return div(left, constant(right));
    }

    public static IrIntExpr div(IrIntExpr... operands) {
        return new IrArithm(IrArithm.Op.Div, operands);
    }

    public static IrIntExpr element(IrIntExpr[] array, IrIntExpr index) {
        Integer constant = IrUtil.getConstant(index);
        if (constant != null) {
            return array[constant.intValue()];
        }
        return new IrElement(array, index);
    }
    /********************
     * 
     * Set
     * 
     ********************/
    public static final IrDomain EmptyDomain = new IrEmptyDomain();
    public static final IrDomain ZeroDomain = new IrEnumDomain(new int[]{0});
    public static final IrDomain OneDomain = new IrEnumDomain(new int[]{1});
    public static final IrSetVar EmptySet = new IrSetVar("{}", EmptyDomain, EmptyDomain, ZeroDomain);

    public static IrDomain boundDomain(int low, int high) {
        return new IrBoundDomain(low, high);
    }

    public static IrDomain enumDomain(int... values) {
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

    public static IrSetVar constant(int[] value) {
        IrDomain domain = enumDomain(value);
        return new IrSetVar(Arrays.toString(value), domain, domain, enumDomain(value.length));
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
        return new IrSetVar(name, env, ker, boundDomain(ker.size(), env.size()));
    }

    public static IrSetVar set(String name, IrDomain env, IrDomain ker, IrDomain card) {
        return new IrSetVar(name, env, ker, card);
    }

    public static IrSetExpr singleton(IrIntExpr value) {
        Integer constant = IrUtil.getConstant(value);
        if (constant != null) {
            return constant(new int[]{constant.intValue()});
        }
        return new IrSingleton(value);
    }

    public static IrSetExpr arrayToSet(IrIntExpr[] array) {
        switch (array.length) {
            case 0:
                return EmptySet;
            case 1:
                return singleton(array[0]);
            default:
                return new IrArrayToSet(array);
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
        return new IrJoin(take, children);
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
        return new IrJoinRef(take, refs);
    }

    // TODO: Partially union set of constants
    public static IrSetExpr union(IrSetExpr[] operands) {
        switch (operands.length) {
            case 0:
                return EmptySet;
            case 1:
                return operands[0];
            default:
                return new IrUnion(operands);
        }
    }
    /**
     * Constraints
     */
    public static final IrBoolConstraint Tautalogy = new IrBoolConstraint(True);
    public static final IrBoolConstraint FalseTautalogy = new IrBoolConstraint(False);

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
