package org.clafer.ir;

import gnu.trove.TIntCollection;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import gnu.trove.stack.TIntStack;
import gnu.trove.stack.array.TIntArrayStack;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.PrimitiveIterator;
import org.clafer.common.UnsatisfiableException;
import org.clafer.common.Util;
import org.clafer.domain.Domain;
import static org.clafer.domain.Domain.constantDomain;
import org.clafer.domain.Domains;
import static org.clafer.domain.Domains.EmptyDomain;
import static org.clafer.domain.Domains.NegativeOneDomain;
import static org.clafer.domain.Domains.TrueFalseDomain;
import static org.clafer.domain.Domains.ZeroOneDomain;
import static org.clafer.domain.Domains.boundDomain;
import static org.clafer.domain.Domains.enumDomain;
import static org.clafer.domain.Domains.enumDomains;
import static org.clafer.domain.Domains.fromToDomain;

/**
 * Import this class to access all IR building functions.
 * <pre>
 * import static org.clafer.ast.Asts.*;
 * </pre>
 *
 * @author jimmy
 */
public class Irs {

    private Irs() {
    }

    /**
     *******************
     *
     * Boolean
     *
     *******************
     */
    public static final IrBoolVar True = new IrBoolVar(true);
    public static final IrBoolVar False = new IrBoolVar(false);

    public static IrBoolVar constant(boolean value) {
        return value ? True : False;
    }

    public static IrBoolVar bool(String name) {
        return new IrBoolVar(name, TrueFalseDomain);
    }

    public static IrBoolExpr not(IrBoolExpr proposition) {
        if (proposition.isConstant()) {
            // Reverse the boolean
            return proposition.getDomain().isTrue() ? False : True;
        }
        return proposition.negate();
    }

    public static IrBoolExpr and(Collection<? extends IrBoolExpr> operands) {
        return and(operands.toArray(new IrBoolExpr[operands.size()]));
    }

    public static IrBoolExpr and(IrBoolExpr... operands) {
        List<IrBoolExpr> flatten = new ArrayList<>(operands.length);
        for (IrBoolExpr operand : operands) {
            if (operand instanceof IrAnd) {
                // Invariant: No nested IrAnd
                flatten.addAll(Arrays.asList(((IrAnd) operand).getOperands()));
            } else {
                flatten.add(operand);
            }
        }
        List<IrBoolExpr> filter = new ArrayList<>(flatten.size());
        for (IrBoolExpr operand : flatten) {
            if (operand.getDomain().isFalse()) {
                return False;
            }
            if (!operand.getDomain().isTrue()) {
                filter.add(operand);
            }
        }
        switch (filter.size()) {
            case 0:
                return True;
            case 1:
                return filter.get(0);
            default:
                return new IrAnd(filter.toArray(new IrBoolExpr[filter.size()]), TrueFalseDomain);
        }
    }

    public static IrBoolExpr lone(Collection<? extends IrBoolExpr> operands) {
        return lone(operands.toArray(new IrBoolExpr[operands.size()]));
    }

    public static IrBoolExpr lone(IrBoolExpr... operands) {
        List<IrBoolExpr> filter = new ArrayList<>(operands.length);
        int count = 0;
        for (IrBoolExpr operand : operands) {
            if (operand.getDomain().isTrue()) {
                count++;
                if (count > 1) {
                    return False;
                }
            } else if (!operand.getDomain().isFalse()) {
                filter.add(operand);
            }
        }
        assert count == 0 || count == 1;
        switch (filter.size()) {
            case 0:
                return True;
            case 1:
                return count == 0 ? True : not(filter.get(0));
            default:
                IrBoolExpr[] f = filter.toArray(new IrBoolExpr[filter.size()]);
                return count == 0
                        ? new IrLone(f, TrueFalseDomain)
                        : not(or(f));
        }
    }

    public static IrBoolExpr one(Collection<? extends IrBoolExpr> operands) {
        return one(operands.toArray(new IrBoolExpr[operands.size()]));
    }

    public static IrBoolExpr one(IrBoolExpr... operands) {
        List<IrBoolExpr> filter = new ArrayList<>(operands.length);
        int count = 0;
        for (IrBoolExpr operand : operands) {
            if (operand.getDomain().isTrue()) {
                count++;
                if (count > 1) {
                    return False;
                }
            } else if (!operand.getDomain().isFalse()) {
                filter.add(operand);
            }
        }
        assert count == 0 || count == 1;
        switch (filter.size()) {
            case 0:
                return count == 0 ? False : True;
            case 1:
                return count == 0 ? filter.get(0) : not(filter.get(0));
            case 2:
                return count == 0
                        ? xor(filter.get(0), filter.get(1))
                        : and(not(filter.get(0)), not(filter.get(1)));
            default:
                IrBoolExpr[] f = filter.toArray(new IrBoolExpr[filter.size()]);
                return count == 0
                        ? new IrOne(f, TrueFalseDomain)
                        : not(or(f));
        }
    }

    public static IrBoolExpr or(Collection<? extends IrBoolExpr> operands) {
        return or(operands.toArray(new IrBoolExpr[operands.size()]));
    }

    public static IrBoolExpr or(IrBoolExpr... operands) {
        List<IrBoolExpr> flatten = new ArrayList<>(operands.length);
        for (IrBoolExpr operand : operands) {
            if (operand instanceof IrOr) {
                // Invariant: No nested IrOr
                flatten.addAll(Arrays.asList(((IrOr) operand).getOperands()));
            } else {
                flatten.add(operand);
            }
        }
        List<IrBoolExpr> filter = new ArrayList<>(flatten.size());
        for (IrBoolExpr operand : flatten) {
            if (operand.getDomain().isTrue()) {
                return True;
            }
            if (!operand.getDomain().isFalse()) {
                filter.add(operand);
            }
        }
        switch (filter.size()) {
            case 0:
                return False;
            case 1:
                return filter.get(0);
            default:
                return new IrOr(filter.toArray(new IrBoolExpr[filter.size()]), TrueFalseDomain);
        }
    }

    public static IrBoolExpr implies(IrBoolExpr antecedent, IrBoolExpr consequent) {
        return or(not(antecedent), consequent);
    }

    public static IrBoolExpr ifThenElse(IrBoolExpr antecedent, IrBoolExpr consequent, IrBoolExpr alternative) {
        if (antecedent.getDomain().isTrue()) {
            return consequent;
        }
        if (antecedent.getDomain().isFalse()) {
            return alternative;
        }
        if (consequent.getDomain().isTrue()) {
            return or(antecedent, alternative);
        }
        if (consequent.getDomain().isFalse()) {
            return and(antecedent.negate(), alternative);
        }
        if (alternative.getDomain().isTrue()) {
            return or(antecedent.negate(), consequent);
        }
        if (alternative.getDomain().isFalse()) {
            return and(antecedent, consequent);
        }
        return new IrIfThenElse(antecedent, consequent, alternative, TrueFalseDomain);
    }

    public static IrBoolExpr ifOnlyIf(IrBoolExpr left, IrBoolExpr right) {
        if (left.equals(right)) {
            return True;
        }
        if (left.getDomain().isTrue()) {
            return right;
        }
        if (left.getDomain().isFalse()) {
            return not(right);
        }
        if (right.getDomain().isTrue()) {
            return left;
        }
        if (right.getDomain().isFalse()) {
            return not(left);
        }
        if (left.isNegative() && right.isNegative()) {
            return new IrIfOnlyIf(left.negate(), right.negate(), TrueFalseDomain);
        }
        return new IrIfOnlyIf(left, right, TrueFalseDomain);
    }

    public static IrBoolExpr xor(IrBoolExpr left, IrBoolExpr right) {
        if (left.getDomain().isTrue()) {
            return not(right);
        }
        if (left.getDomain().isFalse()) {
            return right;
        }
        if (right.getDomain().isTrue()) {
            return not(left);
        }
        if (right.getDomain().isFalse()) {
            return left;
        }
        if (left.isNegative()) {
            return ifOnlyIf(left.negate(), right);
        }
        if (right.isNegative()) {
            return ifOnlyIf(left, right.negate());
        }
        return new IrIfOnlyIf(left, right.negate(), TrueFalseDomain);
    }

    public static IrBoolExpr within(IrIntExpr value, Domain range) {
        Domain domain = value.getDomain();
        range = domain.intersection(range);
        if (range.isEmpty()) {
            return False;
        }
        if (range.isBounded()
                && domain.getLowBound() >= range.getLowBound()
                && domain.getHighBound() <= range.getHighBound()) {
            return True;
        }
        if (domain.getLowBound() > range.getHighBound()
                || domain.getHighBound() < range.getLowBound()) {
            return False;
        }
        if (range.isConstant()) {
            return equal(value, range.getLowBound());
        }
        Domain diff = domain.difference(range);
        switch (diff.size()) {
            case 0:
                return True;
            case 1:
                return notEqual(value, diff.getLowBound());
            default:
                return new IrWithin(value, range, TrueFalseDomain);
        }
    }

    public static IrBoolExpr compare(int left, IrCompare.Op op, IrIntExpr right) {
        return compare(constant(left), op, right);
    }

    public static IrBoolExpr compare(IrIntExpr left, IrCompare.Op op, int right) {
        return compare(left, op, constant(right));
    }

    public static IrBoolExpr compare(IrIntExpr left, IrCompare.Op op, IrIntExpr right) {
        Domain leftDomain = left.getDomain();
        Domain rightDomain = right.getDomain();
        switch (op) {
            case Equal:
                if (left.equals(right)) {
                    return True;
                }
                if (!leftDomain.intersects(rightDomain)) {
                    return False;
                }
                if (left instanceof IrBoolExpr && right instanceof IrBoolExpr) {
                    return ifOnlyIf((IrBoolExpr) left, (IrBoolExpr) right);
                }
                break;
            case NotEqual:
                if (left.equals(right)) {
                    return False;
                }
                if (!leftDomain.intersects(rightDomain)) {
                    return True;
                }
                if (left instanceof IrBoolExpr && right instanceof IrBoolExpr) {
                    return xor((IrBoolExpr) left, (IrBoolExpr) right);
                }
                break;
            case LessThan:
                if (left.equals(right)) {
                    return False;
                }
                if (leftDomain.getHighBound() < rightDomain.getLowBound()) {
                    return True;
                }
                if (leftDomain.getLowBound() >= rightDomain.getHighBound()) {
                    return False;
                }
                if (left instanceof IrBoolExpr && right instanceof IrBoolExpr) {
                    return not(implies((IrBoolExpr) right, (IrBoolExpr) left));
                }
                if (left instanceof IrMinus && right instanceof IrMinus) {
                    return greaterThan(((IrMinus) left).getExpr(), ((IrMinus) right).getExpr());
                }
                break;
            case LessThanEqual:
                if (left.equals(right)) {
                    return True;
                }
                if (leftDomain.getHighBound() <= rightDomain.getLowBound()) {
                    return True;
                }
                if (leftDomain.getLowBound() > rightDomain.getHighBound()) {
                    return False;
                }
                if (leftDomain.getLowBound() == rightDomain.getHighBound()) {
                    return equal(left, right);
                }
                if (left instanceof IrBoolExpr && right instanceof IrBoolExpr) {
                    return implies((IrBoolExpr) left, (IrBoolExpr) right);
                }
                if (left instanceof IrMinus && right instanceof IrMinus) {
                    return greaterThanEqual(((IrMinus) left).getExpr(), ((IrMinus) right).getExpr());
                }
                break;
            default:
                throw new IllegalArgumentException("Unknown op: " + op);
        }
        return new IrCompare(left, op, right, TrueFalseDomain);
    }

    public static IrBoolExpr compare(IrStringExpr left, IrStringCompare.Op op, IrStringExpr right) {
        if (left.equals(right)) {
            switch (op) {
                case Equal:
                case LessThanEqual:
                    return True;
                case NotEqual:
                case LessThan:
                    return False;
                default:
                    throw new IllegalArgumentException();
            }
        }
        if (!left.getLength().intersects(right.getLength())) {
            switch (op) {
                case Equal:
                    return False;
                case NotEqual:
                    return True;
            }
        }
        int minLength = Math.min(left.getLength().getLowBound(), right.getLength().getLowBound());
        for (int i = 0; i < minLength; i++) {
            if (!left.getChars()[i].intersects(right.getChars()[i])) {
                switch (op) {
                    case Equal:
                        return False;
                    case NotEqual:
                        return True;
                }
            }
        }
        return new IrStringCompare(left, op, right, TrueFalseDomain);
    }

    public static IrBoolExpr equal(int left, IrIntExpr right) {
        return equal(constant(left), right);
    }

    public static IrBoolExpr equal(IrIntExpr left, int right) {
        return equal(left, constant(right));
    }

    public static IrBoolExpr equal(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.Equal, right);
    }

    public static IrBoolExpr equal(IrIntExpr[] left, IrIntExpr[] right) {
        if (left.length != right.length) {
            throw new IllegalArgumentException();
        }
        IrBoolExpr[] ands = new IrBoolExpr[left.length];
        for (int i = 0; i < ands.length; i++) {
            ands[i] = equal(left[i], right[i]);
        }
        return and(ands);
    }

    public static IrBoolExpr equality(IrIntArrayExpr left, IrArrayEquality.Op op, IrIntArrayExpr right) {
        if (op.equals(IrArrayEquality.Op.Equal) && left instanceof IrIntArrayVar && right instanceof IrIntArrayVar) {
            return equal((IrIntArrayVar) left, (IrIntArrayVar) right);
        }
        if (left.length() != right.length()) {
            throw new IllegalArgumentException();
        }
        for (int i = 0; i < left.length(); i++) {
            if (!left.getDomains()[i].intersects(right.getDomains()[i])) {
                switch (op) {
                    case Equal:
                        return False;
                    case NotEqual:
                        return True;
                }
            }
        }
        return new IrArrayEquality(left, op, right, TrueFalseDomain);
    }

    public static IrBoolExpr equal(IrIntArrayVar left, IrIntArrayVar right) {
        if (left.length() != right.length()) {
            throw new IllegalArgumentException();
        }
        List<IrIntExpr> filterLeft = new ArrayList<>(left.length());
        List<IrIntExpr> filterRight = new ArrayList<>(right.length());
        for (int i = 0; i < left.length(); i++) {
            if (!left.getDomains()[i].intersects(right.getDomains()[i])) {
                return False;
            }
            if (!left.getArray()[i].equals(right.getArray()[i])) {
                filterLeft.add(left.getArray()[i]);
                filterRight.add(right.getArray()[i]);
            }
        }
        if (filterLeft.isEmpty()) {
            return True;
        }
        if (filterLeft.size() == 1) {
            return equal(filterLeft.get(0), filterRight.get(0));
        }
        if (filterLeft.size() == left.length()) {
            return new IrArrayEquality(left, IrArrayEquality.Op.Equal, right, TrueFalseDomain);
        }
        return new IrArrayEquality(
                array(filterLeft.toArray(new IrIntExpr[filterLeft.size()])),
                IrArrayEquality.Op.Equal,
                array(filterRight.toArray(new IrIntExpr[filterRight.size()])),
                TrueFalseDomain);
    }

    public static IrBoolExpr equal(IrIntArrayExpr left, IrIntArrayExpr right) {
        return equality(left, IrArrayEquality.Op.Equal, right);
    }

    public static IrBoolExpr equality(IrSetExpr left, IrSetEquality.Op op, IrSetExpr right) {
        switch (op) {
            case Equal:
                if (left.equals(right)) {
                    return True;
                }
                if (!left.getKer().isSubsetOf(right.getEnv())
                        || !right.getKer().isSubsetOf(left.getEnv())) {
                    return False;
                }
                if (!left.getCard().intersects(right.getCard())) {
                    return False;
                }
                if (left.isConstant()) {
                    Domain constant = left.getKer();
                    /*
                     * The idea is that integer constraints are easier to
                     * optimize than set constraints. If the expression is a top
                     * level expression than the cardinality propagator will
                     * optimize this expression away anyways.
                     */
                    if (constant.isEmpty()) {
                        return equal(card(right), 0);
                    }
                    if (constant.size() == right.getEnv().size()) {
                        return constant.isSubsetOf(right.getEnv())
                                ? equal(card(right), constant.size()) : False;
                    }
                }
                if (right.isConstant()) {
                    Domain constant = right.getKer();
                    if (constant.isEmpty()) {
                        return equal(card(left), 0);
                    }
                    if (constant.size() == left.getEnv().size()) {
                        return constant.isSubsetOf(left.getEnv())
                                ? equal(card(left), constant.size()) : False;
                    }
                }
                IrIntExpr leftInt = IrUtil.asInt(left);
                if (leftInt != null) {
                    IrIntExpr rightInt = IrUtil.asInt(right);
                    if (rightInt != null) {
                        return equal(leftInt, rightInt);
                    }
                }
                break;
            case NotEqual:
                if (left.equals(right)) {
                    return False;
                }
                if (!left.getKer().isSubsetOf(right.getEnv())
                        || !right.getKer().isSubsetOf(left.getEnv())) {
                    return True;
                }
                if (!left.getCard().intersects(right.getCard())) {
                    return True;
                }
                if (left.isConstant()) {
                    Domain constant = left.getKer();
                    if (constant.isEmpty()) {
                        return notEqual(card(right), 0);
                    }
                    if (constant.size() == right.getEnv().size()) {
                        return constant.isSubsetOf(right.getEnv())
                                ? notEqual(card(right), constant.size()) : True;
                    }
                }
                if (right.isConstant()) {
                    Domain constant = right.getKer();
                    if (constant.isEmpty()) {
                        return notEqual(card(left), 0);
                    }
                    if (constant.size() == left.getEnv().size()) {
                        return constant.isSubsetOf(left.getEnv())
                                ? notEqual(card(left), constant.size()) : True;
                    }
                }
                leftInt = IrUtil.asInt(left);
                if (leftInt != null) {
                    IrIntExpr rightInt = IrUtil.asInt(right);
                    if (rightInt != null) {
                        return notEqual(leftInt, rightInt);
                    }
                }
                break;
            default:
                throw new IllegalArgumentException();
        }
        return new IrSetEquality(left, op, right, TrueFalseDomain);
    }

    public static IrBoolExpr equal(IrSetExpr left, IrSetExpr right) {
        return equality(left, IrSetEquality.Op.Equal, right);
    }

    public static IrBoolExpr equal(IrSetExpr[] left, IrSetExpr[] right) {
        if (left.length != right.length) {
            throw new IllegalArgumentException();
        }
        IrBoolExpr[] ands = new IrBoolExpr[left.length];
        for (int i = 0; i < ands.length; i++) {
            ands[i] = equal(left[i], right[i]);
        }
        return and(ands);
    }

    public static IrBoolExpr equal(IrStringExpr left, IrStringExpr right) {
        return compare(left, IrStringCompare.Op.Equal, right);
    }

    public static IrBoolExpr notEqual(int left, IrIntExpr right) {
        return notEqual(constant(left), right);
    }

    public static IrBoolExpr notEqual(IrIntExpr left, int right) {
        return notEqual(left, constant(right));
    }

    public static IrBoolExpr notEqual(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.NotEqual, right);
    }

    public static IrBoolExpr notEqual(IrIntArrayExpr left, IrIntArrayExpr right) {
        return equality(left, IrArrayEquality.Op.NotEqual, right);
    }

    public static IrBoolExpr notEqual(IrSetExpr left, IrSetExpr right) {
        return equality(left, IrSetEquality.Op.NotEqual, right);
    }

    public static IrBoolExpr notEqual(IrStringExpr left, IrStringExpr right) {
        return compare(left, IrStringCompare.Op.NotEqual, right);
    }

    public static IrBoolExpr lessThan(int left, IrIntExpr right) {
        return lessThan(constant(left), right);
    }

    public static IrBoolExpr lessThan(IrIntExpr left, int right) {
        return lessThan(left, constant(right));
    }

    public static IrBoolExpr lessThan(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.LessThan, right);
    }

    public static IrBoolExpr lessThanEqual(int left, IrIntExpr right) {
        return lessThanEqual(constant(left), right);
    }

    public static IrBoolExpr lessThanEqual(IrIntExpr left, int right) {
        return lessThanEqual(left, constant(right));
    }

    public static IrBoolExpr lessThanEqual(IrIntExpr left, IrIntExpr right) {
        return compare(left, IrCompare.Op.LessThanEqual, right);
    }

    public static IrBoolExpr greaterThan(int left, IrIntExpr right) {
        return greaterThan(constant(left), right);
    }

    public static IrBoolExpr greaterThan(IrIntExpr left, int right) {
        return greaterThan(left, constant(right));
    }

    public static IrBoolExpr greaterThan(IrIntExpr left, IrIntExpr right) {
        return compare(right, IrCompare.Op.LessThan, left);
    }

    public static IrBoolExpr greaterThanEqual(int left, IrIntExpr right) {
        return greaterThanEqual(constant(left), right);
    }

    public static IrBoolExpr greaterThanEqual(IrIntExpr left, int right) {
        return greaterThanEqual(left, constant(right));
    }

    public static IrBoolExpr greaterThanEqual(IrIntExpr left, IrIntExpr right) {
        return compare(right, IrCompare.Op.LessThanEqual, left);
    }

    public static IrBoolExpr member(IrIntExpr element, IrSetExpr set) {
        if (element.getDomain().isSubsetOf(set.getKer())) {
            return True;
        }
        if (!element.getDomain().intersects(set.getEnv())) {
            return False;
        }
        if (set.isConstant()) {
            return within(element, set.getKer());
        }
        return new IrMember(element, set, TrueFalseDomain);
    }

    public static IrBoolExpr notMember(IrIntExpr element, IrSetExpr set) {
        if (!element.getDomain().intersects(set.getEnv())) {
            return True;
        }
        if (element.getDomain().isSubsetOf(set.getKer())) {
            return False;
        }
        if (set.isConstant()) {
            return within(element, set.getKer()).negate();
        }
        return new IrNotMember(element, set, TrueFalseDomain);
    }

    public static IrBoolExpr subsetEq(IrSetExpr subset, IrSetExpr superset) {
        if (subset.getEnv().isSubsetOf(superset.getKer())) {
            return True;
        }
        if (subset.getCard().getLowBound() == superset.getCard().getHighBound()) {
            return equal(subset, superset);
        }
        return new IrSubsetEq(subset, superset, TrueFalseDomain);
    }

    public static IrBoolExpr boolChannel(IrBoolExpr[] bools, IrSetExpr set) {
        if (set.getEnv().isEmpty()
                || (set.getEnv().getLowBound() >= 0 && set.getEnv().getHighBound() < bools.length)) {
            if (set.isConstant()) {
                IrBoolExpr[] ands = new IrBoolExpr[bools.length];
                for (int i = 0; i < ands.length; i++) {
                    ands[i] = equal(bools[i], set.getKer().contains(i) ? True : False);
                }
                return and(ands);
            }
            if (bools.length == 1) {
                return equal(bools[0], card(set));
            }
            TIntHashSet values = new TIntHashSet();
            Domain env = set.getEnv();
            Domain ker = set.getKer();
            boolean entailed = true;
            for (int i = 0; i < bools.length; i++) {
                if (bools[i].getDomain().isTrue()) {
                    if (values != null) {
                        values.add(i);
                    }
                    if (!env.contains(i)) {
                        return False;
                    }
                    if (!ker.contains(i)) {
                        entailed = false;
                    }
                } else if (bools[i].getDomain().isFalse()) {
                    if (ker.contains(i)) {
                        return False;
                    }
                    if (env.contains(i)) {
                        entailed = false;
                    }
                } else {
                    values = null;
                    entailed = false;
                }
            }
            if (entailed) {
                return True;
            }
            if (values != null) {
                return equal(set, constant(enumDomain(values)));
            }
        }
        return new IrBoolChannel(bools, set, TrueFalseDomain);
    }

    public static IrBoolExpr intChannel(IrIntExpr[] ints, IrSetExpr[] sets) {
        if (ints.length == 0) {
            IrBoolExpr[] empty = new IrBoolExpr[sets.length];
            for (int i = 0; i < empty.length; i++) {
                empty[i] = equal(sets[i], EmptySet);
            }
            return and(empty);
        }
        if (sets.length == 0) {
            return ints.length == 0 ? True : False;
        }
        boolean entailed = true;
        for (int i = 0; i < ints.length; i++) {
            if (ints[i].isConstant()) {
                int constant = ints[i].getLowBound();
                if (constant < 0 || constant >= sets.length) {
                    return False;
                }
                IrSetExpr set = sets[constant];
                if (!set.getEnv().contains(i)) {
                    return False;
                } else if (!set.getKer().contains(i)) {
                    entailed = false;
                }
            } else {
                entailed = false;
            }
        }
        for (int i = 0; i < sets.length; i++) {
            if (sets[i].isConstant()) {
                PrimitiveIterator.OfInt iter = sets[i].getKer().iterator();
                while (iter.hasNext()) {
                    int j = iter.next();
                    if (j < 0 || j >= ints.length) {
                        return False;
                    }
                    IrIntExpr iexpr = ints[j];
                    if (!iexpr.getDomain().contains(i)) {
                        return False;
                    } else if (iexpr.getDomain().size() != 1) {
                        entailed = false;
                    }
                }
            } else {
                entailed = false;
            }
        }
        if (entailed) {
            return True;
        }
        return new IrIntChannel(ints, sets, TrueFalseDomain);
    }

    public static IrBoolExpr sort(IrIntExpr... array) {
        if (array.length <= 1) {
            return True;
        }
        IrBoolExpr[] sort = new IrBoolExpr[array.length - 1];
        for (int i = 0; i < array.length - 1; i++) {
            sort[i] = lessThanEqual(array[i], array[i + 1]);
        }
        return and(sort);
    }

    public static IrBoolExpr sortStrict(IrIntExpr... array) {
        if (array.length <= 1) {
            return True;
        }
        IrBoolExpr[] sort = new IrBoolExpr[array.length - 1];
        for (int i = 0; i < array.length - 1; i++) {
            sort[i] = lessThan(array[i], array[i + 1]);
        }
        return and(sort);
    }

    public static IrBoolExpr sort(IrSetExpr[] sets, IrIntExpr[] bounds) {
        if (sets.length != bounds.length) {
            throw new IllegalArgumentException();
        }
        if (sets.length == 0) {
            return True;
        }
        if (bounds.length == 1 || bounds[bounds.length - 2].isConstant()) {
            int low = bounds.length == 1 ? 0 : bounds[bounds.length - 2].getLowBound();
            if (bounds[bounds.length - 1].isConstant()) {
                int high = bounds[bounds.length - 1].getLowBound();
                Domain bound = fromToDomain(low, high);

                IrSetExpr[] filterSets = Arrays.copyOf(sets, sets.length - 1);
                IrIntExpr[] filterBounds = Arrays.copyOf(bounds, bounds.length - 1);
                return and(equal(sets[sets.length - 1], constant(bound)), sort(filterSets, filterBounds));
            }
        }
        // TODO optimize
        return new IrSortSets(sets, bounds, TrueFalseDomain);
    }

    private static IrBoolExpr sortStrings(IrIntExpr[][] strings, boolean strict) {
        if (strings.length < 2) {
            return True;
        }

        boolean[] filter = new boolean[strings.length];
        for (int i = 0; i < strings.length - 1; i++) {
            if (strict) {
                switch (IrUtil.compareString(strings[i], strings[i + 1])) {
                    case LT:
                        break;
                    case GT:
                    case GE:
                    case EQ:
                        return False;
                    default:
                        filter[i] = true;
                        filter[i + 1] = true;
                }
            } else {
                switch (IrUtil.compareString(strings[i], strings[i + 1])) {
                    case LT:
                        break;
                    case GT:
                        return False;
                    default:
                        filter[i] = true;
                        filter[i + 1] = true;
                }
            }
        }
        List<IrIntExpr[]> filterStrings = new ArrayList<>(strings.length);
        for (int i = 0; i < filter.length; i++) {
            if (filter[i]) {
                filterStrings.add(strings[i]);
            }
        }
        if (filterStrings.size() != strings.length) {
            return sortStrings(filterStrings.toArray(new IrIntExpr[filterStrings.size()][]), strict);
        }

        int minLength = strings[0].length;
        for (int i = 1; i < strings.length; i++) {
            minLength = Math.min(minLength, strings[i].length);
        }
        for (int i = 0; i < minLength; i++) {
            boolean allSame = true;
            for (int j = 0; allSame && j < strings.length - 1; j++) {
                allSame &= strings[j][i].equals(strings[j + 1][i]);
            }
            if (allSame) {
                IrIntExpr[][] reducedString = new IrIntExpr[strings.length][];
                for (int k = 0; k < reducedString.length; k++) {
                    reducedString[k] = new IrIntExpr[strings[k].length - 1];
                    System.arraycopy(strings[k], 0, reducedString[k], 0, i);
                    for (int l = i + 1; l < strings[k].length; l++) {
                        reducedString[k][l - 1] = strings[k][l];
                    }
                }
                return sortStrings(reducedString, strict);
            }
        }

        if (minLength == 0) {
            return strict ? False : True;
        }
        if (minLength == 1) {
            IrIntExpr[] array = new IrIntExpr[strings.length];
            for (int i = 0; i < strings.length; i++) {
                IrIntExpr[] string = strings[i];
                if (string.length != 1) {
                    return new IrSortStrings(strings, strict, TrueFalseDomain);
                }
                array[i] = strings[i][0];
            }
            return strict ? sortStrict(array) : sort(array);
        }
        return new IrSortStrings(strings, strict, TrueFalseDomain);
    }

    public static IrBoolExpr sort(IrIntExpr[]... strings) {
        return sortStrings(strings, false);
    }

    public static IrBoolExpr sortStrict(IrIntExpr[]... strings) {
        return sortStrings(strings, true);
    }

    public static IrBoolExpr sortChannel(IrIntExpr[][] strings, IrIntExpr[] ints) {
        if (strings.length != ints.length) {
            throw new IllegalArgumentException();
        }
        for (int i = 1; i < strings.length; i++) {
            if (strings[0].length != strings[i].length) {
                throw new IllegalArgumentException();
            }
        }
        List<IrIntExpr[]> filterStrings = new ArrayList<>(strings.length);
        List<IrIntExpr> filterInts = new ArrayList<>(ints.length);
        for (int i = 0; i < ints.length; i++) {
            boolean equivalence = false;
            for (int j = i + 1; j < ints.length; j++) {
                if (IrUtil.Ordering.EQ.equals(IrUtil.compare(ints[i], ints[j]))
                        && IrUtil.Ordering.EQ.equals(IrUtil.compareString(strings[i], strings[j]))) {
                    equivalence = true;
                    break;
                }
            }
            if (!equivalence) {
                filterStrings.add(strings[i]);
                filterInts.add(ints[i]);
            }
        }
        assert !filterInts.isEmpty();
        if (filterInts.size() == 1) {
            return equal(filterInts.get(0), 0);
        }
        IrIntExpr[][] fstrings = filterStrings.toArray(new IrIntExpr[filterStrings.size()][]);
        IrIntExpr[] fints = filterInts.toArray(new IrIntExpr[filterInts.size()]);
        int[] constant = IrUtil.getConstant(fints);
        if (constant != null) {
            IrIntExpr[][] partialOrdering = new IrIntExpr[constant.length][];
            for (int i = 0; i < constant.length; i++) {
                int val = constant[i];
                if (val < 0 || val >= constant.length) {
                    return False;
                }
                if (partialOrdering[constant[i]] != null) {
                    // TODO
                    return new IrSortStringsChannel(fstrings, fints, TrueFalseDomain);
                }
                partialOrdering[constant[i]] = fstrings[i];
            }
            return sortStrict(partialOrdering);
        }
        return new IrSortStringsChannel(fstrings, fints, TrueFalseDomain);
    }

    public static IrBoolExpr allDifferent(IrIntExpr[] ints) {
        if (ints.length < 2) {
            return True;
        }
        if (ints.length == 2) {
            return notEqual(ints[0], ints[1]);
        }
        Domain domain = ints[0].getDomain();
        int size = ints[0].getDomain().size();
        for (int i = 1; i < ints.length; i++) {
            domain = domain.union(ints[i].getDomain());
            size += ints[i].getDomain().size();
            if (size != domain.size()) {
                return new IrAllDifferent(ints, TrueFalseDomain);
            }
        }
        return True;
    }

    public static IrBoolExpr selectN(IrBoolExpr[] bools, IrIntExpr n) {
        if (bools.length == 1) {
            if (bools[0].equals(n)) {
                return True;
            }
        }
        if (n.getDomain().getLowBound() > bools.length
                || n.getDomain().getHighBound() < 0) {
            return False;
        }
        boolean entailed = true;
        Domain nDomain = n.getDomain();
        for (int i = 0; i < bools.length; i++) {
            if (bools[i].getDomain().isTrue()) {
                if (i >= nDomain.getHighBound()) {
                    return False;
                }
                if (i >= nDomain.getLowBound()) {
                    entailed = false;
                }
            } else if (bools[i].getDomain().isFalse()) {
                if (i < nDomain.getLowBound()) {
                    return False;
                }
                if (i < nDomain.getHighBound()) {
                    entailed = false;
                }
            } else {
                entailed = false;
            }
        }
        if (entailed
                && n.getDomain().getLowBound() >= 0
                && n.getDomain().getHighBound() <= bools.length) {
            return True;
        }
        if (n.isConstant()) {
            int constant = n.getLowBound();
            IrBoolExpr[] ands = new IrBoolExpr[bools.length];
            System.arraycopy(bools, 0, ands, 0, constant);
            for (int i = constant; i < bools.length; i++) {
                ands[i] = not(bools[i]);
            }
            return and(ands);
        }
        return new IrSelectN(bools, n, TrueFalseDomain);
    }

    public static IrBoolExpr acyclic(IrIntExpr[] edges) {
        if (edges.length == 0) {
            return True;
        }
        return new IrAcyclic(edges, TrueFalseDomain);
    }

    public static IrBoolExpr unreachable(IrIntExpr[] edges, int from, int to) {
        return new IrUnreachable(edges, from, to, TrueFalseDomain);
    }

    public static IrBoolExpr connected(IrSetExpr nodes, IrSetArrayExpr relation, boolean directed) {
        return new IrConnected(nodes, relation, directed, TrueFalseDomain);
    }

    /*
     * TODO STRING
     */
    public static IrBoolExpr prefix(IrStringExpr prefix, IrStringExpr word) {
        if (prefix.getLength().getHighBound() == 0) {
            return True;
        }
        if (prefix.getLength().getLowBound() >= word.getLength().getHighBound()) {
            return equal(prefix, word);
        }
        return new IrPrefix(prefix, word, TrueFalseDomain);
    }

    /*
     * TODO STRING
     */
    public static IrBoolExpr suffix(IrStringExpr suffix, IrStringExpr word) {
        if (suffix.getLength().getHighBound() == 0) {
            return True;
        }
        if (suffix.getLength().getLowBound() >= word.getLength().getHighBound()) {
            return equal(suffix, word);
        }
        return new IrSuffix(suffix, word, TrueFalseDomain);
    }
    /**
     *******************
     *
     * Integer
     *
     *******************
     */
    public static final IrIntVar Zero = False;
    public static final IrIntVar One = True;

    public static IrIntVar constant(int value) {
        switch (value) {
            case 0:
                return Zero;
            case 1:
                return One;
            default:
                return new IrIntVar(value);
        }
    }

    public static IrIntVar domainInt(String name, Domain domain) {
        switch (domain.size()) {
            case 1:
                return constant(domain.getLowBound());
            case 2:
                if (domain.getLowBound() == 0 && domain.getHighBound() == 1) {
                    return new IrBoolVar(name, TrueFalseDomain);
                }
            default:
                return new IrIntVar(name, domain);
        }
    }

    public static IrIntVar boundInt(String name, int low, int high) {
        return domainInt(name, boundDomain(low, high));
    }

    public static IrIntVar enumInt(String name, int... values) {
        return domainInt(name, enumDomain(values));
    }

    public static IrIntExpr minus(IrIntExpr expr) {
        if (expr.isConstant()) {
            return constant(-expr.getLowBound());
        }
        if (expr instanceof IrMinus) {
            IrMinus minus = (IrMinus) expr;
            return minus.getExpr();
        }
        return new IrMinus(expr, expr.getDomain().minus());

    }

    public static IrIntExpr card(IrSetExpr set) {
        if (set instanceof IrSetVar) {
            IrSetVar var = (IrSetVar) set;
            return var.getCardVar();
        } else if (set instanceof IrJoinFunction) {
            IrJoinFunction join = (IrJoinFunction) set;
            if (join.hasGlobalCardinality() && join.getGlobalCardinality().equals(1)) {
                return card(join.getTake());
            }
        } else if (set instanceof IrSetUnion) {
            IrSetUnion union = (IrSetUnion) set;
            if (union.isDisjoint()) {
                IrSetExpr[] operands = union.getOperands();
                IrIntExpr[] cards = new IrIntExpr[operands.length];
                for (int i = 0; i < cards.length; i++) {
                    cards[i] = card(operands[i]);
                }
                return add(cards);
            }
        } else if (set instanceof IrOffset) {
            IrOffset offset = (IrOffset) set;
            return card(offset.getSet());
        } else if (set instanceof IrSetTernary) {
            IrSetTernary ternary = (IrSetTernary) set;
            return ternary(ternary.getAntecedent(),
                    card(ternary.getConsequent()),
                    card(ternary.getAlternative()));
        }
        Domain domain = set.getCard();
        if (domain.isConstant()) {
            return constant(domain.getLowBound());
        }
        return new IrCard(set, domain);
    }

    public static IrIntExpr add(int addend1, IrIntExpr addend2) {
        if (addend1 == 0) {
            return addend2;
        }
        return add(constant(addend1), addend2);
    }

    public static IrIntExpr add(IrIntExpr addend1, int addend2) {
        if (addend2 == 0) {
            return addend1;
        }
        return add(addend1, constant(addend2));
    }

    public static IrIntExpr add(Collection<? extends IrIntExpr> addends) {
        return add(addends.toArray(new IrIntExpr[addends.size()]));
    }

    public static IrIntExpr add(IrIntExpr... addends) {
        int constants = 0;
        List<IrIntExpr> filter = new ArrayList<>(addends.length);
        for (IrIntExpr addend : addends) {
            if (addend instanceof IrAdd) {
                IrAdd add = (IrAdd) addend;
                // Invariant: No nested IrAdd or constants
                filter.addAll(Arrays.asList(add.getAddends()));
                constants += add.getOffset();
            } else {
                if (addend.isConstant()) {
                    constants += addend.getLowBound();
                } else {
                    filter.add(addend);
                }
            }
        }
        if (filter.isEmpty()) {
            return constant(constants);
        }
        if (filter.size() == 1) {
            IrIntExpr first = filter.get(0);
            if (constants == 0) {
                return first;
            }
            return new IrAdd(new IrIntExpr[]{first}, constants,
                    first.getDomain().offset(constants));
        }
        Domain domain = filter.stream().map(IrIntExpr::getDomain).reduce(
                constantDomain(constants), Domain::add);
        return new IrAdd(filter.toArray(new IrIntExpr[filter.size()]), constants, domain);
    }

    public static IrIntExpr sub(int minuend, IrIntExpr subtrahend) {
        if (minuend == 0) {
            return subtrahend;
        }
        return sub(constant(minuend), subtrahend);
    }

    public static IrIntExpr sub(IrIntExpr minuend, int subtrahend) {
        if (subtrahend == 0) {
            return minuend;
        }
        return sub(minuend, constant(subtrahend));
    }

    public static IrIntExpr sub(Collection<? extends IrIntExpr> subtrahends) {
        return sub(subtrahends.toArray(new IrIntExpr[subtrahends.size()]));
    }

    public static IrIntExpr sub(IrIntExpr... subtrahends) {
        if (subtrahends.length == 0) {
            return Zero;
        }
        IrIntExpr[] flip = new IrIntExpr[subtrahends.length];
        flip[0] = subtrahends[0];
        for (int i = 1; i < flip.length; i++) {
            flip[i] = minus(subtrahends[i]);
        }
        return add(flip);
    }

    public static IrIntExpr mul(int multiplicand, IrIntExpr multiplier, Domain intRange) {
        if (multiplicand == 1) {
            return multiplier;
        }
        return mul(constant(multiplicand), multiplier, intRange);
    }

    public static IrIntExpr mul(IrIntExpr multiplicand, int multiplier, Domain intRange) {
        if (multiplier == 1) {
            return multiplicand;
        }
        return mul(multiplicand, constant(multiplier), intRange);
    }

    private static int mulTwo(int a, int b) {
        long ma = a;
        long mb = b;
        long mul = ma * mb;
        if (mul > Integer.MAX_VALUE) {
            return Integer.MAX_VALUE;
        } else if (mul < Integer.MIN_VALUE) {
            return Integer.MIN_VALUE;
        }
        return (int) mul;
    }

    public static IrIntExpr mul(Collection<? extends IrIntExpr> multiplicands, Domain intRange) {
        return mul(multiplicands.toArray(new IrIntExpr[multiplicands.size()]), intRange);
    }

    public static IrIntExpr mul(IrIntExpr multiplicand, IrIntExpr multiplier, Domain intRange) {
        if (multiplicand.isConstant()) {
            switch (multiplicand.getLowBound()) {
                case -1:
                    return minus(multiplier);
                case 0:
                    return multiplicand;
                case 1:
                    return multiplier;
            }
        }
        if (multiplier.isConstant()) {
            switch (multiplier.getLowBound()) {
                case -1:
                    return minus(multiplicand);
                case 0:
                    return multiplier;
                case 1:
                    return multiplicand;
            }
        }
        if (multiplicand.isConstant() && multiplier.isConstant()) {
            return constant(multiplicand.getLowBound() * multiplier.getLowBound());
        }
        Domain domain;
        long m0 = multiplicand.getDomain().size();
        long m1 = multiplier.getDomain().size();
        if (m0 * m1 <= 16 || m0 * m1 <= intRange.size()) {
            TIntSet domainSet = new TIntHashSet();
            PrimitiveIterator.OfInt iter0 = multiplicand.getDomain().iterator();
            // TODO: implement domain.mul
            while (iter0.hasNext()) {
                long i0 = iter0.next();
                PrimitiveIterator.OfInt iter1 = multiplier.getDomain().iterator();
                while (iter1.hasNext()) {
                    long i1 = iter1.next();
                    long mul = i0 * i1;
                    int mulI = (int) mul;
                    if (mul == mulI) {
                        domainSet.add(mulI);
                    }
                }
            }
            domain = enumDomain(domainSet);
        } else {
            domain = mulBoundDomain(multiplicand.getDomain(), multiplier.getDomain()).intersection(intRange);
        }
        if (domain.isEmpty()) {
            throw new UnsatisfiableException();
        }
        return new IrMul(multiplicand, multiplier, intRange, domain);
    }

    public static Domain mulBoundDomain(Domain multiplicandDomain, Domain multiplierDomain) {
        int low1 = multiplicandDomain.getLowBound();
        int high1 = multiplicandDomain.getHighBound();
        int low2 = multiplierDomain.getLowBound();
        int high2 = multiplierDomain.getHighBound();
        int ll = mulTwo(low1, low2);
        int lh = mulTwo(low1, high2);
        int hl = mulTwo(high1, low2);
        int hh = mulTwo(high1, high2);
        return boundDomain(
                Util.min(ll, lh, hl, hh),
                Util.max(ll, lh, hl, hh));
    }

    public static IrIntExpr mul(IrIntExpr[] multiplicands, Domain intRange) {
        if (multiplicands.length == 0) {
            return One;
        }
        IrIntExpr product = multiplicands[0];
        for (int i = 1; i < multiplicands.length; i++) {
            product = mul(product, multiplicands[i], intRange);
        }
        return product;
    }

    public static IrIntExpr div(int dividend, IrIntExpr divisor) {
        return div(constant(dividend), divisor);
    }

    public static IrIntExpr div(IrIntExpr dividend, int divisor) {
        if (divisor == 1) {
            return dividend;
        }
        return div(dividend, constant(divisor));
    }

    public static IrIntExpr div(IrIntExpr dividend, IrIntExpr divisor) {
        if (dividend.isConstant() && dividend.getLowBound() == 0) {
            return dividend;
        }
        if (divisor.isConstant() && divisor.getLowBound() == 1) {
            return dividend;
        }
        if (dividend.isConstant() && divisor.isConstant()) {
            if (divisor.getLowBound() == 0) {
                throw new UnsatisfiableException();
            }
            return constant(dividend.getLowBound() / divisor.getLowBound());
        }
        int low = dividend.getDomain().getLowBound();
        int high = dividend.getDomain().getHighBound();
        int min = Util.min(low, -low, high, -high);
        int max = Util.max(low, -low, high, -high);
        return new IrDiv(dividend, divisor, boundDomain(min, max));
    }

    public static IrIntExpr mod(IrIntExpr dividend, IrIntExpr divisor) {
        if (dividend.isConstant() && dividend.getLowBound() == 0) {
            return dividend;
        }
        if (dividend.isConstant() && divisor.isConstant()) {
            if (divisor.getLowBound() == 0) {
                throw new UnsatisfiableException();
            }
            return constant(dividend.getLowBound() % divisor.getLowBound());
        }
        int low = divisor.getDomain().getLowBound();
        int high = divisor.getDomain().getHighBound();
        int min = Math.min(-Math.abs(low), -Math.abs(high));
        int max = Math.max(Math.abs(low), Math.abs(high));
        return new IrMod(dividend, divisor, boundDomain(min, max));
    }

    public static IrIntExpr element(IrIntExpr[] array, IrIntExpr index) {
        return element(array(array), index);
    }

    public static IrIntExpr element(IrIntArrayExpr array, IrIntExpr index) {
        IrIntArrayExpr $array = subArray(array, 0, index.getDomain().getHighBound() + 1);

        if (index.isConstant()) {
            return get($array, index.getLowBound());
        }

        Optional<Domain> domain = Util.mapWithin(
                index.getDomain().stream(), $array.getDomains())
                .reduce(Domain::union);
        assert domain.isPresent();
        return IrUtil.asConstant(new IrElement($array, index, domain.get()));
    }

    public static IrIntExpr count(int value, IrIntArrayExpr array) {
        List<IrIntExpr> filter = new ArrayList<>();
        int count = 0;
        for (IrIntExpr i : IrUtil.asArray(array)) {
            if (i.equals(constant(value))) {
                count++;
            } else if (i.getDomain().contains(value)) {
                filter.add(i);
            }
        }
        switch (filter.size()) {
            case 0:
                return constant(count);
            case 1:
                return add(equal(value, filter.get(0)), count);
            default:
                return add(
                        new IrCount(value,
                                array.length() == filter.size() ? array : intArray(filter),
                                boundDomain(0, filter.size())),
                        count);
        }
    }

    public static IrIntExpr countNotEqual(int value, IrIntArrayExpr array) {
        return sub(array.length(), count(value, array));
    }

    public static IrIntExpr max(IrSetExpr set, int defaultValue) {
        if (set.getCard().getHighBound() == 0) {
            return constant(defaultValue);
        }
        if (set instanceof IrSingleton) {
            IrSingleton singleton = (IrSingleton) set;
            return singleton.getValue();
        }
        Domain domain = set.getEnv();
        if (!set.getKer().isEmpty()) {
            domain = domain.boundLow(set.getKer().getHighBound());
        }
        return IrUtil.asConstant(new IrSetMax(set, defaultValue, set.getCard().getLowBound() > 0
                ? domain : domain.insert(defaultValue)));
    }

    public static IrIntExpr min(IrSetExpr set, int defaultValue) {
        if (set.getCard().getHighBound() == 0) {
            return constant(defaultValue);
        }
        if (set instanceof IrSingleton) {
            IrSingleton singleton = (IrSingleton) set;
            return singleton.getValue();
        }
        Domain domain = set.getEnv();
        if (!set.getKer().isEmpty()) {
            domain = domain.boundHigh(set.getKer().getLowBound());
        }
        return IrUtil.asConstant(new IrSetMin(set, defaultValue, set.getCard().getLowBound() > 0
                ? domain : domain.insert(defaultValue)));
    }

    public static IrIntExpr sum(IrSetExpr set) {
        if (set.getCard().getHighBound() == 0) {
            return Zero;
        }
        if (set.getCard().getHighBound() == 1) {
            return min(set, 0);
        }
        if (set instanceof IrSingleton) {
            IrSingleton singleton = (IrSingleton) set;
            return singleton.getValue();
        }

        int sum = set.getKer().stream().sum();
        int count = set.getKer().size();

        // Calculate low
        int low = sum;
        int lowCount = count;
        PrimitiveIterator.OfInt envIter = set.getEnv().iterator();
        while (lowCount < set.getCard().getHighBound() && envIter.hasNext()) {
            int env = envIter.next();
            if (env >= 0 && lowCount >= set.getCard().getLowBound()) {
                break;
            }
            if (!set.getKer().contains(env)) {
                low += env;
                lowCount++;
            }
        }

        // Calculate high
        int high = sum;
        int highCount = count;
        envIter = set.getEnv().iterator(false);
        while (highCount < set.getCard().getHighBound() && envIter.hasNext()) {
            int env = envIter.next();
            if (env <= 0 && highCount >= set.getCard().getLowBound()) {
                break;
            }
            if (!set.getKer().contains(env)) {
                high += env;
                highCount++;
            }
        }

        return new IrSetSum(set, boundDomain(low, high));
    }

    public static IrIntExpr ternary(IrBoolExpr antecedent, IrIntExpr consequent, IrIntExpr alternative) {
        if (antecedent.getDomain().isTrue()) {
            return consequent;
        }
        if (antecedent.getDomain().isFalse()) {
            return alternative;
        }
        if (consequent.equals(alternative)) {
            return consequent;
        }
        Domain domain = consequent.getDomain().union(alternative.getDomain());
        return new IrTernary(antecedent, consequent, alternative, domain);
    }

    public static IrIntExpr length(IrStringExpr string) {
        if (string instanceof IrStringVar) {
            return ((IrStringVar) string).getLengthVar();
        }
        return new IrLength(string, string.getLength());
    }

    /**
     *******************
     *
     * Set
     *
     *******************
     */
    public static final IrSetVar EmptySet = new IrSetVar(EmptyDomain);

    public static IrSetVar set(String name, int lowEnv, int highEnv) {
        return set(name, boundDomain(lowEnv, highEnv));
    }

    public static IrSetVar set(String name, int lowEnv, int highEnv, int lowKer, int highKer) {
        return set(name, boundDomain(lowEnv, highEnv), boundDomain(lowKer, highKer));
    }

    public static IrSetVar set(String name, int lowEnv, int highEnv, int[] ker) {
        return set(name, boundDomain(lowEnv, highEnv), enumDomain(ker));
    }

    public static IrSetVar set(String name, int[] env) {
        return set(name, enumDomain(env));
    }

    public static IrSetVar set(String name, int[] env, int lowKer, int highKer) {
        return set(name, enumDomain(env), boundDomain(lowKer, highKer));
    }

    public static IrSetVar set(String name, int[] env, int[] ker) {
        return set(name, enumDomain(env), enumDomain(ker));
    }

    public static IrSetVar set(String name, Domain env) {
        return set(name, env, EmptyDomain);
    }

    public static IrSetVar set(String name, Domain env, Domain ker) {
        return set(name, env, ker, boundDomain(ker.size(), env.size()));
    }

    public static IrSetVar set(String name, Domain env, Domain ker, Domain card) {
        return set(name, env, ker, domainInt("|" + name + "|", card));
    }

    public static IrSetVar set(String name, Domain env, Domain ker, IrIntVar card) {
        IrSetVar var = new IrSetVar(name, env, ker, card);
        if (var.isConstant()) {
            return constant(var.getKer());
        }
        return var;
    }

    public static IrSetVar constant(int[] value) {
        return constant(enumDomain(value));
    }

    public static IrSetVar constant(TIntCollection value) {
        return constant(enumDomain(value));
    }

    public static IrSetVar constant(TIntSet value) {
        return constant(enumDomain(value));
    }

    public static IrSetVar constant(Domain value) {
        if (value.isEmpty()) {
            return EmptySet;
        }
        return new IrSetVar(value);
    }

    public static IrSetExpr singleton(IrIntExpr value) {
        if (value.isConstant()) {
            return constant(constantDomain(value.getLowBound()));
        }
        return new IrSingleton(value, value.getDomain(), EmptyDomain);
    }

    public static IrSetExpr singletonFilter(IrIntExpr value, int filter) {
        if (!value.getDomain().contains(filter)) {
            return singleton(value);
        }
        if (value.isConstant()) {
            return value.getLowBound() == filter ? EmptySet : singleton(value);
        }
        return new IrSingletonFilter(value, filter, value.getDomain().remove(filter), EmptyDomain, ZeroOneDomain);
    }

    public static IrSetExpr arrayToSet(IrIntExpr[] array, Integer globalCardinality) {
        switch (array.length) {
            case 0:
                return EmptySet;
            case 1:
                return singleton(array[0]);
            default:
                Domain env = array[0].getDomain();
                for (int i = 1; i < array.length; i++) {
                    env = env.union(array[i].getDomain());
                }
                TIntSet values = new TIntHashSet();
                for (IrIntExpr i : array) {
                    if (i.isConstant()) {
                        values.add(i.getLowBound());
                    }
                }
                Domain ker = enumDomain(values);
                int lowCard = Math.max(
                        globalCardinality == null ? 1 : divRoundUp(array.length, globalCardinality),
                        ker.size());
                int highCard = Math.min(array.length, env.size());
                if (lowCard > highCard) {
                    assert globalCardinality > 0;
                    throw new UnsatisfiableException();
                }
                Domain card = boundDomain(lowCard, highCard);
                return IrUtil.asConstant(new IrArrayToSet(array, env, ker, card, globalCardinality));
        }
    }

//    public static IrSetExpr element(IrSetExpr[] array, IrIntExpr index) {
//        IrSetExpr[] $array = index.getDomain().getHighBound() + 1 < array.length
//                ? Arrays.copyOf(array, index.getDomain().getHighBound() + 1)
//                : array.clone();
//        for (int i = 0; i < $array.length; i++) {
//            if (!index.getDomain().contains(i)) {
//                $array[i] = EmptySet;
//            }
//        }
//
//        Integer constant = IrUtil.getConstant(index);
//        if (constant != null) {
//            return $array[constant];
//        }
//        TIntIterator iter = index.getDomain().iterator();
//        assert iter.hasNext();
//
//        int val = iter.next();
//        Domain env = $array[val].getEnv();
//        Domain ker = $array[val].getKer();
//        Domain card = $array[val].getCard();
//        while (iter.hasNext()) {
//            val = iter.next();
//            if (val < $array.length) {
//                env = env.union($array[val].getEnv());
//                ker = ker.intersection($array[val].getKer());
//                card = card.union($array[val].getCard());
//            }
//        }
//        return new IrSetElement($array, index, env, ker, card);
//    }
    public static IrSetExpr element(IrSetArrayExpr array, IrIntExpr index) {
        if (index.isConstant()) {
            return get(array, index.getLowBound());
        }

        // TODO bound for other element calls
        PrimitiveIterator.OfInt iter = index.getDomain().boundBetween(0, array.length() - 1).iterator();
        assert iter.hasNext();

        int val = iter.next();
        Domain env = array.getEnvs()[val];
        Domain ker = array.getKers()[val];
        Domain card = array.getCards()[val];
        while (iter.hasNext()) {
            val = iter.next();
            env = env.union(array.getEnvs()[val]);
            ker = ker.intersection(array.getKers()[val]);
            card = card.union(array.getCards()[val]);
        }
        return IrUtil.asConstant(new IrSetElement(array, index, env, ker, card));
    }

    /**
     * Relational join.
     *
     * Union{for all i in take} children[i]
     *
     * @param take
     * @param children
     * @param injective
     * @return the join expression take.children
     */
    public static IrSetExpr joinRelation(IrSetExpr take, IrSetArrayExpr children, boolean injective) {
        if (take.getEnv().isEmpty()) {
            return EmptySet;
        }
        IrSetArrayExpr $children = subArray(children, 0, take.getEnv().getHighBound() + 1);

        IrIntExpr[] ints = IrUtil.asInts(children);
        if (ints != null) {
            return joinFunction(take, ints, injective ? 1 : null);
        }

        if (take.isConstant()) {
            int[] array = take.getKer().getValues();
            IrSetExpr[] to = new IrSetExpr[array.length];
            for (int i = 0; i < to.length; i++) {
                // TODO optimize
                to[i] = get($children, array[i]);
            }
            return union(to, injective);
        }

        if (take instanceof IrSingleton) {
            IrSingleton singleton = (IrSingleton) take;
            return element(children, singleton.getValue());
        }

        Domain takeEnv = take.getEnv();
        Domain takeKer = take.getKer();

        // Compute env
        Domain env = Util.map(
                takeEnv.stream(), $children.getEnvs())
                .reduce(Domain::union).orElse(EmptyDomain);

        // Compute ker
        Domain ker = Util.map(
                takeKer.stream(), $children.getKers())
                .reduce(Domain::union).orElse(EmptyDomain);

        // Compute card
        Domain takeCard = take.getCard();
        int index = 0;
        int[] childrenLowCards = new int[takeEnv.size() - takeKer.size()];
        int[] childrenHighCards = new int[takeEnv.size() - takeKer.size()];
        int cardLow = 0, cardHigh = 0;

        PrimitiveIterator.OfInt iter = takeEnv.iterator();
        while (iter.hasNext()) {
            int val = iter.next();
            Domain childDomain = $children.getCards()[val];
            if (takeKer.contains(val)) {
                cardLow = injective
                        ? cardLow + childDomain.getLowBound()
                        : Math.max(cardLow, childDomain.getLowBound());
                cardHigh += childDomain.getHighBound();
            } else {
                childrenLowCards[index] = childDomain.getLowBound();
                childrenHighCards[index] = childDomain.getHighBound();
                index++;
            }
        }
        assert index == childrenLowCards.length;
        assert index == childrenHighCards.length;

        Arrays.sort(childrenLowCards);
        Arrays.sort(childrenHighCards);

        for (int i = 0; i < takeCard.getLowBound() - takeKer.size(); i++) {
            cardLow = injective
                    ? cardLow + childrenLowCards[i]
                    : Math.max(cardLow, childrenLowCards[i]);
        }
        for (int i = 0; i < takeCard.getHighBound() - takeKer.size(); i++) {
            cardHigh += childrenHighCards[childrenHighCards.length - 1 - i];
        }
        cardLow = Math.max(cardLow, ker.size());
        cardHigh = Math.min(cardHigh, env.size());
        Domain card = boundDomain(cardLow, cardHigh);

        return IrUtil.asConstant(new IrJoinRelation(take, $children, env, ker, card, injective));
    }

    public static IrSetExpr joinFunction(IrSetExpr take, IrIntExpr[] refs, Integer globalCardinality) {
        return joinFunction(take, array(refs), globalCardinality);
    }

    private static boolean isIdentityRelation(IrIntArrayExpr refs) {
        for (int i = 0; i < refs.length(); i++) {
            Domain domain = refs.getDomains()[i];
            if (!domain.isConstant() || domain.getLowBound() != i) {
                return false;
            }
        }
        return true;
    }

    public static IrSetExpr joinFunction(IrSetExpr take, IrIntArrayExpr refs, Integer globalCardinality) {
        if (take.getEnv().isEmpty()) {
            return EmptySet;
        }
        IrIntArrayExpr $refs = subArray(refs, 0, take.getEnv().getHighBound() + 1);

        if (take.isConstant()) {
            int[] array = take.getKer().getValues();
            IrIntExpr[] to = new IrIntExpr[array.length];
            for (int i = 0; i < to.length; i++) {
                to[i] = get($refs, array[i]);
            }
            return arrayToSet(to, globalCardinality);
        }

        if (isIdentityRelation($refs)) {
            return take;
        }

        if (take instanceof IrSingleton) {
            IrSingleton singleton = (IrSingleton) take;
            return singleton(element(refs, singleton.getValue()));
        }

        // Compute env
        Domain env = Util.map(
                take.getEnv().stream(), refs.getDomains())
                .reduce(Domain::union).orElse(EmptyDomain);

        // Compute ker
        Domain ker = Util.map(
                take.getKer().stream(), refs.getDomains())
                .filter(Domain::isConstant).reduce(Domain::union).orElse(EmptyDomain);

        // Compute card
        Domain takeCard = take.getCard();
        int lowTakeCard = takeCard.getLowBound();
        int highTakeCard = takeCard.getHighBound();
        Domain card;
        if (globalCardinality == null) {
            card = lowTakeCard == 0
                    ? boundDomain(Math.max(0, ker.size()), Math.min(highTakeCard, env.size()))
                    : boundDomain(Math.max(1, ker.size()), Math.min(highTakeCard, env.size()));
        } else {
            card = boundDomain(
                    divRoundUp(Math.max(lowTakeCard, ker.size()), globalCardinality),
                    Math.min(highTakeCard, env.size()));
        }

        return IrUtil.asConstant(new IrJoinFunction(take, $refs, env, ker, card, globalCardinality));
    }

    private static int divRoundUp(int a, int b) {
        assert a >= 0;
        assert b > 0;

        return (a + b - 1) / b;
    }

    public static IrSetExpr difference(IrSetExpr minuend, IrSetExpr subtrahend) {
        Domain env = minuend.getEnv().difference(subtrahend.getKer());
        Domain ker = minuend.getKer().difference(subtrahend.getEnv());
        int low = Math.max(0, minuend.getCard().getLowBound() - subtrahend.getCard().getHighBound());
        int high = minuend.getCard().getHighBound();
        Domain card = boundDomain(Math.max(low, ker.size()), Math.min(high, env.size()));
        return new IrSetDifference(minuend, subtrahend, env, ker, card);
    }

    public static IrSetExpr intersection(IrSetExpr... operands) {
        List<IrSetExpr> flatten = new ArrayList<>(operands.length);
        for (IrSetExpr operand : operands) {
            if (operand instanceof IrSetIntersection) {
                // Invariant: No nested IrSetIntersection
                flatten.addAll(Arrays.asList(((IrSetIntersection) operand).getOperands()));
            } else {
                flatten.add(operand);
            }
        }
        Domain constants = null;
        List<IrSetExpr> filter = new ArrayList<>();
        for (IrSetExpr operand : flatten) {
            if (operand.isConstant()) {
                if (constants == null) {
                    constants = operand.getKer();
                } else {
                    constants = constants.intersection(operand.getKer());
                }
            } else {
                filter.add(operand);
            }
        }
        if (constants != null) {
            filter.add(constant(constants));
        }
        IrSetExpr[] ops = filter.toArray(new IrSetExpr[filter.size()]);
        switch (ops.length) {
            case 0:
                return EmptySet;
            case 1:
                return ops[0];
            default:
                Domain env = ops[0].getEnv();
                Domain ker = ops[0].getKer();
                int low = 0;
                int high = ops[0].getCard().getHighBound();
                for (int i = 1; i < ops.length; i++) {
                    env = env.intersection(ops[i].getEnv());
                    ker = ker.intersection(ops[i].getKer());
                    high = Math.max(high, ops[i].getCard().getHighBound());
                }
                Domain card = boundDomain(
                        Math.max(low, ker.size()),
                        Math.min(high, env.size()));
                return new IrSetIntersection(ops, env, ker, card);
        }
    }

    public static IrSetExpr union(IrSetExpr... operands) {
        return union(operands, false);
    }

    public static IrSetExpr unionDisjoint(IrSetExpr... operands) {
        return union(operands, true);
    }

    public static IrSetExpr union(IrSetExpr[] operands, boolean disjoint) {
        List<IrSetExpr> flatten = new ArrayList<>(operands.length);
        for (IrSetExpr operand : operands) {
            if (operand instanceof IrSetUnion) {
                // Invariant: No nested IrSetUnion
                flatten.addAll(Arrays.asList(((IrSetUnion) operand).getOperands()));
            } else {
                flatten.add(operand);
            }
        }
        Domain constants = Domains.EmptyDomain;
        List<IrSetExpr> filter = new ArrayList<>();
        for (IrSetExpr operand : flatten) {
            if (operand.isConstant()) {
                constants = constants.union(operand.getKer());
            } else {
                filter.add(operand);
            }
        }
        if (!constants.isEmpty()) {
            filter.add(constant(constants));
        }
        IrSetExpr[] ops = filter.toArray(new IrSetExpr[filter.size()]);
        switch (ops.length) {
            case 0:
                return EmptySet;
            case 1:
                return ops[0];
            default:
                Domain env = ops[0].getEnv();
                Domain ker = ops[0].getKer();
                Domain operandCard = ops[0].getCard();
                int low = operandCard.getLowBound();
                int high = operandCard.getHighBound();
                for (int i = 1; i < ops.length; i++) {
                    env = env.union(ops[i].getEnv());
                    ker = ker.union(ops[i].getKer());
                    operandCard = ops[i].getCard();
                    low = disjoint
                            ? low + operandCard.getLowBound()
                            : Math.max(low, operandCard.getLowBound());
                    high += operandCard.getHighBound();
                }
                int cardLow = Math.max(low, ker.size());
                int cardHigh = Math.min(high, env.size());
                if (cardLow > cardHigh) {
                    throw new IllegalSetException();
                }
                Domain card = boundDomain(cardLow, cardHigh);
                return IrUtil.asConstant(new IrSetUnion(ops, env, ker, card, disjoint));
        }
    }

    public static IrSetExpr offset(IrSetExpr set, int offset) {
        if (offset == 0) {
            return set;
        }
        if (set instanceof IrOffset) {
            IrOffset nested = (IrOffset) set;
            return offset(nested.getSet(), offset + nested.getOffset());
        }
        Domain env = set.getEnv().offset(offset);
        Domain ker = set.getKer().offset(offset);
        Domain card = set.getCard();
        return IrUtil.asConstant(new IrOffset(set, offset, env, ker, card));
    }

    public static IrSetExpr mask(IrSetExpr set, int from, int to) {
        if (from > to) {
            throw new IllegalArgumentException();
        }
        if (from == to) {
            return EmptySet;
        }
        if (from <= set.getEnv().getLowBound() && to > set.getEnv().getHighBound()) {
            return offset(set, -from);
        }
        Domain env = set.getEnv().offset(-from).boundBetween(0, to - from - 1);
        Domain ker = set.getKer().offset(-from).boundBetween(0, to - from - 1);
        Domain card = boundDomain(ker.size(), Math.min(env.size(), set.getCard().getHighBound()));
        return IrUtil.asConstant(new IrMask(set, from, to, env, ker, card));
    }

    public static IrSetExpr ternary(IrBoolExpr antecedent, IrSetExpr consequent, IrSetExpr alternative) {
        if (antecedent.getDomain().isTrue()) {
            return consequent;
        }
        if (antecedent.getDomain().isFalse()) {
            return alternative;
        }
        if (consequent.equals(alternative)) {
            return consequent;
        }
        Domain env = consequent.getEnv().union(alternative.getEnv());
        Domain ker = consequent.getKer().intersection(alternative.getKer());
        Domain card = consequent.getCard().union(alternative.getCard());
        return new IrSetTernary(antecedent, consequent, alternative, env, ker, card);
    }

    public static IrSetExpr containsTernary(IrSetExpr antecedent, int x, IrSetExpr consequent) {
        if (antecedent.getKer().contains(x)) {
            return consequent;
        }
        if (!antecedent.getEnv().contains(x)) {
            return EmptySet;
        }
        return new IrContainsSetTernary(antecedent, x, consequent, consequent.getEnv(), EmptyDomain, consequent.getCard().insert(0));
    }

    /**
     *******************
     *
     * Array
     *
     *******************
     */
    public static IrIntArrayExpr array(IrIntExpr... array) {
        return new IrIntArrayVar(array);
    }

    public static IrIntArrayExpr intArray(List<IrIntExpr> array) {
        return new IrIntArrayVar(array.toArray(new IrIntExpr[array.size()]));
    }

    public static IrSetArrayExpr array(IrSetExpr... array) {
        return new IrSetArrayVar(array);
    }

    public static IrSetArrayExpr setArray(List<IrSetExpr> array) {
        return new IrSetArrayVar(array.toArray(new IrSetExpr[array.size()]));
    }

    public static IrIntExpr get(IrIntArrayExpr expr, int index) {
        if (expr instanceof IrIntArrayVar) {
            IrIntArrayVar var = (IrIntArrayVar) expr;
            return var.getArray()[index];
        }
        return IrUtil.asConstant(new IrElement(expr, constant(index), expr.getDomains()[index]));
    }

    public static IrSetExpr get(IrSetArrayExpr expr, int index) {
        if (expr instanceof IrSetArrayVar) {
            IrSetArrayVar var = (IrSetArrayVar) expr;
            return var.getArray()[index];
        }
        return IrUtil.asConstant(new IrSetElement(expr, constant(index), expr.getEnvs()[index], expr.getKers()[index], expr.getCards()[index]));
    }

    public static IrIntArrayExpr subArray(IrIntArrayExpr expr, int from, int to) {
        if (from == 0 && to == expr.length()) {
            return expr;
        }
        IrIntExpr[] array = new IrIntExpr[to - from];
        for (int i = from; i < to; i++) {
            array[i - from] = get(expr, i);
        }
        return array(array);
    }

    public static IrSetArrayExpr subArray(IrSetArrayExpr expr, int from, int to) {
        if (from == 0 && to == expr.length()) {
            return expr;
        }
        IrSetExpr[] array = new IrSetExpr[to - from];
        for (int i = from; i < to; i++) {
            array[i - from] = get(expr, i);
        }
        return array(array);
    }

    public static IrSetArrayExpr cons(IrSetExpr left, IrSetArrayExpr right) {
        IrSetExpr[] array = new IrSetExpr[1 + right.length()];
        array[0] = left;
        for (int i = 0; i < right.length(); i++) {
            array[1 + i] = get(right, i);
        }
        return array(array);
    }

    public static IrIntArrayExpr snoc(IrIntArrayExpr left, IrIntExpr right) {
        IrIntExpr[] array = new IrIntExpr[left.length() + 1];
        for (int i = 0; i < left.length(); i++) {
            array[i] = get(left, i);
        }
        array[left.length()] = right;
        return array(array);
    }

    public static IrSetArrayExpr snoc(IrSetArrayExpr left, IrSetExpr right) {
        IrSetExpr[] array = new IrSetExpr[left.length() + 1];
        for (int i = 0; i < left.length(); i++) {
            array[i] = get(left, i);
        }
        array[left.length()] = right;
        return array(array);
    }

    public static IrSetArrayExpr concat(IrSetArrayExpr left, IrSetArrayExpr right) {
        IrSetExpr[] array = new IrSetExpr[left.length() + right.length()];
        for (int i = 0; i < left.length(); i++) {
            array[i] = get(left, i);
        }
        for (int i = 0; i < right.length(); i++) {
            array[left.length() + i] = get(right, i);
        }
        return array(array);
    }

    public static IrSetArrayExpr filterNotEqual(IrIntArrayExpr expr, int value) {
        IrSetExpr[] array = new IrSetExpr[expr.length()];
        for (int i = 0; i < array.length; i++) {
            IrIntExpr element = get(expr, i);
            array[i] = singletonFilter(element, value);
        }
        return array(array);
    }

    public static IrSetArrayExpr domainRestriction(IrSetExpr domain, IrSetArrayExpr relation) {
        IrSetExpr[] array = new IrSetExpr[relation.length()];
        for (int i = 0; i < array.length; i++) {
            array[i] = containsTernary(domain, i, get(relation, i));
        }
        return array(array);
    }

    public static IrSetArrayExpr rangeRestriction(IrSetArrayExpr relation, IrSetExpr range) {
        IrSetExpr[] array = new IrSetExpr[relation.length()];
        for (int i = 0; i < array.length; i++) {
            array[i] = intersection(get(relation, i), range);
        }
        return array(array);
    }

    public static IrSetArrayExpr inverse(IrSetArrayExpr relation, int length) {
        TIntSet[] envs = new TIntSet[length];
        TIntSet[] kers = new TIntSet[length];
        for (int i = 0; i < length; i++) {
            envs[i] = new TIntHashSet();
            kers[i] = new TIntHashSet();
        }
        for (int i = 0; i < relation.length(); i++) {
            PrimitiveIterator.OfInt iter = relation.getEnvs()[i].iterator();
            while (iter.hasNext()) {
                int val = iter.next();
                if (val < envs.length) {
                    envs[val].add(i);
                }
            }
            iter = relation.getKers()[i].iterator();
            while (iter.hasNext()) {
                int val = iter.next();
                if (val < kers.length) {
                    kers[val].add(i);
                }
            }
        }
        Domain[] cards = new Domain[length];
        for (int i = 0; i < cards.length; i++) {
            cards[i] = boundDomain(kers[i].size(), envs[i].size());
        }
        return new IrInverse(relation, enumDomains(envs), enumDomains(kers), cards);
    }

    public static IrSetArrayExpr transitiveClosure(IrSetArrayExpr relation, boolean reflexive) {
        int n = relation.length();
        Domain[] envs = new Domain[n];
        Domain[] kers = new Domain[n];
        Domain[] cards = new Domain[n];

        for (int i = 0; i < n; i++) {
            TIntStack q = new TIntArrayStack(n);
            q.push(i);
            TIntSet env = new TIntHashSet(relation.getEnvs()[i].getValues());
            if (reflexive) {
                env.add(i);
            }
            do {
                relation.getEnvs()[q.pop()].stream()
                        .filter(env::add).forEach(q::push);
            } while (q.size() > 0);

            q.push(i);
            TIntSet ker = new TIntHashSet(relation.getKers()[i].getValues());
            if (reflexive) {
                ker.add(i);
            }
            do {
                relation.getKers()[q.pop()].stream()
                        .filter(ker::add).forEach(q::push);
            } while (q.size() > 0);

            envs[i] = enumDomain(env);
            kers[i] = enumDomain(ker);
            cards[i] = boundDomain(kers[i].size(), envs[i].size());
        }
        return new IrTransitiveClosure(relation, reflexive, envs, kers, cards);
    }

    /**
     *******************
     *
     * String
     *
     *******************
     */
    public static final IrStringVar EmptyString = new IrStringVar("", new IrIntVar[0], Zero);

    public static IrStringVar constant(String value) {
        if (value.length() == 0) {
            return EmptyString;
        }
        IrIntVar[] chars = new IrIntVar[value.length()];
        for (int i = 0; i < chars.length; i++) {
            chars[i] = constant(value.charAt(i));
        }
        IrIntVar length = constant(value.length());
        return new IrStringVar(value, chars, length);
    }

    public static IrStringVar string(String name, IrIntVar[] chars, IrIntVar length) {
        if (length.isConstant()) {
            char[] string = new char[chars.length];
            for (int i = 0; string != null && i < string.length; i++) {
                if (chars[i].isConstant()) {
                    string[i] = (char) chars[i].getLowBound();
                } else {
                    string = null;
                }
            }
            if (string != null) {
                return constant(new String(string, 0, length.getLowBound()));
            }
        }
        int lengthHigh = length.getDomain().getHighBound();
        if (lengthHigh < 0) {
            throw new IllegalArgumentException();
        }
        int maxLength = chars.length - 1;
        while (maxLength >= lengthHigh && Zero.equals(chars[maxLength])) {
            maxLength--;
        }
        if (maxLength + 1 < chars.length) {
            chars = Arrays.copyOf(chars, maxLength + 1);
        }
        return new IrStringVar(name, chars, length);
    }

    public static IrStringVar string(String name, Domain charDomain, int maxLength) {
        Domain domain = charDomain.insert(0);
        IrIntVar[] chars = new IrIntVar[maxLength];
        for (int i = 0; i < maxLength; i++) {
            chars[i] = domainInt(name + "[" + i + "]", domain);
        }
        return string(name, chars, boundInt("|" + name + "|", 0, maxLength));
    }

    public static IrStringExpr element(IrStringExpr[] array, IrIntExpr index) {
        IrStringExpr[] $array = index.getDomain().getHighBound() + 1 < array.length
                ? Arrays.copyOf(array, index.getDomain().getHighBound() + 1)
                : array.clone();
        for (int i = 0; i < $array.length; i++) {
            if (!index.getDomain().contains(i)) {
                $array[i] = EmptyString;
            }
        }

        if (index.isConstant()) {
            return $array[index.getLowBound()];
        }
        PrimitiveIterator.OfInt iter = index.getDomain().iterator();
        assert iter.hasNext();

        Domain length = EmptyDomain;
        Domain[] chars = new Domain[IrUtil.maxLength($array)];
        Arrays.fill(chars, EmptyDomain);
        while (iter.hasNext()) {
            int val = iter.next();
            if (val < $array.length) {
                IrStringExpr string = $array[val];
                length = length.union(string.getLength());
                for (int i = 0; i < string.getChars().length; i++) {
                    chars[i] = chars[i].union(string.getChars()[i]);
                }
            }
        }
        for (int i = 0; i < chars.length; i++) {
            if (i < length.getLowBound()) {
                chars[i] = chars[i].remove(0);
            } else {
                assert i < length.getHighBound();
                chars[i] = chars[i].insert(0);
            }
        }
        return new IrStringElement($array, index, chars, length);
    }

    public static IrStringExpr concat(IrStringExpr left, IrStringExpr right) {
        Domain leftLength = left.getLength();
        Domain[] leftChars = left.getChars();
        Domain rightLength = right.getLength();
        Domain[] rightChars = right.getChars();

        if (leftLength.getHighBound() == 0) {
            return right;
        }
        if (rightLength.getHighBound() == 0) {
            return left;
        }

        Domain length;
        if (leftLength.isConstant()) {
            length = rightLength.offset(leftLength.getLowBound());
        } else if (rightLength.isConstant()) {
            length = leftLength.offset(rightLength.getLowBound());
        } else {
            length = boundDomain(
                    leftLength.getLowBound() + rightLength.getLowBound(),
                    leftLength.getHighBound() + rightLength.getHighBound());
        }
        Domain[] charDomains = new Domain[length.getHighBound()];
        int i;
        for (i = 0; i < leftLength.getLowBound(); i++) {
            charDomains[i] = leftChars[i];
        }
        Domain union = EmptyDomain;
        for (; i < leftLength.getHighBound(); i++) {
            int j = i - leftLength.getLowBound();
            if (j < rightChars.length) {
                union = union.union(rightChars[j]);
            }
            charDomains[i] = leftChars[i].union(union);
        }
        for (; i < charDomains.length; i++) {
            int j = i - leftLength.getLowBound();
            int k = i - leftLength.getHighBound();
            assert k < rightChars.length;
            charDomains[i] = union(rightChars, k, Math.min(j + 1, rightChars.length));
        }
        for (i = length.getLowBound(); i < charDomains.length; i++) {
            charDomains[i] = charDomains[i].insert(0);
        }
        return IrUtil.asConstant(new IrConcat(left, right, charDomains, length));
    }

    public static IrIntArrayExpr subarray(IrIntArrayExpr array, IrIntExpr index, IrIntExpr sublength) {
        if (index.isConstant()) {
            if (sublength.isConstant()) {
                IrIntExpr[] subarray = new IrIntExpr[array.length()];
                for (int i = 0; i < sublength.getLowBound(); i++) {
                    subarray[i] = get(array, index.getLowBound() + i);
                }
                for (int i = sublength.getLowBound(); i < subarray.length; i++) {
                    subarray[i] = constant(-1);
                }
                return array(subarray);
            }
        }
        Domain[] charDomains = new Domain[array.length()];
        for (int i = 0; i < charDomains.length; i++) {
            charDomains[i] = i < sublength.getHighBound() && index.getLowBound() + i < charDomains.length
                    ? union(array.getDomains(),
                            index.getLowBound() + i,
                            Integer.min(index.getHighBound() + i + 1, charDomains.length))
                    : NegativeOneDomain;
            if (i >= sublength.getLowBound()) {
                charDomains[i] = charDomains[i].insert(-1);
            }
        }
        return new IrSubarray(array, index, sublength, charDomains);
    }

    private static Domain union(Domain[] domains, int start, int end) {
        assert start < end;
        Domain union = domains[start];
        for (int i = start + 1; i < end; i++) {
            union = union.union(domains[i]);
        }
        return union;
    }
}
