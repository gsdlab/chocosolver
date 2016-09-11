package org.clafer.ir.analysis.deduction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.clafer.collection.DisjointSets;
import org.clafer.common.UnsatisfiableException;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrNot;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.IrUtil.Ordering.EQ;
import static org.clafer.ir.IrUtil.Ordering.GE;
import static org.clafer.ir.IrUtil.Ordering.GT;
import static org.clafer.ir.IrUtil.Ordering.LE;
import static org.clafer.ir.IrUtil.Ordering.LT;
import static org.clafer.ir.IrUtil.Ordering.UNKNOWN;
import org.clafer.ir.Irs;
import static org.clafer.ir.Irs.domainInt;
import static org.clafer.ir.Irs.set;

/**
 *
 * @author jimmy
 */
class Deduction {

    private final Map<Class<?>, BoolDeducer<?>> boolDeducers;
    private final Map<Class<?>, IntDeducer<?>> intDeducers;
    private final Map<Class<?>, SetDeducer<?>> setDeducers;

    private final DisjointSets<IrIntVar> intEquals;
    private final DisjointSets<IrIntVar> intNotEquals;
    public final Map<IrIntVar, Domain> intRetains;

    private final DisjointSets<IrSetVar> setEquals;
    private final Map<IrSetVar, Domain> setContains;
    private final Map<IrSetVar, Domain> setSubsetOf;

    public Deduction(
            Map<Class<?>, BoolDeducer<?>> boolDeducers,
            Map<Class<?>, IntDeducer<?>> intDeducers,
            Map<Class<?>, SetDeducer<?>> setDeducers) {
        this.boolDeducers = boolDeducers;
        this.intDeducers = intDeducers;
        this.setDeducers = setDeducers;
        this.intEquals = new DisjointSets<>();
        this.intNotEquals = new DisjointSets<>();
        this.intRetains = new HashMap<>();
        this.setEquals = new DisjointSets<>();
        this.setContains = new HashMap<>();
        this.setSubsetOf = new HashMap<>();

    }

    public Deduction(Deduction deduction) {
        this.boolDeducers = deduction.boolDeducers;
        this.intDeducers = deduction.intDeducers;
        this.setDeducers = deduction.setDeducers;
        this.intEquals = new DisjointSets<>(deduction.intEquals);
        this.intNotEquals = new DisjointSets<>(deduction.intNotEquals);
        this.intRetains = new HashMap<>(deduction.intRetains);
        this.setEquals = new DisjointSets<>(deduction.setEquals);
        this.setContains = new HashMap<>(deduction.setContains);
        this.setSubsetOf = new HashMap<>(deduction.setSubsetOf);
    }

    boolean checkInvariants() {
        intRetains.forEach((var, domain) -> {
            assert domain.isSubsetOf(var.getDomain()) && !domain.equals(var.getDomain());
            assert !var.isConstant();
        });
        setContains.forEach((var, domain) -> {
            assert domain.isSupersetOf(var.getKer()) && !domain.equals(var.getKer());
            assert !var.isConstant();
        });
        setSubsetOf.forEach((var, domain) -> {
            assert domain.isSubsetOf(var.getEnv()) && !domain.equals(var.getEnv()) : domain + " : " + var;
            assert !var.isConstant();
        });
        return true;
    }

    public Collection<Set<IrIntVar>> getIntEquals() {
        return intEquals.connectedComponents();
    }

    public Map<IrIntVar, Domain> getIntRetains() {
        return intRetains;
    }

    public Collection<Set<IrSetVar>> getSetEquals() {
        return setEquals.connectedComponents();
    }

    public Map<IrSetVar, Domain> getSetContains() {
        return setContains;
    }

    public Map<IrSetVar, Domain> getSetSubsetOf() {
        return setSubsetOf;
    }

    public void tautology(IrBoolExpr expr) {
        BoolDeducer boolDeducer = boolDeducers.get(expr.getClass());
        if (boolDeducer != null) {
            boolDeducer.deduce(expr, this);
        }
    }

    public void contradiction(IrBoolExpr expr) {
        IrBoolExpr not = Irs.not(expr);
        // Prevent infinite recursion.
        if (expr instanceof IrBoolVar || !(not instanceof IrNot)) {
            tautology(not);
        }
    }

    public void equal(IrIntExpr left, IrIntExpr right) {
        if (left.isConstant()) {
            within(right, left.getDomain());
        } else if (right.isConstant()) {
            within(left, right.getDomain());
        } else {
            if (left instanceof IrIntVar && right instanceof IrIntVar) {
                intEquals.union((IrIntVar) left, (IrIntVar) right);
            }
            within(left, right.getDomain());
            within(right, left.getDomain());
        }
    }

    public void equal(IrIntExpr expr, int value) {
        within(expr, Domains.constantDomain(value));
    }

    public void notEqual(IrIntExpr left, IrIntExpr right) {
        if (left.isConstant()) {
            within(right, right.getDomain().remove(left.getDomain().getLowBound()));
        } else if (right.isConstant()) {
            within(left, left.getDomain().remove(right.getDomain().getLowBound()));
        } else if (left instanceof IrIntVar && right instanceof IrIntVar) {
            intNotEquals.union((IrIntVar) left, (IrIntVar) right);
        }
    }

    public void notEqual(IrIntExpr expr, int value) {
        notWithin(expr, Domains.constantDomain(value));
    }

    public void greaterThan(IrIntExpr left, IrIntExpr right) {
        lessThan(right, left);
    }

    public void greaterThanEqual(IrIntExpr left, IrIntExpr right) {
        lessThanEqual(right, left);
    }

    public void greaterThanEqual(IrIntExpr expr, int lb) {
        Domain domain = expr.getDomain();
        if (domain.getLowBound() < lb) {
            within(expr, domain.boundLow(lb));
        }
    }

    public void lessThan(IrIntExpr left, IrIntExpr right) {
        Domain leftDomain = left.getDomain();
        Domain rightDomain = right.getDomain();
        if (leftDomain.getHighBound() >= rightDomain.getHighBound()) {
            within(left, leftDomain.boundHigh(rightDomain.getHighBound() - 1));
        }
        if (rightDomain.getLowBound() <= leftDomain.getLowBound()) {
            within(right, rightDomain.boundLow(leftDomain.getLowBound() + 1));
        }
    }

    public void lessThanEqual(IrIntExpr left, IrIntExpr right) {
        Domain leftDomain = left.getDomain();
        Domain rightDomain = right.getDomain();
        if (leftDomain.getHighBound() > rightDomain.getHighBound()) {
            within(left, leftDomain.boundHigh(rightDomain.getHighBound()));
        }
        if (rightDomain.getLowBound() < leftDomain.getLowBound()) {
            within(right, rightDomain.boundLow(leftDomain.getLowBound()));
        }
    }

    public void lessThanEqual(IrIntExpr expr, int ub) {
        Domain domain = expr.getDomain();
        if (domain.getHighBound() > ub) {
            within(expr, domain.boundHigh(ub));
        }
    }

    public void allDifferent(IrIntExpr[] exprs) {
        for (int i = 0; i < exprs.length - 1; i++) {
            notEqual(exprs[i], exprs[i + 1]);
        }
        // TODO improve
    }

    public void atLeastNValue(IrIntExpr[] exprs, int n) {
        if (n == exprs.length) {
            for (int i = 0; i < exprs.length - 1; i++) {
                notEqual(exprs[i], exprs[i + 1]);
            }
        }
        // TODO improve
    }

    public void atMostNValue(IrIntExpr[] exprs, int n) {
        if (n == 1) {
            for (int i = 0; i < exprs.length - 1; i++) {
                equal(exprs[i], exprs[i + 1]);
            }
        }
        // TODO improve
    }

    public void within(IrIntExpr expr, Domain domain) {
        if (!domain.isSupersetOf(expr.getDomain())) {
            if (expr instanceof IrIntVar) {
                IrIntVar var = (IrIntVar) expr;
                if (intRetains.containsKey(var)) {
                    intRetains.merge(var, domain, Domain::intersection);
                } else {
                    domain = expr.getDomain().intersection(domain);
                    failIf(domain.isEmpty());
                    if (domain != expr.getDomain()) {
                        intRetains.put(var, domain);
                    }
                }
            } else {
                IntDeducer intDeducer = intDeducers.get(expr.getClass());
                if (intDeducer != null) {
                    intDeducer.deduce(expr, domain, this);
                } else if (domain.isConstant() && expr instanceof IrBoolExpr) {
                    if (domain.getLowBound() == 0) {
                        contradiction((IrBoolExpr) expr);
                    } else {
                        assert domain.getLowBound() == 1;
                        tautology((IrBoolExpr) expr);
                    }
                }
            }
        }
    }

    public void notWithin(IrIntExpr expr, Domain domain) {
        within(expr, expr.getDomain().difference(domain));
    }

    public void equal(IrSetExpr left, IrSetExpr right) {
        if (left instanceof IrSetVar && right instanceof IrSetVar) {
            IrSetVar leftVar = (IrSetVar) left;
            IrSetVar rightVar = (IrSetVar) right;
            equal(leftVar.getCardVar(), rightVar.getCardVar());
            if (left.isConstant()) {
                kerContains(right, left.getKer());
                envSubsetOf(right, left.getEnv());
            } else if (right.isConstant()) {
                kerContains(left, right.getKer());
                envSubsetOf(left, right.getEnv());
            } else {
                setEquals.union(leftVar, rightVar);
            }
        } else {
            kerContains(left, right.getKer());
            kerContains(right, left.getKer());
            envSubsetOf(left, right.getEnv());
            envSubsetOf(right, left.getEnv());
            equal(Irs.card(left), Irs.card(right));
        }
    }

    public void kerContains(IrSetExpr expr, int value) {
        kerContains(expr, Domains.constantDomain(value));
    }

    public void kerContains(IrSetExpr expr, Domain domain) {
        if (expr instanceof IrSetVar) {
            IrSetVar var = (IrSetVar) expr;
            if (setContains.containsKey(var)) {
                Domain merge = setContains.merge(var, domain, Domain::union);
                if (domain != merge) {
                    cardGreaterThanEqual(expr, merge.size());
                }
            } else {
                domain = expr.getKer().union(domain);
                if (domain != expr.getKer()) {
                    setContains.put(var, domain);
                    cardGreaterThanEqual(expr, domain.size());
                }
            }
        } else {
            SetDeducer setDeducer = setDeducers.get(expr.getClass());
            if (setDeducer != null) {
                setDeducer.deduceKer(expr, domain, this);
            }
        }
    }

    public void envSubsetOf(IrSetExpr expr, Domain domain) {
        if (expr instanceof IrSetVar) {
            IrSetVar var = (IrSetVar) expr;
            if (setSubsetOf.containsKey(var)) {
                Domain merge = setSubsetOf.merge((IrSetVar) expr, domain, Domain::intersection);
                if (domain != merge) {
                    cardLessThanEqual(expr, merge.size());
                }
            } else {
                domain = expr.getEnv().intersection(domain);
                if (domain != expr.getEnv()) {
                    setSubsetOf.put(var, domain);
                    cardLessThanEqual(expr, domain.size());
                }
            }
        } else {
            SetDeducer setDeducer = setDeducers.get(expr.getClass());
            if (setDeducer != null) {
                setDeducer.deduceEnv(expr, domain, this);
            }
        }
    }

    public void envRemove(IrSetExpr expr, Domain domain) {
        envSubsetOf(expr, expr.getEnv().difference(domain));
    }

    public void cardGreaterThanEqual(IrSetExpr expr, int lb) {
        Domain card = expr.getCard();
        failIf(card.getHighBound() < lb);
        if (card.getLowBound() < lb) {
            cardWithin(expr, Domains.boundDomain(lb, card.getHighBound()));
        }
    }

    public void cardLessThanEqual(IrSetExpr expr, int ub) {
        Domain card = expr.getCard();
        failIf(card.getLowBound() > ub);
        if (card.getHighBound() > ub) {
            cardWithin(expr, Domains.boundDomain(card.getLowBound(), ub));
        }
    }

    public void cardWithin(IrSetExpr expr, Domain domain) {
        if (expr instanceof IrSetVar) {
            within(((IrSetVar) expr).getCardVar(), domain);
        } else {
            SetDeducer setDeducer = setDeducers.get(expr.getClass());
            if (setDeducer != null) {
                setDeducer.deduceCard(expr, domain, this);
            }
        }
    }

    public void lexEqual(IrIntExpr[] a, IrIntExpr[] b) {
        for (int i = 0; i < Math.min(a.length, b.length); i++) {
            equal(a[i], b[i]);
        }
    }

    public void lexThan(IrIntExpr[] a, IrIntExpr[] b) {
        lexThan(a, b, 0);
    }

    private void lexThan(IrIntExpr[] a, IrIntExpr[] b, int index) {
        assert a.length == b.length;
        failIf(index == a.length);
        switch (IrUtil.compare(a[index], b[index])) {
            case EQ:
                lexThan(a, b, index + 1);
                return;
            case LT:
                return;
            case GT:
                fail();
                return;
            case LE:
            case GE:
            case UNKNOWN:
                switch (IrUtil.compareString(a, b, index + 1)) {
                    case EQ:
                    case GT:
                    case GE:
                        lessThan(a[index], b[index]);
                        return;
                    case LT:
                    case LE:
                    case UNKNOWN:
                        lessThanEqual(a[index], b[index]);
                        return;
                    default:
                        throw new IllegalStateException();
                }
            default:
                throw new IllegalStateException();
        }
    }

    public void lexThanEqual(IrIntExpr[] a, IrIntExpr[] b) {
        lexThanEqual(a, b, 0);
    }

    private void lexThanEqual(IrIntExpr[] a, IrIntExpr[] b, int index) {
        if (index == a.length || index == b.length) {
            return;
        }
        switch (IrUtil.compare(a[index], b[index])) {
            case EQ:
                lexThanEqual(a, b, index + 1);
                return;
            case LT:
                return;
            case GT:
                fail();
                return;
            case LE:
            case GE:
            case UNKNOWN:
                switch (IrUtil.compareString(a, b, index + 1)) {
                    case EQ:
                    case LT:
                    case LE:
                    case GE:
                    case UNKNOWN:
                        lessThanEqual(a[index], b[index]);
                        return;
                    case GT:
                        lessThan(a[index], b[index]);
                        return;
                    default:
                        throw new IllegalStateException();
                }
            default:
                throw new IllegalStateException();
        }
    }

    public void fail() {
        throw new UnsatisfiableException();
    }

    public void failIf(boolean condition) {
        if (condition) {
            throw new UnsatisfiableException();
        }
    }

    public Coalesce apply(Set<IrSetVar> setVariables, Set<IrStringVar> stringVariables) {
        Map<IrIntVar, IrIntVar> coalescedInts = new HashMap<>();
        Map<IrSetVar, IrSetVar> coalescedSets = new HashMap<>();

        Collection<IrSetVar> pendingSetVars = new ArrayList<>(setVariables);

        for (IrStringVar var : stringVariables) {
            IrIntVar[] chars = var.getCharVars();
            Domain length = intRetains.get(var.getLengthVar());
            if (length != null) {
                for (int i = length.getHighBound(); i < chars.length; i++) {
                    // If the length was reduced, then set the trailing characters to 0.
                    // Do this before coalescing the integer variables.
                    equal(chars[i], 0);
                }
            }
        }

        for (Set<IrIntVar> component : intEquals.connectedComponents()) {
            if (component.size() > 1) {
                Iterator<IrIntVar> iter = component.iterator();
                IrIntVar var = iter.next();
                Domain domain = removeOrDefault(intRetains, var, var.getDomain());
                List<String> names = new ArrayList<>(component.size());
                names.add(var.getName());
                while (iter.hasNext()) {
                    var = iter.next();
                    domain = domain.intersection(removeOrDefault(intRetains, var, var.getDomain()));
                    names.add(var.getName());
                }
                IrIntVar coalesced = domainInt(joinNames(names), domain);
                for (IrIntVar coalesce : component) {
                    coalescedInts.put(coalesce, coalesced);
                }
            }
        }
        intRetains.forEach(
                (var, domain) -> coalescedInts.put(var, domainInt(var.getName(), domain))
        );

        for (Set<IrSetVar> component : setEquals.connectedComponents()) {
            if (component.size() > 1) {
                Iterator<IrSetVar> iter = component.iterator();
                IrSetVar var = iter.next();
                Domain ker = setContains.getOrDefault(var, var.getKer());
                Domain env = setSubsetOf.getOrDefault(var, var.getEnv());
                IrIntVar card = coalescedInts.getOrDefault(var.getCardVar(), var.getCardVar());
                List<String> names = new ArrayList<>(component.size());
                names.add(var.getName());
                while (iter.hasNext()) {
                    var = iter.next();
                    ker = ker.union(setContains.getOrDefault(var, var.getKer()));
                    env = env.intersection(setSubsetOf.getOrDefault(var, var.getEnv()));
                    assert card.equals(coalescedInts.getOrDefault(var.getCardVar(), var.getCardVar()));
                    names.add(var.getName());
                }
                IrSetVar coalesced = set(joinNames(names), env, ker, card);
                for (IrSetVar coalesce : component) {
                    coalescedSets.put(coalesce, coalesced);
                }
                pendingSetVars.removeAll(component);
            }
        }
        for (IrSetVar var : pendingSetVars) {
            if (!var.isConstant()) {
                Domain ker = setContains.get(var);
                Domain env = setSubsetOf.get(var);
                IrIntVar card = coalescedInts.get(var.getCardVar());
                if (ker != null || env != null || card != null) {
                    ker = ker == null ? var.getKer() : ker;
                    env = env == null ? var.getEnv() : env;
                    card = card == null ? var.getCardVar() : card;
                    coalescedSets.put(var, set(var.getName(), env, ker, card));
                }
            }
        }
        return new Coalesce(coalescedInts, coalescedSets);
    }

    private static <K, V> V removeOrDefault(Map<K, V> map, K key, V defaultValue) {
        V value = map.remove(key);
        return value == null ? defaultValue : value;
    }

    private static String stripParens(String name) {
        if (name.startsWith("(") && name.endsWith(")")) {
            return name.substring(1, name.length() - 1);
        }
        return name;
    }

    private static String joinNames(List<String> names) {
        if (names.size() == 1) {
            return names.get(0);
        }
        return names.stream().map(Deduction::stripParens).collect(Collectors.joining(";", "(", ")"));
    }

    @Override
    public String toString() {
        return intEquals + "\n\n" + intNotEquals + "\n\n" + intRetains;
    }
}
