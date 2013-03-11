package org.clafer;

import java.util.Iterator;
import org.clafer.constraint.JoinManager;
import org.clafer.tree.SetExpr;
import org.clafer.tree.IntExpr;
import org.clafer.tree.BoolExpr;
import org.clafer.tree.Expr;
import org.clafer.tree.RefClafer;
import org.clafer.tree.RootClafer;
import org.clafer.tree.AtomicClafer;
import org.clafer.tree.Card;
import org.clafer.tree.ConcreteClafer;
import org.clafer.constraint.JoinRefManager;
import choco.Choco;
import choco.Options;
import choco.cp.model.CPModel;
import choco.kernel.model.Model;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.clafer.collection.Pair;
import org.clafer.constraint.SingletonManager;
import org.clafer.tree.AbstractClafer;
import org.clafer.tree.IntClafer;
import org.clafer.tree.SetConstraint;

/**
 * Best way to use this class is with a Javascript engine since it has dynamic dispatch.
 * Dynamic dispatch should choose the more optimal translation.
 * 
 * @author jimmy
 */
public class Exprs implements Iterable<String> {

    private final Model model;
    private final IntClafer intClafer;
    private final RootClafer root;

    public Exprs(int bitwidth) {
        model = new CPModel();
        // TODO: needed?
        model.setDefaultExpressionDecomposition(false);
        intClafer = new IntClafer(bitwidth);
        root = new RootClafer(model);
    }

    public Model getModel() {
        return model;
    }

    public RootClafer getRoot() {
        return root;
    }

    public ConcreteClafer newConcreteClafer(String name, int scope, Card card) {
        return new ConcreteClafer(model, name, scope, card, root);
    }

    public ConcreteClafer newConcreteClafer(String name, int scope, Card card, AbstractClafer sup) {
        return new ConcreteClafer(model, name, scope, card, root, sup);
    }

    public ConcreteClafer newConcreteClafer(String name, int scope, Card card, AtomicClafer parent) {
        return new ConcreteClafer(model, name, scope, card, parent);
    }

    public ConcreteClafer newConcreteClafer(String name, int scope, Card card, AtomicClafer parent, AbstractClafer sup) {
        return new ConcreteClafer(model, name, scope, card, parent, sup);
    }

    /**
     * Create a new reference with bag semantics, ie. can reference
     * the same value under the same parent.
     */
    public RefClafer newRefBag(AtomicClafer type, ConcreteClafer parent) {
        return new RefClafer(model, type, parent, false);
    }

    /**
     * Create a new reference with set semantics, ie. cannot reference
     * the same value under the same parent.
     */
    public RefClafer newRefSet(AtomicClafer type, ConcreteClafer parent) {
        return new RefClafer(model, type, parent, true);
    }

    /**
     * Create a new reference to integers with bag semantics, ie. can reference
     * the same value under the same parent.
     */
    public RefClafer newIntRefBag(ConcreteClafer parent) {
        return new RefClafer(model, intClafer, parent, false);
    }

    /**
     * Create a new reference to integers with set semantics, ie. cannot reference
     * the same value under the same parent.
     */
    public RefClafer newIntRefSet(ConcreteClafer parent) {
        return new RefClafer(model, intClafer, parent, true);
    }

    @Override
    public Iterator<String> iterator() {
        return new ExprsSolutions(this);
    }

    private static IntegerVariable[] reverseCards(SetVariable... childSet) {
        IntegerVariable[] reverseCards = new IntegerVariable[childSet.length];
        for (int i = 0; i < reverseCards.length; i++) {
            reverseCards[i] = childSet[childSet.length - i - 1].getCard();
        }
        return reverseCards;
    }

    private static boolean sameChildSetLength(List<ConcreteClafer> clafers) {
        if (clafers.size() > 0) {
            ConcreteClafer fst = clafers.get(0);
            for (ConcreteClafer clafer : clafers) {
                if (fst.getChildSet().length != clafer.getChildSet().length) {
                    return false;
                }
            }
        }
        return true;
    }

    private Pair<IntegerVariable[], Integer> reverseCardsAndScale(List<ConcreteClafer> clafers) {
        assert sameChildSetLength(clafers);

        // Give clafers earlier in the list higher scales
        Collections.reverse(clafers);

        IntegerVariable[][] cards = new IntegerVariable[clafers.size()][];

        for (int i = 0; i < clafers.size(); i++) {
            ConcreteClafer clafer = clafers.get(i);
            cards[i] = reverseCards(clafer.getChildSet());
        }
        cards = Util.transpose(cards);

        long scale = 1;
        int[] scales = new int[clafers.size()];
        for (int i = 0; i < clafers.size(); i++) {
            ConcreteClafer clafer = clafers.get(i);
            scales[i] = (int) scale;
            scale *= clafer.getScope() + 1;
            if(scale > Integer.MAX_VALUE) {
                // It's possible to overflow
                return null;
            }
        }
        scale--;

        IntegerVariable[] scaledCards = new IntegerVariable[cards.length];
        for(int i = 0; i < cards.length; i++) {
            scaledCards[i] = numIntVar("scaledCards", 0, (int) scale);
            model.addConstraint(Choco.equation(scaledCards[i], cards[i], scales));
        }

        return new Pair(scaledCards, (int) scale);
    }

    /**
     * Call at most once AND do not change the model after optimized.
     */
    public void optimize() {
        // Sort child cards
        for (AtomicClafer clafer : root.getNestedChildren()) {
            if (clafer.getScope() <= 1) {
                // Already sorted (since at most one card)
                continue;
            }
            List<ConcreteClafer> children = Util.filterInexactCard(Util.filterConcrete(clafer.getChildren()));
            switch (children.size()) {
                case 0:
                    // Already sorted (since no cards with inexact bounds).
                    break;
                case 1:
                    ConcreteClafer child = (ConcreteClafer) children.get(0);
                    IntegerVariable[] reverseCard = reverseCards(child.getChildSet());
                    model.addConstraint(Choco.increasingSum(reverseCard, child.getSet().getCard()));
                    break;
                default:
                    Pair<IntegerVariable[], Integer> pair = reverseCardsAndScale(children);
                    IntegerVariable[] reverseScaledCards = pair.getFst();
                    int scale = pair.getSnd();
                    model.addConstraint(Choco.increasingSum(reverseScaledCards,
                            numIntVar("increasingSumCard", 0, scale)));
                    break;
            }
        }
    }

    public static void main(String[] args) {
        long start = System.currentTimeMillis();
        final Exprs e = new Exprs(3);
        final ConcreteClafer person = e.newConcreteClafer("person", 2, new Card(2));
        final ConcreteClafer hand = e.newConcreteClafer("hand", 2, new Card(0), person);
        final ConcreteClafer claw = e.newConcreteClafer("claw", 2, new Card(0), person);

//        e.constraintUnder(hand,
//                new SetConstraint() {
//
//                    @Override
//                    public BoolExpr apply(SetExpr thisHand) {
//                        return none(join(joinParent(thisHand), claw));
//                    }
//                });

        e.optimize();

        Set<String> ans = new HashSet<String>();

        int c = 0;
        for (String s : e) {
            c++;
            System.out.println(s);
            if (!ans.add(s)) {
                throw new Error();
            }
        }
        // 40 solutions
        System.out.println(c);

        System.out.println(System.currentTimeMillis() - start);
    }
//    public static void main(String[] args) {
//        long start = System.currentTimeMillis();
//        final Exprs e = new Exprs(3);
//        final ConcreteClafer person = e.newConcreteClafer("person", 6, new Card(3));
//        final ConcreteClafer hand = e.newConcreteClafer("hand", 12, new Card(2), person);
//        final RefClafer age = e.newIntRefBag(person);
//
//        e.constraintUnder(person,
//                new IntConstraint() {
//
//                    @Override
//                    public BoolExpr apply(IntExpr thisPerson) {
//                        return eq(joinRef(thisPerson), e.constantInt(2));
//                    }
//                });
//
//        Set<String> ans = new HashSet<String>();
//
//        int c = 0;
//        for (String s : e) {
//            c++;
//            System.out.println(s);
//            if (!ans.add(s)) {
//                throw new Error();
//            }
//        }
//        // 40 solutions
//        System.out.println(c);
//
//        System.out.println(System.currentTimeMillis() - start);
//    }
    private static int num = 0;

    public static SetVariable setVar(String name, int low, int high, String... options) {
        return Choco.makeSetVar(Check.notNull(name), low, high, options);
    }

    public static SetVariable[] setArray(String name, int dimension, int low, int high, String... options) {
        return Choco.makeSetVarArray(Check.notNull(name), dimension, low, high, options);
    }

    public static SetVariable numSetVar(String name, int low, int high, String... options) {
        return setVar(Check.notNull(name) + (num++), low, high, options);
    }

    public static IntegerVariable intVar(String name, int low, int high, String... options) {
        return Choco.makeIntVar(Check.notNull(name), low, high, options);
    }

    public static IntegerVariable[] intArray(String name, int dimension, int low, int high) {
        return Choco.makeIntVarArray(Check.notNull(name), dimension, low, high);
    }

    public static IntegerVariable numIntVar(String name, int low, int high) {
        return intVar(Check.notNull(name) + (num++), low, high);
    }

    public static IntExpr constant(AtomicClafer type, int value) {
        return new IntExpr(type, Choco.constant(value));
    }

    public IntExpr constantInt(int value) {
        return new IntExpr(intClafer, Choco.constant(value));
    }

    public static SetExpr constantSet(AtomicClafer type, int... values) {
        return new SetExpr(type, Choco.constant(values));
    }

    public SetExpr constantIntSet(int... values) {
        return constantSet(intClafer, values);
    }

    public static BoolExpr none(SetExpr sv) {
        return new BoolExpr(_eq(sv.getValue(), Choco.emptySet()), sv.getConstraints());
    }

    public static BoolExpr some(SetExpr sv) {
        return new BoolExpr(_geqCard(sv.getValue(), 1), sv.getConstraints());
    }

    public static SetExpr intToSet(IntExpr iv) {
        SetVariable sv = numSetVar("intToSet", iv.getLowB(), iv.getUppB());

        List<Constraint> constraints = Util.cons(
                iv.getConstraints(),
                SingletonManager.singleton(iv.getValue(), sv));

        return new SetExpr(iv.getType(), sv, constraints);
    }

    /**
     * @param take
     * @return take.ref
     */
    public static IntExpr joinRef(IntExpr take) {
        Check.notNull(take);
        AtomicClafer type = take.getType();
        if (!type.hasRef()) {
            throw new IllegalArgumentException("Cannot .ref on type " + type.getName());
        }
        RefClafer ref = type.getRef();

        IntegerVariable to = numIntVar("joinRef", ref.getScopeLow(), ref.getScopeHigh());

        List<Constraint> constraints = Util.cons(
                take.getConstraints(),
                Choco.nth(take.getValue(), ref.getRefs(), to));

        return new IntExpr(ref.getType(), to, constraints);
    }

    /**
     * @param take
     * @return take.ref
     */
    public static SetExpr joinRef(SetExpr take) {
        Check.notNull(take);
        AtomicClafer type = take.getType();
        if (!type.hasRef()) {
            throw new IllegalArgumentException("Cannot .ref on type " + type.getName());
        }
        RefClafer ref = type.getRef();

        SetVariable to = numSetVar("joinRef", ref.getScopeLow(), ref.getScopeHigh());

        List<Constraint> constraints = Util.cons(
                take.getConstraints(),
                JoinRefManager.joinRef(take.getValue(), ref.getRefs(), to));

        return new SetExpr(ref.getType(), to, constraints);
    }

    /**
     * @param take
     * @return take.parent
     */
    public static IntExpr joinParent(IntExpr take) {
        Check.notNull(take);
        AtomicClafer type = take.getType();
        if (!(type instanceof ConcreteClafer)) {
            throw new IllegalArgumentException("Cannot .parent on type " + type.getName());
        }
        ConcreteClafer cType = (ConcreteClafer) type;
        AtomicClafer parent = cType.getParent();

        IntegerVariable to = numIntVar("joinParent", parent.getScopeLow(), parent.getScopeHigh());

        List<Constraint> constraints = Util.cons(
                take.getConstraints(),
                Choco.nth(take.getValue(), cType.getParentPointers(), to));

        return new IntExpr(parent, to, constraints);
    }

    /**
     * @param take
     * @return take.parent
     */
    public static SetExpr joinParent(SetExpr take) {
        Check.notNull(take);
        AtomicClafer type = take.getType();
        if (!(type instanceof ConcreteClafer)) {
            throw new IllegalArgumentException("Cannot .parent on type " + type.getName());
        }
        ConcreteClafer cType = (ConcreteClafer) type;
        AtomicClafer parent = cType.getParent();

        SetVariable to = numSetVar("joinParent", parent.getScopeLow(), parent.getScopeHigh());

        List<Constraint> constraints = Util.cons(
                take.getConstraints(),
                JoinRefManager.joinRef(take.getValue(), cType.getParentPointers(), to));

        return new SetExpr(parent, to, constraints);
    }

    /**
     * @param take
     * @param children
     * @return take.children
     */
    public static SetExpr join(IntExpr take, ConcreteClafer children) {
        Check.notNull(take);
        Check.notNull(children);
        if (!take.getType().hasChild(children)) {
            throw new IllegalArgumentException("Cannot join " + take.getType().getName() + "." + children.getName());
        }

        // TODO: Optimize for fixed cards
        return join(intToSet(take), children);
    }

    /**
     * @param take
     * @param children
     * @return take.children
     */
    public static SetExpr join(SetExpr take, ConcreteClafer children) {
        Check.notNull(take);
        Check.notNull(children);
        if (!take.getType().hasChild(children)) {
            throw new IllegalArgumentException("Cannot join " + take.getType().getName() + "." + children.getName());
        }

        int low = Choco.MAX_UPPER_BOUND;
        int high = Choco.MIN_LOWER_BOUND;
        for (SetVariable child : children.getChildSet()) {
            high = Math.max(high, child.getUppB());
            low = Math.min(low, child.getLowB());
        }

        SetVariable to = numSetVar("join", low, high);

        List<Constraint> constraints = Util.cons(
                take.getConstraints(),
                JoinManager.join(take.getValue(), children.getChildSet(), to));

        return new SetExpr(children, to, constraints);
    }

    public static BoolExpr eq(SetExpr e1, SetExpr e2) {
        return new BoolExpr(
                _eq(e1.getValue(), e2.getValue()),
                concatConstraints(e1, e2));
    }

    public static BoolExpr eq(IntExpr e1, IntExpr e2) {
        return new BoolExpr(
                _eq(e1.getValue(), e2.getValue()),
                concatConstraints(e1, e2));
    }

    public static BoolExpr eq(SetExpr e1, IntExpr e2) {
        return new BoolExpr(
                SingletonManager.singleton(e1.getValue(), e2.getValue()),
                concatConstraints(e1, e2));
    }

    public static BoolExpr eq(IntExpr e1, SetExpr e2) {
        return eq(e2, e1);
    }

    public static BoolExpr neq(SetExpr e1, SetExpr e2) {
        return new BoolExpr(
                _neq(e1.getValue(), e2.getValue()),
                concatConstraints(e1, e2));
    }

    public static BoolExpr neq(IntExpr e1, IntExpr e2) {
        return new BoolExpr(
                _neq(e1.getValue(), e2.getValue()),
                concatConstraints(e1, e2));
    }

    public static BoolExpr neq(SetExpr e1, IntExpr e2) {
        return new BoolExpr(
                _or(_neq(e1.getValue().getCard(), 1), _notMember(e2.getValue(), e1.getValue())),
                concatConstraints(e1, e2));
    }

    public static BoolExpr neq(IntExpr e1, SetExpr e2) {
        return neq(e2, e1);
    }

//    public void constraintUnder(AtomicClafer c, IntConstraint b) {
//        SetVariable s = c.getSet();
//        for (int i = s.getLowB(); i <= s.getUppB(); i++) {
//            BoolExpr cond = b.apply(constant(c, i));
//            model.addConstraint(_implies(_member(i, s), cond.getValue()));
//            for (Constraint constraint : cond.getConstraints()) {
//                model.addConstraint(constraint);
//            }
//        }
//    }
    public void constraintUnder(AtomicClafer c, SetConstraint b) {
        SetVariable s = c.getSet();
        for (int i = s.getLowB(); i <= s.getUppB(); i++) {
            SetVariable thisV = numSetVar("constraintUnder" + c.getName(), i, i, Options.V_NO_DECISION);

            BoolExpr cond = b.apply(new SetExpr(c, thisV));

            // TODO: if then else
            model.addConstraint(_implies(_member(i, s), _and(cond.getValue(), _eq(thisV, Choco.constant(new int[]{i})))));
            model.addConstraint(_implies(_notMember(i, s), _eq(thisV, Choco.emptySet())));
            for (Constraint constraint : cond.getConstraints()) {
                model.addConstraint(constraint);
            }
        }
    }

    public static Constraint _eq(SetVariable e1, SetVariable e2) {
        return Choco.eq(e1, e2);
    }

    public static Constraint _eq(IntegerVariable e1, IntegerVariable e2) {
        return Choco.eq(e1, e2);
    }

    public static Constraint _neq(SetVariable e1, SetVariable e2) {
        return Choco.neq(e1, e2);
    }

    public static Constraint _neq(IntegerVariable e1, IntegerVariable e2) {
        return Choco.neq(e1, e2);
    }

    public static Constraint _neq(IntegerVariable e1, int e2) {
        return Choco.neq(e1, e2);
    }

    public static Constraint _leq(IntegerVariable e1, IntegerVariable e2) {
        return Choco.leq(e1, e2);
    }

    public static Constraint _geq(IntegerVariable e1, IntegerVariable e2) {
        return Choco.geq(e1, e2);
    }

    public static Constraint _geqCard(SetVariable sv, int val) {
        return Choco.geqCard(sv, val);
    }

    private static Constraint _or(List<Constraint> constraints) {
        return _or(constraints.toArray(new Constraint[constraints.size()]));
    }

    private static Constraint _or(Constraint... constraints) {
        switch (constraints.length) {
            case 0:
                return Choco.TRUE;
            case 1:
                return constraints[0];
            default:
                return Choco.or(constraints);
        }
    }

    public static Constraint _and(List<Constraint> constraints) {
        return _and(constraints.toArray(new Constraint[constraints.size()]));
    }

    public static Constraint _and(Constraint... constraints) {
        switch (constraints.length) {
            case 0:
                return Choco.TRUE;
            case 1:
                return constraints[0];
            default:
                return Choco.and(constraints);
        }
    }

    public static Constraint _implies(Constraint c1, Constraint c2) {
        return Choco.implies(c1, c2);
    }

    public static Constraint _ifOnlyIf(Constraint c1, Constraint c2) {
        return Choco.ifOnlyIf(c1, c2);
    }

    public static Constraint _member(int val, SetVariable sv) {
        return Choco.member(val, sv);
    }

    public static Constraint _notMember(int iv, SetVariable sv) {
        return Choco.notMember(iv, sv);
    }

    public static Constraint _notMember(IntegerVariable iv, SetVariable sv) {
        return Choco.notMember(iv, sv);
    }

    public static Constraint _betweenCard(SetVariable set, int low, int high) {
        if (low == high) {
            return Choco.eqCard(set, low);
        }
        List<Constraint> constraints = new ArrayList<Constraint>();
        if (low != 0) {
            constraints.add(Choco.geqCard(set, low));
        }
        if (high != Integer.MAX_VALUE) {
            constraints.add(Choco.leqCard(set, high));
        }
        return _and(constraints);
    }

    private static SetVariable[] getValues(SetExpr[] children) {
        SetVariable[] r = new SetVariable[children.length];
        for (int i = 0; i < children.length; i++) {
            r[i] = children[i].getValue();
        }
        return r;
    }

    private static List<Constraint> concatConstraints(Expr... exprs) {
        List<Constraint> r = new ArrayList<Constraint>();
        for (Expr expr : exprs) {
            r.addAll(expr.getConstraints());
        }
        return r;
    }
}
