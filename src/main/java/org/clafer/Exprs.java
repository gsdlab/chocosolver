package org.clafer;

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
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.clafer.constraint.SingletonManager;
import org.clafer.tree.AbstractClafer;
import org.clafer.tree.ClaferConstraint;
import org.clafer.tree.IntClafer;
import org.clafer.tree.IntConstraint;
import org.clafer.tree.SetConstraint;

/**
 * Best way to use this class is with a Javascript engine since it has dynamic dispatch.
 * Dynamic dispatch should choose the more optimal translation.
 * 
 * @author jimmy
 */
public class Exprs implements Iterable<String> {

    private final IntClafer intClafer;
    private final RootClafer root;

    public Exprs(int bitwidth) {
        // TODO: needed?
        // model.setDefaultExpressionDecomposition(false);
        intClafer = new IntClafer(bitwidth);
        root = new RootClafer();
    }

    @Override
    public ExprsSolutions iterator() {
        return new ExprsSolutions(this);
    }

    public RootClafer getRoot() {
        return root;
    }

    public ConcreteClafer newConcreteClafer(String name, int scope, Card card) {
        return new ConcreteClafer(name, scope, card, root);
    }

    public ConcreteClafer newConcreteClafer(String name, int scope, Card card, AbstractClafer sup) {
        return new ConcreteClafer(name, scope, card, root, sup);
    }

    public ConcreteClafer newConcreteClafer(String name, int scope, Card card, AtomicClafer parent) {
        return new ConcreteClafer(name, scope, card, parent);
    }

    public ConcreteClafer newConcreteClafer(String name, int scope, Card card, AtomicClafer parent, AbstractClafer sup) {
        return new ConcreteClafer(name, scope, card, parent, sup);
    }

    /**
     * Create a new reference with bag semantics, ie. can reference
     * the same value under the same parent.
     */
    public RefClafer newRefBag(AtomicClafer type, ConcreteClafer parent) {
        return new RefClafer(type, parent, false);
    }

    /**
     * Create a new reference with set semantics, ie. cannot reference
     * the same value under the same parent.
     */
    public RefClafer newRefSet(AtomicClafer type, ConcreteClafer parent) {
        return new RefClafer(type, parent, true);
    }

    /**
     * Create a new reference to integers with bag semantics, ie. can reference
     * the same value under the same parent.
     */
    public RefClafer newIntRefBag(ConcreteClafer parent) {
        return new RefClafer(intClafer, parent, false);
    }

    /**
     * Create a new reference to integers with set semantics, ie. cannot reference
     * the same value under the same parent.
     */
    public RefClafer newIntRefSet(ConcreteClafer parent) {
        return new RefClafer(intClafer, parent, true);
    }

//    public static void main(String[] args) {
//        long start = System.currentTimeMillis();
//        final Exprs e = new Exprs(3);
//        final ConcreteClafer person = e.newConcreteClafer("person", 2, new Card(2));
//        final ConcreteClafer hand = e.newConcreteClafer("hand", 2, new Card(0), person);
//        final ConcreteClafer claw = e.newConcreteClafer("claw", 2, new Card(0), person);
//
//        final ClaferConstraint handClawDisjoint = new ClaferConstraint(hand,
//                new SetConstraint() {
//
//                    @Override
//                    public BoolExpr apply(SetExpr thisHand) {
//                        return none(e.join(e.joinParent(thisHand), claw));
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
//    public static void main(String[] args) {
//        long start = System.currentTimeMillis();
//        final Exprs e = new Exprs(3);
//        final ConcreteClafer age = e.newConcreteClafer("age", 3, new Card(2));
//        final RefClafer ageRef = e.newIntRefBag(age);
//
//        final ClaferConstraint ageConstraint = new ClaferConstraint(age,
//                new SetConstraint() {
//
//                    @Override
//                    public BoolExpr apply(SetExpr thisHand) {
//                        return e.eq(e.joinRef(thisHand), e.constantInt(3));
//                    }
//                },
//                new IntConstraint() {
//
//                    @Override
//                    public BoolExpr apply(IntExpr thisHand) {
//                        return e.eq(e.joinRef(thisHand), e.constantInt(3));
//                    }
//                });
//
//        Set<String> ans = new HashSet<String>();
//
//        ExprsSolutions iter = e.iterator();
//        while (iter.hasNext()) {
//            String s = iter.next();
//            System.out.println(iter.getRuntimeStatistics());
//            System.out.println(iter.getSolutionToString());
//
//            System.out.println(s);
//            if (!ans.add(s)) {
//                throw new Error();
//            }
//        }
//        // 9 Time (ms), 4 Nodes, 4 Backtracks, 0 Restarts - 
//        System.out.println(iter.getRuntimeStatistics());
//        System.out.println(iter.getSolutionCount());
//        System.out.println(System.currentTimeMillis() - start);
//    }
    public static void main(String[] args) {
        long start = System.currentTimeMillis();
        final Exprs e = new Exprs(3);
        final ConcreteClafer person = e.newConcreteClafer("person", 6, new Card(3));
        final ConcreteClafer hand = e.newConcreteClafer("hand", 12, new Card(2), person);
        final RefClafer age = e.newIntRefBag(person);

        new ClaferConstraint(person,
                new SetConstraint() {

                    @Override
                    public BoolExpr apply(SetExpr thisPerson) {
                        return e.eq(e.joinRef(thisPerson), e.constantInt(2));
                    }
                },
                new IntConstraint() {

                    @Override
                    public BoolExpr apply(IntExpr thisPerson) {
                        return e.eq(e.joinRef(thisPerson), e.constantInt(2));
                    }
                });

        Set<String> ans = new HashSet<String>();

        ExprsSolutions iter = e.iterator();
        while (iter.hasNext()) {
            String s = iter.next();
            System.out.println(iter.getRuntimeStatistics());
            System.out.println(iter.getSolutionToString());

            System.out.println(s);
            if (!ans.add(s)) {
                throw new Error();
            }
        }
        // 401 Time (ms), 179 Nodes, 278 Backtracks, 0 Restarts - 
        System.out.println(iter.getRuntimeStatistics());
        System.out.println(iter.getSolutionCount());
        System.out.println(System.currentTimeMillis() - start);
    }
    private int num = 0;

    public SetVariable numSetVar(String name, int low, int high, String... options) {
        return Choco.makeSetVar(Check.notNull(name) + (num++), low, high, options);
    }

    public IntegerVariable numIntVar(String name, int low, int high) {
        return Choco.makeIntVar(Check.notNull(name) + (num++), low, high);
    }

    public IntExpr constant(AtomicClafer type, int value) {
        return new IntExpr(type, Choco.constant(value));
    }

    public IntExpr constantInt(int value) {
        return new IntExpr(intClafer, Choco.constant(value));
    }

    public SetExpr constantSet(AtomicClafer type, int... values) {
        return new SetExpr(type, Choco.constant(values));
    }

    public SetExpr constantIntSet(int... values) {
        return constantSet(intClafer, values);
    }

    public BoolExpr none(SetExpr sv) {
        return new BoolExpr(Choco.eq(sv.getValue(), Choco.emptySet()), sv.getConstraints());
    }

    public BoolExpr some(SetExpr sv) {
        return new BoolExpr(Choco.geqCard(sv.getValue(), 1), sv.getConstraints());
    }

    public SetExpr intToSet(IntExpr iv) {
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
    public IntExpr joinRef(IntExpr take) {
        Check.notNull(take);
        AtomicClafer type = take.getType();
        if (!type.hasRef()) {
            throw new IllegalArgumentException("Cannot .ref on type " + type.getName());
        }
        RefClafer ref = type.getRef();

        Integer constant = take.getConstant();
        if (constant != null) {
            return new IntExpr(ref.getType(), ref.getRefs()[constant], take.getConstraints());
        }

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
    public SetExpr joinRef(SetExpr take) {
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
    public IntExpr joinParent(IntExpr take) {
        Check.notNull(take);
        AtomicClafer type = take.getType();
        if (!(type instanceof ConcreteClafer)) {
            throw new IllegalArgumentException("Cannot .parent on type " + type.getName());
        }
        ConcreteClafer cType = (ConcreteClafer) type;
        AtomicClafer parent = cType.getParent();

        Integer constant = take.getConstant();
        if (constant != null) {
            return new IntExpr(parent, cType.getParentPointers()[constant], take.getConstraints());
        }

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
    public SetExpr joinParent(SetExpr take) {
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
    public SetExpr join(IntExpr take, ConcreteClafer children) {
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
    public SetExpr join(SetExpr take, ConcreteClafer children) {
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

    public BoolExpr eq(SetExpr e1, SetExpr e2) {
        return new BoolExpr(
                Choco.eq(e1.getValue(), e2.getValue()),
                concatConstraints(e1, e2));
    }

    public BoolExpr eq(IntExpr e1, IntExpr e2) {
        return new BoolExpr(
                Choco.eq(e1.getValue(), e2.getValue()),
                concatConstraints(e1, e2));
    }

    public BoolExpr eq(SetExpr e1, IntExpr e2) {
        // TODO: use singleton but we need a "not singleton" constraint for opposite
        return new BoolExpr(
                Choco.and(Choco.eqCard(e1.getValue(), 1), Choco.member(e2.getValue(), e1.getValue())),
                //                SingletonManager.singleton(e1.getValue(), e2.getValue()),
                concatConstraints(e1, e2));
    }

    public BoolExpr eq(IntExpr e1, SetExpr e2) {
        return eq(e2, e1);
    }

    public BoolExpr neq(SetExpr e1, SetExpr e2) {
        return new BoolExpr(
                Choco.neq(e1.getValue(), e2.getValue()),
                concatConstraints(e1, e2));
    }

    public BoolExpr neq(IntExpr e1, IntExpr e2) {
        return new BoolExpr(
                Choco.neq(e1.getValue(), e2.getValue()),
                concatConstraints(e1, e2));
    }

    public BoolExpr neq(SetExpr e1, IntExpr e2) {
        return new BoolExpr(
                Choco.or(
                Choco.neq(e1.getValue().getCard(), 1),
                Choco.notMember(e2.getValue(), e1.getValue())),
                concatConstraints(e1, e2));
    }

    public BoolExpr neq(IntExpr e1, SetExpr e2) {
        return neq(e2, e1);
    }

    private static List<Constraint> concatConstraints(Expr... exprs) {
        List<Constraint> r = new ArrayList<Constraint>();
        for (Expr expr : exprs) {
            r.addAll(expr.getConstraints());
        }
        return r;
    }
}
