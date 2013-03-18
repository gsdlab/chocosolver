package org.clafer;

import java.util.HashSet;
import java.util.Set;
import org.clafer.tree.AbstractClafer;
import org.clafer.tree.BoolExpr;
import org.clafer.tree.Card;
import org.clafer.tree.ConcreteClafer;
import org.clafer.tree.IntConstraint;
import org.clafer.tree.IntExpr;
import org.clafer.tree.SetConstraint;
import org.clafer.tree.SetExpr;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class ExprsTest {

    private static int allSolutions(Exprs e) {
        Set<String> ans = new HashSet<String>();
        ExprsSolutions iter = e.iterator();
        while (iter.hasNext()) {
            assertTrue(ans.add(iter.next()));
        }
        return iter.getSolutionCount();
    }

    @Test(timeout = 60000)
    public void testExprs() {
        final Exprs e = new Exprs();
        final ConcreteClafer person = e.newTopClafer("person", 6, new Card(3));
        final ConcreteClafer hand = person.addChild("hand", 12, new Card(2));
        hand.refTo(e.getIntType());

        hand.addConstraint(
                new SetConstraint() {

                    @Override
                    public BoolExpr apply(SetExpr thisHand) {
                        return e.eq(e.joinRef(thisHand), e.constantInt(2));
                    }
                },
                new IntConstraint() {

                    @Override
                    public BoolExpr apply(IntExpr thisHand) {
                        return e.eq(e.joinRef(thisHand), e.constantInt(2));
                    }
                });

        assertEquals(40, allSolutions(e));
    }

    @Test(timeout = 60000)
    public void testMultiChildrenIsomorphism() {
        final Exprs e = new Exprs();
        final ConcreteClafer person = e.newTopClafer("person", 2, new Card(2));
        final ConcreteClafer hand = person.addChild("hand", 2, new Card(0));
        final ConcreteClafer claw = person.addChild("claw", 2, new Card(0));

        hand.addConstraint(
                new SetConstraint() {

                    @Override
                    public BoolExpr apply(SetExpr thisHand) {
                        return e.none(e.join(e.joinParent(thisHand), claw));
                    }
                },
                new IntConstraint() {

                    @Override
                    public BoolExpr apply(IntExpr thisHand) {
                        return e.none(e.join(e.joinParent(thisHand), claw));
                    }
                });

        assertEquals(11, allSolutions(e));
    }

    @Test(timeout = 60000)
    public void testAbstractIsomorphism() {
        final Exprs e = new Exprs();
        final AbstractClafer animal = e.newAbstractClafer("animal", 2);
        final ConcreteClafer person = e.newTopClafer("person", 2, new Card(2)).extending(animal);
        final ConcreteClafer hand = animal.addChild("hand", 2, new Card(0));
        final ConcreteClafer claw = person.addChild("claw", 2, new Card(0));

        assertEquals(20, allSolutions(e));
    }

    @Test(timeout = 60000)
    public void testUpcastIsomorphism() {
        final Exprs e = new Exprs();
        final AbstractClafer object = e.newAbstractClafer("object", 2);
        final AbstractClafer animal = e.newAbstractClafer("animal", 2).extending(object);
        final ConcreteClafer person = e.newTopClafer("person", 2, new Card(2)).extending(animal);
        final ConcreteClafer hand = person.addChild("hand", 2, new Card(0));
        final ConcreteClafer claw = animal.addChild("claw", 2, new Card(0));
        final ConcreteClafer name = object.addChild("name", 2, new Card(1, 1));

        hand.addConstraint(
                new SetConstraint() {

                    @Override
                    public BoolExpr apply(SetExpr thisHand) {
                        return e.none(e.join(e.upcast(e.joinParent(thisHand)), claw));
                    }
                },
                new IntConstraint() {

                    @Override
                    public BoolExpr apply(IntExpr thisHand) {
                        return e.none(e.join(e.upcast(e.joinParent(thisHand)), claw));
                    }
                });

        assertEquals(11, allSolutions(e));
    }

    @Test(timeout = 60000)
    public void testMultiSub() {
        final Exprs e = new Exprs();
        final AbstractClafer animal = e.newAbstractClafer("animal", 4);
        final ConcreteClafer claw = animal.addChild("claw", 4, new Card(1, 1));
        final ConcreteClafer person = e.newTopClafer("person", 2, new Card()).extending(animal);
        final ConcreteClafer lion = e.newTopClafer("lion", 2, new Card()).extending(animal);

        for (String s : e) {
            System.out.println(s);
        }
    }
}
