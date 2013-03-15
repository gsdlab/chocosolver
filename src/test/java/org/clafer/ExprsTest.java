package org.clafer;

import java.util.HashSet;
import java.util.Set;
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

    @Test
    public void testExprs() {
        final Exprs e = new Exprs(3);
        final ConcreteClafer person = e.newTopClafer("person", 6, new Card(3));
        final ConcreteClafer hand = person.addChildClafer("hand", 12, new Card(2));
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

    @Test
    public void testMultiChildrenIsomorphism() {
        final Exprs e = new Exprs(3);
        final ConcreteClafer person = e.newTopClafer("person", 2, new Card(2));
        final ConcreteClafer hand = person.addChildClafer("hand", 2, new Card(0));
        final ConcreteClafer claw = person.addChildClafer("claw", 2, new Card(0));

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
}
