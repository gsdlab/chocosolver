package org.clafer;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import org.clafer.tree.BoolExpr;
import org.clafer.tree.Card;
import org.clafer.tree.ClaferConstraint;
import org.clafer.tree.ConcreteClafer;
import org.clafer.tree.IntConstraint;
import org.clafer.tree.IntExpr;
import org.clafer.tree.RefClafer;
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
            System.out.println(iter.getRuntimeStatistics());
        }
        return iter.getSolutionCount();
    }

    @Test
    public void testExprs() {
        final Exprs e = new Exprs(3);
        final ConcreteClafer person = e.newConcreteClafer("person", 6, new Card(3));
        final ConcreteClafer hand = e.newConcreteClafer("hand", 12, new Card(2), person);
        final RefClafer age = e.newIntRefBag(person);

        final ClaferConstraint atleast2 = new ClaferConstraint(person,
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

        assertEquals(40, allSolutions(e));
    }
    
    @Test
    public void testMultiChildrenIsomorphism() {
        final Exprs e = new Exprs(3);
        final ConcreteClafer person = e.newConcreteClafer("person", 2, new Card(2));
        final ConcreteClafer hand = e.newConcreteClafer("hand", 2, new Card(0), person);
        final ConcreteClafer claw = e.newConcreteClafer("claw", 2, new Card(0), person);

        final ClaferConstraint handClawDisjoint = new ClaferConstraint(hand,
                new SetConstraint() {

                    @Override
                    public BoolExpr apply(SetExpr thisHand) {
                        return e.none(e.join(e.joinParent(thisHand), claw));
                    }
                });
        
        assertEquals(11, allSolutions(e));
    }
}
