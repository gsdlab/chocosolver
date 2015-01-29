package org.clafer.choco.constraint;

import org.clafer.test.TestUtil;
import org.chocosolver.solver.variables.CSetVar;
import static org.junit.Assert.*;
import org.junit.Test;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.constraints.LCF;
import org.chocosolver.solver.constraints.set.SCF;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
public class SymmetryBreakingTest {
    /**
     * <pre>
     * Patron 2
     *     Food +
     *         Cheese *
     * </pre>
     *
     * default scope = 3
     */
    @Test(timeout = 60000)
    public void testBreakSymmetryWithLexChain() {
        Solver solver = new Solver();

        CSetVar food = cset("food", env(0, 1, 2), ker(), card(1,2,3), solver);
        CSetVar food0 = cset("food0", env(0, 1, 2), ker(), card(1,2,3), solver);
        CSetVar food1 = cset("food1", env(0, 1, 2), ker(), card(1,2,3), solver);
        IntVar[] foodParent = enumeratedArray("foodparent", 3, 0, 2, solver);
        SetVar unusedFood = set("foodunused", new int[]{0, 1, 2}, solver);

        solver.post(Constraints.union(mapSet(food0, food1), mapCard(food0, food1),
                food.getSet(), food.getCard(), true));
        solver.post(Constraints.intChannel(new SetVar[]{food0.getSet(), food1.getSet(), unusedFood}, foodParent));
        solver.post(ICF.arithm(foodParent[0], "<=", foodParent[1]));
        solver.post(ICF.arithm(foodParent[1], "<=", foodParent[2]));

        CSetVar cheese = cset("cheese0", new int[]{0, 1, 2}, solver);
        CSetVar cheese0 = cset("cheese0", new int[]{0, 1, 2}, solver);
        CSetVar cheese1 = cset("cheese1", new int[]{0, 1, 2}, solver);
        CSetVar cheese2 = cset("cheese2", new int[]{0, 1, 2}, solver);
        IntVar[] cheeseParent = enumeratedArray("cheeseparent", 3, 0, 3, solver);
        SetVar unusedCheese = set("cheeseunused", new int[]{0, 1, 2}, solver);

        solver.post(Constraints.union(mapSet(cheese0, cheese1, cheese2), mapCard(cheese0, cheese1, cheese2),
                cheese.getSet(), cheese.getCard(), true));
        solver.post(Constraints.intChannel(new SetVar[]{cheese0.getSet(), cheese1.getSet(), cheese2.getSet(), unusedCheese}, cheeseParent));
        solver.post(ICF.arithm(cheeseParent[0], "<=", cheeseParent[1]));
        solver.post(ICF.arithm(cheeseParent[1], "<=", cheeseParent[2]));
        LCF.ifThen(SCF.member(fixed(0, solver), food.getSet()).getOpposite(),
                ICF.arithm(cheese0.getCard(), "=", 0));
        LCF.ifThen(SCF.member(fixed(1, solver), food.getSet()).getOpposite(),
                ICF.arithm(cheese1.getCard(), "=", 0));
        LCF.ifThen(SCF.member(fixed(2, solver), food.getSet()).getOpposite(),
                ICF.arithm(cheese2.getCard(), "=", 0));

        IntVar[] cheeseWeight = enumeratedArray("cheeseweight", 3, 0, 0, solver);
        IntVar[] cheese0Index = enumeratedArray("cheese0index", 3, -1, 2, solver);
        IntVar[] cheese1Index = enumeratedArray("cheese1index", 3, -1, 2, solver);
        IntVar[] cheese2Index = enumeratedArray("cheese2index", 3, -1, 2, solver);
        solver.post(Constraints.filterString(cheese0.getSet(), cheese0.getCard(), 0, cheeseWeight, cheese0Index));
        solver.post(Constraints.filterString(cheese1.getSet(), cheese1.getCard(), 0, cheeseWeight, cheese1Index));
        solver.post(Constraints.filterString(cheese2.getSet(), cheese2.getCard(), 0, cheeseWeight, cheese2Index));

        IntVar[] foodWeight = enumeratedArray("foodweight", 3, 0, 2, solver);
        solver.post(Constraints.lexChainChannel(new IntVar[][]{cheese0Index, cheese1Index, cheese2Index}, foodWeight));

        LCF.ifThen(ICF.arithm(foodParent[0], "=", foodParent[1]),
                ICF.arithm(foodWeight[0], ">=", foodWeight[1]));
        LCF.ifThen(ICF.arithm(foodParent[0], "=", foodParent[2]),
                ICF.arithm(foodWeight[0], ">=", foodWeight[2]));
        LCF.ifThen(ICF.arithm(foodParent[1], "=", foodParent[2]),
                ICF.arithm(foodWeight[1], ">=", foodWeight[2]));

        IntVar[] food0Index = enumeratedArray("food0index", 3, -1, 2, solver);
        IntVar[] food1Index = enumeratedArray("food2index", 3, -1, 2, solver);
        solver.post(Constraints.filterString(food0.getSet(), food0.getCard(), 0, foodWeight, food0Index));
        solver.post(Constraints.filterString(food1.getSet(), food1.getCard(), 0, foodWeight, food1Index));

        IntVar[] patronWeight = enumeratedArray("patronWeight", 2, 0, 1, solver);
        solver.post(Constraints.lexChainChannel(new IntVar[][]{food0Index, food1Index}, patronWeight));

        solver.post(ICF.arithm(patronWeight[0], ">=", patronWeight[1]));

        assertEquals(19, TestUtil.randomizeStrategy(solver).findAllSolutions());
    }
}
