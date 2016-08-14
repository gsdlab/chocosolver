package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.clafer.test.TestUtil;
import static org.junit.Assert.*;
import org.junit.Test;
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
        Model model = new Model();

        SetVar food = model.setVar("food", ker(), env(0, 1, 2));
        food.setCard(model.intVar("|food|", 1, 3));
        SetVar food0 = model.setVar("food0", ker(), env(0, 1, 2));
        food0.setCard(model.intVar("|food0|", 1, 3));
        SetVar food1 = model.setVar("food1", ker(), env(0, 1, 2));
        food1.setCard(model.intVar("|food1|", 1, 3));
        IntVar[] foodParent = model.intVarArray("foodparent", 3, 0, 2);
        SetVar unusedFood = model.setVar("foodunused", ker(), env(0, 1, 2));

        Constraints.union(
                new SetVar[]{food0, food1},
                new IntVar[]{food0.getCard(), food1.getCard()},
                food, food.getCard(), true).post();
        Constraints.intChannel(new SetVar[]{food0, food1, unusedFood}, foodParent).post();
        model.arithm(foodParent[0], "<=", foodParent[1]).post();
        model.arithm(foodParent[1], "<=", foodParent[2]).post();

        SetVar cheese = model.setVar("cheese0", ker(), env(0, 1, 2));
        SetVar cheese0 = model.setVar("cheese0", ker(), env(0, 1, 2));
        SetVar cheese1 = model.setVar("cheese1", ker(), env(0, 1, 2));
        SetVar cheese2 = model.setVar("cheese2", ker(), env(0, 1, 2));
        IntVar[] cheeseParent = model.intVarArray("cheeseparent", 3, 0, 3);
        SetVar unusedCheese = model.setVar("cheeseunused", ker(), env(0, 1, 2));

        Constraints.union(
                new SetVar[]{cheese0, cheese1, cheese2},
                new IntVar[]{cheese0.getCard(), cheese1.getCard(), cheese2.getCard()},
                cheese, cheese.getCard(), true).post();
        Constraints.intChannel(new SetVar[]{cheese0, cheese1, cheese2, unusedCheese}, cheeseParent).post();
        model.arithm(cheeseParent[0], "<=", cheeseParent[1]).post();
        model.arithm(cheeseParent[1], "<=", cheeseParent[2]).post();
        model.ifThen(model.member(model.intVar(0), food).getOpposite(),
                model.arithm(cheese0.getCard(), "=", 0));
        model.ifThen(model.member(model.intVar(1), food).getOpposite(),
                model.arithm(cheese1.getCard(), "=", 0));
        model.ifThen(model.member(model.intVar(2), food).getOpposite(),
                model.arithm(cheese2.getCard(), "=", 0));

        IntVar[] cheeseWeight = model.intVarArray("cheeseweight", 3, 0, 0);
        IntVar[] cheese0Index = model.intVarArray("cheese0index", 3, -1, 2);
        IntVar[] cheese1Index = model.intVarArray("cheese1index", 3, -1, 2);
        IntVar[] cheese2Index = model.intVarArray("cheese2index", 3, -1, 2);
        Constraints.filterString(cheese0, cheese0.getCard(), 0, cheeseWeight, cheese0Index).post();
        Constraints.filterString(cheese1, cheese1.getCard(), 0, cheeseWeight, cheese1Index).post();
        Constraints.filterString(cheese2, cheese2.getCard(), 0, cheeseWeight, cheese2Index).post();

        IntVar[] foodWeight = model.intVarArray("foodweight", 3, 0, 2);
        Constraints.lexChainChannel(new IntVar[][]{cheese0Index, cheese1Index, cheese2Index}, foodWeight).post();

        model.ifThen(model.arithm(foodParent[0], "=", foodParent[1]),
                model.arithm(foodWeight[0], ">=", foodWeight[1]));
        model.ifThen(model.arithm(foodParent[0], "=", foodParent[2]),
                model.arithm(foodWeight[0], ">=", foodWeight[2]));
        model.ifThen(model.arithm(foodParent[1], "=", foodParent[2]),
                model.arithm(foodWeight[1], ">=", foodWeight[2]));

        IntVar[] food0Index = model.intVarArray("food0index", 3, -1, 2);
        IntVar[] food1Index = model.intVarArray("food2index", 3, -1, 2);
        Constraints.filterString(food0, food0.getCard(), 0, foodWeight, food0Index).post();
        Constraints.filterString(food1, food1.getCard(), 0, foodWeight, food1Index).post();

        IntVar[] patronWeight = model.intVarArray("patronWeight", 2, 0, 1);
        Constraints.lexChainChannel(new IntVar[][]{food0Index, food1Index}, patronWeight).post();

        model.arithm(patronWeight[0], ">=", patronWeight[1]).post();

        assertEquals(19, TestUtil.randomizeStrategy(model.getSolver()).findAllSolutions());
    }
}
