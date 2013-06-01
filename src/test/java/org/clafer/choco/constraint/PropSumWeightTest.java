package org.clafer.choco.constraint;

import java.util.Arrays;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.ICF;
import solver.constraints.LCF;
import solver.constraints.set.SCF;
import solver.search.strategy.SetStrategyFactory;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class PropSumWeightTest extends ConstraintTest {

    /**
     * <pre>
     * Patron 2
     *     Food +
     *         Cheese *
     * </pre>
     *
     * default scope = 3
     */
//    @Test(timeout = 60000)
//    public void testBreakSymmetry() {
//        Solver solver = new Solver();
//
//        int patronWeight = 13;
//        int foodWeight = 4;
//        int cheeseWeight = 1;
//
//        SetVar food = VF.set("food", new int[]{0, 1, 2}, solver);
//        SetVar food0 = VF.set("food0", new int[]{0, 1, 2}, solver);
//        IntVar food0Card = VF.enumerated("|food0|", 1, 3, solver);
//        SetVar food1 = VF.set("food1", new int[]{0, 1, 2}, solver);
//        IntVar food1Card = VF.enumerated("|food1|", 1, 3, solver);
//        IntVar[] foodParent = VF.enumeratedArray("foodparent", 3, 0, 2, solver);
//        SetVar unusedFood = VF.set("foodunused", new int[]{0, 1, 2}, solver);
//
//        solver.post(Constraints.union(new SetVar[]{food0, food1}, food));
//        solver.post(SCF.cardinality(food0, food0Card));
//        solver.post(SCF.cardinality(food1, food1Card));
//        solver.post(Constraints.intChannel(new SetVar[]{food0, food1, unusedFood}, foodParent));
//        solver.post(ICF.arithm(foodParent[0], "<=", foodParent[1]));
//        solver.post(ICF.arithm(foodParent[1], "<=", foodParent[2]));
//
//        SetVar cheese = VF.set("cheese0", new int[]{0, 1, 2}, solver);
//        SetVar cheese0 = VF.set("cheese0", new int[]{0, 1, 2}, solver);
//        IntVar cheese0Card = VF.enumerated("|cheese0|", 0, 3, solver);
//        SetVar cheese1 = VF.set("cheese1", new int[]{0, 1, 2}, solver);
//        IntVar cheese1Card = VF.enumerated("|cheese1|", 0, 3, solver);
//        SetVar cheese2 = VF.set("cheese2", new int[]{0, 1, 2}, solver);
//        IntVar cheese2Card = VF.enumerated("|cheese2|", 0, 3, solver);
//        IntVar[] cheeseParent = VF.enumeratedArray("cheeseparent", 3, 0, 3, solver);
//        SetVar unusedCheese = VF.set("cheeseunused", new int[]{0, 1, 2}, solver);
//
//        solver.post(Constraints.union(new SetVar[]{cheese0, cheese1, cheese2}, cheese));
//        solver.post(SCF.cardinality(cheese0, cheese0Card));
//        solver.post(SCF.cardinality(cheese1, cheese1Card));
//        solver.post(SCF.cardinality(cheese2, cheese2Card));
//        solver.post(Constraints.intChannel(new SetVar[]{cheese0, cheese1, cheese2, unusedCheese}, cheeseParent));
//        solver.post(ICF.arithm(cheeseParent[0], "<=", cheeseParent[1]));
//        solver.post(ICF.arithm(cheeseParent[1], "<=", cheeseParent[2]));
//        solver.post(LCF.ifThen(SCF.member(VF.fixed(0, solver), food).reif().not(),
//                ICF.arithm(cheese0Card, "=", 0)));
//        solver.post(LCF.ifThen(SCF.member(VF.fixed(1, solver), food).reif().not(),
//                ICF.arithm(cheese1Card, "=", 0)));
//        solver.post(LCF.ifThen(SCF.member(VF.fixed(2, solver), food).reif().not(),
//                ICF.arithm(cheese2Card, "=", 0)));
//
//        solver.post(ICF.arithm(food0Card, ">=", food1Card));
////        solver.post(LCF.ifThen(ICF.arithm(foodParent[0], "=", foodParent[1]),
////                ICF.arithm(cheese0Card, ">=", cheese1Card)));
////        solver.post(LCF.ifThen(ICF.arithm(foodParent[0], "=", foodParent[2]),
////                ICF.arithm(cheese0Card, ">=", cheese2Card)));
////        solver.post(LCF.ifThen(ICF.arithm(foodParent[1], "=", foodParent[2]),
////                ICF.arithm(cheese1Card, ">=", cheese2Card)));
//
//        IntVar[] ones = new IntVar[]{VF.fixed(1, solver), VF.fixed(1, solver), VF.fixed(1, solver)};
//        IntVar[] cheeseWeights = VF.enumeratedArray("cheeseWeight", 3, 0, 10000000, solver);
//        IntVar[] foodWeights = VF.enumeratedArray("foodWeight", 2, 0, 10000000, solver);
//        solver.post(Constraints.sumWeight(cheese0, ones, 4, cheeseWeights[0]));
//        solver.post(Constraints.sumWeight(cheese1, ones, 4, cheeseWeights[1]));
//        solver.post(Constraints.sumWeight(cheese2, ones, 4, cheeseWeights[2]));
//        solver.post(Constraints.sumWeight(food0, cheeseWeights, 1, foodWeights[0]));
//        solver.post(Constraints.sumWeight(food1, cheeseWeights, 1, foodWeights[1]));
//        solver.post(LCF.ifThen(ICF.arithm(foodParent[0], "=", foodParent[1]),
//                ICF.arithm(cheeseWeights[0], ">=", cheeseWeights[1])));
//        solver.post(LCF.ifThen(ICF.arithm(foodParent[0], "=", foodParent[2]),
//                ICF.arithm(cheeseWeights[0], ">=", cheeseWeights[2])));
//        solver.post(LCF.ifThen(ICF.arithm(foodParent[1], "=", foodParent[2]),
//                ICF.arithm(cheeseWeights[1], ">=", cheeseWeights[2])));
//        solver.post(LCF.ifThen(ICF.arithm(food0Card, "=", food1Card),
//                ICF.arithm(foodWeights[0], ">=", foodWeights[1])));
//
//        solver.set(SetStrategyFactory.setLex(new SetVar[]{food0, food1, cheese0, cheese1, cheese2}));
//        int count = 0;
//        if (solver.findSolution()) {
//            do {
//                count++;
//                System.out.println(Arrays.toString(food0.getValue()) + " " + Arrays.toString(food1.getValue())
//                        + " " + Arrays.toString(cheese0.getValue()) + " " + Arrays.toString(cheese1.getValue()) + " " + Arrays.toString(cheese2.getValue())
//                        + " : " + cheeseWeights[0].getValue() + " " + cheeseWeights[1].getValue() + " " + cheeseWeights[2].getValue()
//                        + " : " + foodWeights[0].getValue() + " " + foodWeights[1].getValue());
//            } while (solver.nextSolution());
//        }
//        System.out.println(count);
////        System.out.println(randomizeStrategy(solver).findAllSolutions());
//    }

    private static int pow(int base, int exp) {
        switch (exp) {
            case 0:
                return 1;
            case 1:
                return base;
            case 2:
                return base * base;
            default:
                return base * base * pow(base, exp - 2);
        }
    }

    @Test(timeout = 60000)
    public void testPropSumWeight() {
        /*
         * import Control.Monad
         * 
         * powerset = filterM (const [True, False])
         * 
         * solutions = do
         *     set <- powerset [0..3]
         *     weights <- sequence  $ replicate 4 [0..3]
         *     let weighted = map (weights !!) set
         *     sum <- [10..12]
         *     guard $ sumScale weighted == sum
         *     return (set, weights, weighted, sum)
         *     where
         *         sumScale = foldl (\x y -> x * scale + y) 0
         *         scale = 4
         */
        Solver solver = new Solver();

        SetVar set = VF.set("set", new int[]{0, 1, 2, 3}, solver);
        IntVar[] weights = VF.enumeratedArray("weight", 4, 0, 3, solver);
        IntVar sum = VF.enumerated("sum", 10, 12, solver);
        int scale = 4;

        solver.post(Constraints.sumWeight(set, weights, scale, sum));

        assertEquals(339, randomizeStrategy(solver).findAllSolutions());
    }
}
