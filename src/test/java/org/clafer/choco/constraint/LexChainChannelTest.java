package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.ICF;
import solver.constraints.LCF;
import solver.constraints.set.SCF;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class LexChainChannelTest extends ConstraintTest {

    private static void checkCorrectness(IntVar[][] strings, IntVar[] ints) {
        int[][] $strings = new int[strings.length][];
        TIntSet $ints = new TIntHashSet();

        for (int i = 0; i < strings.length; i++) {
            $strings[i] = new int[strings[i].length];
            for (int j = 0; j < strings[i].length; j++) {
                $strings[i][j] = strings[i][j].getValue();
            }
            $ints.add(ints[i].getValue());
        }

        for (int i = 0; i < strings.length; i++) {
            for (int j = i + 1; j < strings.length; j++) {
                assertEquals(compare($strings[i], $strings[j]), compare(ints[i].getValue(), ints[j].getValue()));
            }
        }
        int[] sorted = $ints.toArray();
        Arrays.sort(sorted);
        for (int i = 0; i < sorted.length; i++) {
            assertEquals(i, sorted[i]);
        }
    }

    private static int compare(int[] x, int[] y) {
        for (int i = 0; i < x.length; i++) {
            if (x[i] < y[i]) {
                return -1;
            }
            if (x[i] > y[i]) {
                return 1;
            }
        }
        return 0;
    }

    private static int compare(int x, int y) {
        return (x < y) ? -1 : ((x == y) ? 0 : 1);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();
            int m = nextInt(5) + 1;
            int n = nextInt(5) + 1;

            IntVar[][] strings = new IntVar[m][];
            for (int i = 0; i < strings.length; i++) {
                strings[i] = VF.enumeratedArray("string" + i, n, -nextInt(3), nextInt(3), solver);
            }
            IntVar[] ints = VF.enumeratedArray("int", m, 0, m, solver);

            solver.post(Constraints.lexChainChannel(strings, ints));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(strings, ints);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(strings, ints);
            }
        }
    }

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

        SetVar food = VF.set("food", new int[]{0, 1, 2}, solver);
        IntVar foodCard = VF.enumerated("|food|", 1, 3, solver);
        SetVar food0 = VF.set("food0", new int[]{0, 1, 2}, solver);
        IntVar food0Card = VF.enumerated("|food0|", 1, 3, solver);
        SetVar food1 = VF.set("food1", new int[]{0, 1, 2}, solver);
        IntVar food1Card = VF.enumerated("|food1|", 1, 3, solver);
        IntVar[] foodParent = VF.enumeratedArray("foodparent", 3, 0, 2, solver);
        SetVar unusedFood = VF.set("foodunused", new int[]{0, 1, 2}, solver);

        solver.post(Constraints.union(new SetVar[]{food0, food1}, new IntVar[]{food0Card, food1Card},
                food, foodCard));
        solver.post(SCF.cardinality(food0, food0Card));
        solver.post(SCF.cardinality(food1, food1Card));
        solver.post(Constraints.intChannel(new SetVar[]{food0, food1, unusedFood}, foodParent));
        solver.post(ICF.arithm(foodParent[0], "<=", foodParent[1]));
        solver.post(ICF.arithm(foodParent[1], "<=", foodParent[2]));

        SetVar cheese = VF.set("cheese0", new int[]{0, 1, 2}, solver);
        IntVar cheeseCard = VF.enumerated("|cheese|", 0, 3, solver);
        SetVar cheese0 = VF.set("cheese0", new int[]{0, 1, 2}, solver);
        IntVar cheese0Card = VF.enumerated("|cheese0|", 0, 3, solver);
        SetVar cheese1 = VF.set("cheese1", new int[]{0, 1, 2}, solver);
        IntVar cheese1Card = VF.enumerated("|cheese1|", 0, 3, solver);
        SetVar cheese2 = VF.set("cheese2", new int[]{0, 1, 2}, solver);
        IntVar cheese2Card = VF.enumerated("|cheese2|", 0, 3, solver);
        IntVar[] cheeseParent = VF.enumeratedArray("cheeseparent", 3, 0, 3, solver);
        SetVar unusedCheese = VF.set("cheeseunused", new int[]{0, 1, 2}, solver);

        solver.post(Constraints.union(new SetVar[]{cheese0, cheese1, cheese2}, new IntVar[]{cheese0Card, cheese1Card, cheese2Card},
                cheese, cheeseCard));
        solver.post(SCF.cardinality(cheese0, cheese0Card));
        solver.post(SCF.cardinality(cheese1, cheese1Card));
        solver.post(SCF.cardinality(cheese2, cheese2Card));
        solver.post(Constraints.intChannel(new SetVar[]{cheese0, cheese1, cheese2, unusedCheese}, cheeseParent));
        solver.post(ICF.arithm(cheeseParent[0], "<=", cheeseParent[1]));
        solver.post(ICF.arithm(cheeseParent[1], "<=", cheeseParent[2]));
        solver.post(LCF.ifThen(SCF.member(VF.fixed(0, solver), food).reif().not(),
                ICF.arithm(cheese0Card, "=", 0)));
        solver.post(LCF.ifThen(SCF.member(VF.fixed(1, solver), food).reif().not(),
                ICF.arithm(cheese1Card, "=", 0)));
        solver.post(LCF.ifThen(SCF.member(VF.fixed(2, solver), food).reif().not(),
                ICF.arithm(cheese2Card, "=", 0)));

        IntVar[] cheeseWeight = VF.enumeratedArray("cheeseweight", 3, 0, 0, solver);
        IntVar[] cheese0Index = VF.enumeratedArray("cheese0index", 3, -1, 2, solver);
        IntVar[] cheese1Index = VF.enumeratedArray("cheese1index", 3, -1, 2, solver);
        IntVar[] cheese2Index = VF.enumeratedArray("cheese2index", 3, -1, 2, solver);
        solver.post(Constraints.filterString(cheese0, 0, cheeseWeight, cheese0Index));
        solver.post(Constraints.filterString(cheese1, 0, cheeseWeight, cheese1Index));
        solver.post(Constraints.filterString(cheese2, 0, cheeseWeight, cheese2Index));

        IntVar[] foodWeight = VF.enumeratedArray("foodweight", 3, 0, 2, solver);
        solver.post(Constraints.lexChainChannel(new IntVar[][]{cheese0Index, cheese1Index, cheese2Index}, foodWeight));

        solver.post(LCF.ifThen(ICF.arithm(foodParent[0], "=", foodParent[1]),
                ICF.arithm(foodWeight[0], ">=", foodWeight[1])));
        solver.post(LCF.ifThen(ICF.arithm(foodParent[0], "=", foodParent[2]),
                ICF.arithm(foodWeight[0], ">=", foodWeight[2])));
        solver.post(LCF.ifThen(ICF.arithm(foodParent[1], "=", foodParent[2]),
                ICF.arithm(foodWeight[1], ">=", foodWeight[2])));

        IntVar[] food0Index = VF.enumeratedArray("food0index", 3, -1, 2, solver);
        IntVar[] food1Index = VF.enumeratedArray("food2index", 3, -1, 2, solver);
        solver.post(Constraints.filterString(food0, 0, foodWeight, food0Index));
        solver.post(Constraints.filterString(food1, 0, foodWeight, food1Index));

        IntVar[] patronWeight = VF.enumeratedArray("patronWeight", 2, 0, 1, solver);
        solver.post(Constraints.lexChainChannel(new IntVar[][]{food0Index, food1Index}, patronWeight));

        solver.post(ICF.arithm(patronWeight[0], ">=", patronWeight[1]));

        assertEquals(19, randomizeStrategy(solver).findAllSolutions());
    }

    @Test(timeout = 60000)
    public void testLexChainChannel() {
        /*
         * import Control.Monad
         * 
         * solutions = do
         *     string0 <- sequence $ replicate 2 [0..2]
         *     string1 <- sequence $ replicate 2 [0..2]
         *     string2 <- sequence $ replicate 2 [0..2]
         *     int0 <- [0..2]
         *     int1 <- [0..2]
         *     int2 <- [0..2]
         * 
         *     guard $ sort (nub [int0, int1, int2]) `isPrefixOf` [0,1,2]
         * 
         *     guard $ compare string0 string1 == compare int0 int1
         *     guard $ compare string0 string2 == compare int0 int2
         *     guard $ compare string1 string2 == compare int1 int2
         *     
         *     return (string0, string1, string2, int0, int1, int2)
         */
        Solver solver = new Solver();

        IntVar[][] strings = new IntVar[3][];
        for (int i = 0; i < strings.length; i++) {
            strings[i] = VF.enumeratedArray("string" + i, 2, 0, 2, solver);
        }
        IntVar[] ints = VF.enumeratedArray("int", 3, 0, 2, solver);

        solver.post(Constraints.lexChainChannel(strings, ints));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(strings, ints);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(729, count);
    }
}
