package org.clafer.choco.constraint;

import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import org.clafer.collection.Pair;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
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
public class LexChainChannelTest extends ConstraintTest<Pair<IntVar[][], IntVar[]>> {

    @Override
    protected void check(Pair<IntVar[][], IntVar[]> s) {
        int[][] strings = new int[s.getFst().length][];
        int[] ints = getValues(s.getSnd());
        for (int i = 0; i < strings.length; i++) {
            strings[i] = getValues(s.getFst()[i]);
        }

        for (int i = 0; i < strings.length; i++) {
            for (int j = i + 1; j < strings.length; j++) {
                assertEquals(compare(strings[i], strings[j]), compare(ints[i], ints[j]));
            }
        }
        int[] sorted = new TIntHashSet(ints).toArray();
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
        randomizedTest(new TestCase<Pair<IntVar[][], IntVar[]>>() {
            @Override
            public Pair<Constraint, Pair<IntVar[][], IntVar[]>> setup(Solver solver) {
                int m = nextInt(3) + 1;
                int n = nextInt(3) + 1;
                IntVar[][] strings = new IntVar[m][];
                for (int i = 0; i < strings.length; i++) {
                    strings[i] = toIntVars(randInts(n), solver);
                }
                IntVar[] ints = toIntVars(randPositiveInts(m), solver);
                return pair(Constraints.lexChainChannel(strings, ints), pair(strings, ints));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLexChainChannel() {
        /*
         * import Control.Monad
         * 
         * positive = do
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
         * 
         * negative = (3^2)^3 * 3^3 - length positive
         */
        randomizedTest(new TestCase<Pair<IntVar[][], IntVar[]>>() {
            @PositiveSolutions(729)
            @NegativeSolutions(18954)
            @Override
            public Pair<Constraint, Pair<IntVar[][], IntVar[]>> setup(Solver solver) {
                IntVar[][] strings = new IntVar[3][];
                for (int i = 0; i < strings.length; i++) {
                    strings[i] = VF.enumeratedArray("string" + i, 2, 0, 2, solver);
                }
                IntVar[] ints = VF.enumeratedArray("int", 3, 0, 2, solver);
                return pair(Constraints.lexChainChannel(strings, ints),
                        pair(strings, ints));
            }
        });
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
        IntVar foodCard = enforcedCardVar(food, 1, 3);
        SetVar food0 = VF.set("food0", new int[]{0, 1, 2}, solver);
        IntVar food0Card = enforcedCardVar(food0, 1, 3);
        SetVar food1 = VF.set("food1", new int[]{0, 1, 2}, solver);
        IntVar food1Card = enforcedCardVar(food1, 1, 3);
        IntVar[] foodParent = VF.enumeratedArray("foodparent", 3, 0, 2, solver);
        SetVar unusedFood = VF.set("foodunused", new int[]{0, 1, 2}, solver);

        solver.post(Constraints.union(new SetVar[]{food0, food1}, new IntVar[]{food0Card, food1Card},
                food, foodCard, true));
        solver.post(Constraints.intChannel(new SetVar[]{food0, food1, unusedFood}, foodParent));
        solver.post(ICF.arithm(foodParent[0], "<=", foodParent[1]));
        solver.post(ICF.arithm(foodParent[1], "<=", foodParent[2]));

        SetVar cheese = VF.set("cheese0", new int[]{0, 1, 2}, solver);
        IntVar cheeseCard = enforcedCardVar(cheese, 0, 3);
        SetVar cheese0 = VF.set("cheese0", new int[]{0, 1, 2}, solver);
        IntVar cheese0Card = enforcedCardVar(cheese0, 0, 3);
        SetVar cheese1 = VF.set("cheese1", new int[]{0, 1, 2}, solver);
        IntVar cheese1Card = enforcedCardVar(cheese1, 0, 3);
        SetVar cheese2 = VF.set("cheese2", new int[]{0, 1, 2}, solver);
        IntVar cheese2Card = enforcedCardVar(cheese2, 0, 3);
        IntVar[] cheeseParent = VF.enumeratedArray("cheeseparent", 3, 0, 3, solver);
        SetVar unusedCheese = VF.set("cheeseunused", new int[]{0, 1, 2}, solver);

        solver.post(Constraints.union(new SetVar[]{cheese0, cheese1, cheese2}, new IntVar[]{cheese0Card, cheese1Card, cheese2Card},
                cheese, cheeseCard, true));
        solver.post(Constraints.intChannel(new SetVar[]{cheese0, cheese1, cheese2, unusedCheese}, cheeseParent));
        solver.post(ICF.arithm(cheeseParent[0], "<=", cheeseParent[1]));
        solver.post(ICF.arithm(cheeseParent[1], "<=", cheeseParent[2]));
        solver.post(LCF.ifThen(SCF.member(VF.fixed(0, solver), food).getOpposite(),
                ICF.arithm(cheese0Card, "=", 0)));
        solver.post(LCF.ifThen(SCF.member(VF.fixed(1, solver), food).getOpposite(),
                ICF.arithm(cheese1Card, "=", 0)));
        solver.post(LCF.ifThen(SCF.member(VF.fixed(2, solver), food).getOpposite(),
                ICF.arithm(cheese2Card, "=", 0)));

        IntVar[] cheeseWeight = VF.enumeratedArray("cheeseweight", 3, 0, 0, solver);
        IntVar[] cheese0Index = VF.enumeratedArray("cheese0index", 3, -1, 2, solver);
        IntVar[] cheese1Index = VF.enumeratedArray("cheese1index", 3, -1, 2, solver);
        IntVar[] cheese2Index = VF.enumeratedArray("cheese2index", 3, -1, 2, solver);
        solver.post(Constraints.filterString(cheese0, cheese0Card, 0, cheeseWeight, cheese0Index));
        solver.post(Constraints.filterString(cheese1, cheese1Card, 0, cheeseWeight, cheese1Index));
        solver.post(Constraints.filterString(cheese2, cheese2Card, 0, cheeseWeight, cheese2Index));

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
        solver.post(Constraints.filterString(food0, food0Card, 0, foodWeight, food0Index));
        solver.post(Constraints.filterString(food1, food1Card, 0, foodWeight, food1Index));

        IntVar[] patronWeight = VF.enumeratedArray("patronWeight", 2, 0, 1, solver);
        solver.post(Constraints.lexChainChannel(new IntVar[][]{food0Index, food1Index}, patronWeight));

        solver.post(ICF.arithm(patronWeight[0], ">=", patronWeight[1]));

        assertEquals(19, randomizeStrategy(solver).findAllSolutions());
    }
}
