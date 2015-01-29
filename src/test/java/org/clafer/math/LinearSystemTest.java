package org.clafer.math;

import org.clafer.common.Util;
import static org.clafer.math.LinearEquation.*;
import static org.clafer.math.LinearFunctionBuilder.constant;
import static org.clafer.math.LinearFunctionBuilder.term;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class LinearSystemTest {

    @Test
    public void testCommonSubexpression() {
        Variable sil1 = new Variable("sil1", 0, 4);
        Variable sil2 = new Variable("sil2", 0, 4);
        Variable sil3 = new Variable("sil3", 0, 4);
        Variable sil4 = new Variable("sil4", 0, 4);
        Variable sil5 = new Variable("sil5", 0, 4);
        Variable sil6 = new Variable("sil6", 0, 4);
        Variable cost1 = new Variable("cost1", 0, 4);
        Variable cost2 = new Variable("cost2", 0, 4);
        Variable cost3 = new Variable("cost3", 0, 4);
        Variable cost4 = new Variable("cost4", 0, 4);
        Variable cost5 = new Variable("cost5", 0, 4);
        Variable cost6 = new Variable("cost6", 0, 4);
        Variable totalCost = new Variable("totalCost", 0, 100);

        LinearSystem s = new LinearSystem(
                greaterThanEqual(term(sil1).plusTerm(sil2), constant(4)),
                greaterThanEqual(term(sil3).plusTerm(sil4), constant(4)),
                greaterThanEqual(term(sil5).plusTerm(sil6), constant(4)),
                equal(term(sil1).plusConstant(1), term(cost1)),
                equal(term(sil2).plusConstant(1), term(cost2)),
                equal(term(sil3).plusConstant(1), term(cost3)),
                equal(term(sil4).plusConstant(1), term(cost4)),
                equal(term(sil5).plusConstant(1), term(cost5)),
                equal(term(sil6).plusConstant(1), term(cost6)),
                equal(term(cost1).plusTerm(cost2).plusTerm(cost3).plusTerm(cost4).plusTerm(cost5).plusTerm(cost6), term(totalCost))
        );
        assertTrue(Util.in(greaterThanEqual(term(totalCost), constant(18)),
                s.equalityElimination().fourierMotzkinElimination().strengthenInequalities()
                .gaussJordanElimination().dominantElimination().getEquations()));
    }

    // Currently fails.
    @Test
    public void testTransitiveInequality() {
        Variable sil1 = new Variable("sil1", 0, 4);
        Variable sil2 = new Variable("sil2", 0, 4);
        Variable sil3 = new Variable("sil3", 0, 4);
        Variable sil4 = new Variable("sil4", 0, 4);
        Variable sil5 = new Variable("sil5", 0, 4);
        Variable sil6 = new Variable("sil6", 0, 4);
        Variable cost1 = new Variable("cost1", 1, 5);
        Variable cost2 = new Variable("cost2", 1, 5);
        Variable cost3 = new Variable("cost3", 1, 5);
        Variable cost4 = new Variable("cost4", 1, 5);
        Variable cost5 = new Variable("cost5", 1, 5);
        Variable cost6 = new Variable("cost6", 1, 5);
        Variable totalCost = new Variable("totalCost", 0, 100);

        LinearSystem s = new LinearSystem(
                greaterThanEqual(term(sil1).plusTerm(sil2), constant(4)),
                greaterThanEqual(term(sil3).plusTerm(sil4), constant(4)),
                greaterThanEqual(term(sil5).plusTerm(sil6), constant(4)),
                lessThanEqual(term(sil1).plusConstant(1), term(cost1)),
                lessThanEqual(term(sil2).plusConstant(1), term(cost2)),
                lessThanEqual(term(sil3).plusConstant(1), term(cost3)),
                lessThanEqual(term(sil4).plusConstant(1), term(cost4)),
                lessThanEqual(term(sil5).plusConstant(1), term(cost5)),
                lessThanEqual(term(sil6).plusConstant(1), term(cost6)),
                equal(term(cost1).plusTerm(cost2).plusTerm(cost3).plusTerm(cost4).plusTerm(cost5).plusTerm(cost6), term(totalCost))
        );
        assertTrue(Util.in(greaterThanEqual(term(totalCost), constant(18)),
                s.equalityElimination().fourierMotzkinElimination().strengthenInequalities()
                .gaussJordanElimination().dominantElimination().getEquations()));
    }
}
