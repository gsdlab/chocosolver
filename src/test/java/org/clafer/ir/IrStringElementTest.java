package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import org.clafer.test.Positive;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.CStringVar;
import solver.variables.IntVar;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrStringElementTest {

    //[[String1446[0]{domain={0, 97, 99}}, String1446[1]{domain={0, 97, ..., 99}}, String1446[2]{domain={0, 97, 98}}]
    //    [length=Int1447{domain={0, ..., 3}}]
    //
    //[[99, String1448[1]{domain={0, 98, 99}}, String1448[2]{domain={0, 98}}]
    //    [length=Int1449{domain={1, ..., 3}}]
    //
    //[99, String1450[1]{domain={97, 98}}]
    //    [length=2]
    //
    //[99, String1452[1]{domain={97, 99}}, 97, String1452[3]{domain={97, 98}}]
    //    [length=4]]
    //
    //Int1454{domain={1, 2}}]
    public static void main(String[] args) {
        IrStringVar s0 = string("s0",
                new IrIntVar[]{
                    enumInt("s00", 0, 97, 99),
                    enumInt("s01", 0, 97, 98, 99),
                    enumInt("s02", 0, 97, 98)},
                boundInt("|s0|", 0, 3));
        IrStringVar s1 = string("s1",
                new IrIntVar[]{
                    constant(99),
                    enumInt("s11", 0, 98, 99),
                    enumInt("s12", 0, 98)},
                boundInt("|s1|", 1, 3));
        IrStringVar s2 = string("s2",
                new IrIntVar[]{
                    constant(99),
                    enumInt("s21", 97, 98)},
                constant(2));
        IrStringVar s3 = string("s3",
                new IrIntVar[]{
                    constant(99),
                    enumInt("s31", 97, 99),
                    constant(97),
                    enumInt("s33", 97, 98)},
                constant(4));
        IrIntVar i = boundInt("i", 1, 2);
        IrModule module = new IrModule();
        module.addVariables(s0, s1, s2, s3, i);
        module.addConstraint(equal(s0, element(new IrStringVar[]{s1, s2, s3}, i)));
        System.out.println(element(new IrStringVar[]{s1, s2, s3}, i).getLength());
        System.out.println(element(new IrStringVar[]{s1, s2, s3}, i).getChars()[0]);
        System.out.println(element(new IrStringVar[]{s1, s2, s3}, i).getChars()[1]);
        System.out.println(element(new IrStringVar[]{s1, s2, s3}, i).getChars()[2]);
        System.out.println(element(new IrStringVar[]{s1, s2, s3}, i).getChars()[3]);

        Solver solver = new Solver();
        IrSolutionMap m = IrCompiler.compile(module, solver);
System.out.println(solver);
        if (solver.findSolution()) {
            do {
                System.out.println(m.getValue(s0) + " : " + m.getValue(i));
            } while (solver.nextSolution());
        }
    }

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrStringVar element, IrStringVar[] array, @Positive IrIntVar index) {
        assumeTrue(index.getHighBound() < array.length);
        return equal(element, element(array, index));
    }

    @Solution
    public Constraint setup(CStringVar element, CStringVar[] array, IntVar index) {
        return Constraints.element(index,
                mapChars(array), mapLength(array),
                element.getChars(), element.getLength());
    }
}
