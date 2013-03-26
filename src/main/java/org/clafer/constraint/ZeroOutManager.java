//package org.clafer.constraint;
//
//import static choco.Choco.*;
//import choco.kernel.model.constraints.Constraint;
//import choco.kernel.model.variables.integer.IntegerVariable;
//import choco.kernel.model.variables.set.SetVariable;
//
///**
// *
// * @author jimmy
// */
//public class ZeroOutManager {
//
//    /**
//     * bools[x] == false => i[x] = 0
//     */
//    public static Constraint zeroOut(IntegerVariable[] bools, IntegerVariable[] i) {
//        if (bools.length != i.length) {
//            throw new IllegalArgumentException();
//        }
//        Constraint[] cs = new Constraint[i.length];
//        for (int x = 0; x < i.length; x++) {
//            cs[x] = implies(eq(bools[x], 0), eq(i[x], 0));
//        }
//        return and(cs);
//    }
//}
