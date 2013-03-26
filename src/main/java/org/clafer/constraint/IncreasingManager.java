//package org.clafer.constraint;
//
//import choco.Choco;
//import choco.cp.model.CPModel;
//import choco.cp.model.managers.IntConstraintManager;
//import choco.cp.solver.CPSolver;
//import choco.kernel.model.Model;
//import choco.kernel.model.ModelException;
//import choco.kernel.model.constraints.ComponentConstraint;
//import choco.kernel.model.constraints.Constraint;
//import choco.kernel.model.variables.integer.IntegerVariable;
//import choco.kernel.solver.Solver;
//import choco.kernel.solver.constraints.SConstraint;
//import java.util.Arrays;
//import java.util.List;
//import org.clafer.Util;
//
///**
// *
// * @author jimmy
// */
//public class IncreasingManager extends IntConstraintManager {
//
//    @Override
//    public SConstraint makeConstraint(Solver solver, IntegerVariable[] variables, Object parameters, List<String> options) {
//        if (solver instanceof CPSolver) {
//            return new Increasing(solver.getVar(variables));
//        }
//        throw new ModelException("Could not found a constraint manager in " + this.getClass() + " !");
//    }
//
//    public static Constraint increasing(IntegerVariable[] variables) {
//        switch (variables.length) {
//            case 0:
//            case 1:
//                return Choco.TRUE;
//            case 2:
//                return Choco.leq(variables[0], variables[1]);
//            default:
//                return new ComponentConstraint(IncreasingManager.class, null, variables);
//        }
//    }
//
//    public static Constraint decreasing(IntegerVariable[] variables) {
//        variables = Arrays.copyOf(variables, variables.length);
//        Util.reverse(variables);
//        return new ComponentConstraint(IncreasingManager.class, null, variables);
//    }
//
//    public static void main(String[] args) {
//        Model m = new CPModel();
//        IntegerVariable[] vars = Choco.makeIntVarArray("var", 5, 0, 10);
//
//        m.addConstraint(increasing(vars));
//
//        System.out.println(Util.allSolutions(m).getStatistics());
//        // #3003 solutions 201 Time (ms), 4004 Nodes, 4003 Backtracks, 0 Restarts -
//    }
//}
