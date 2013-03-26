//package org.clafer.constraint;
//
//import choco.Choco;
//import choco.cp.model.CPModel;
//import choco.cp.model.managers.IntConstraintManager;
//import choco.cp.solver.CPSolver;
//import choco.cp.solver.search.integer.branching.AssignVar;
//import choco.cp.solver.search.integer.valiterator.IncreasingDomain;
//import choco.cp.solver.search.integer.varselector.MinDomain;
//import choco.cp.solver.search.set.AssignSetVar;
//import choco.cp.solver.search.set.MinDomSet;
//import choco.cp.solver.search.set.MinEnv;
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
//public class SelectNManager extends IntConstraintManager {
//
//    @Override
//    public SConstraint makeConstraint(Solver solver, IntegerVariable[] variables, Object parameters, List<String> options) {
//        if (solver instanceof CPSolver) {
//            IntegerVariable n = variables[0];
//            IntegerVariable[] bools = Arrays.copyOfRange(variables, 1, variables.length);
//            return new SelectN(solver.getVar(bools), solver.getVar(n));
//        }
//        throw new ModelException("Could not found a constraint manager in " + this.getClass() + " !");
//    }
//
//    public static Constraint selectN(IntegerVariable[] bools, IntegerVariable n) {
//        return new ComponentConstraint(
//                SelectNManager.class,
//                null,
//                Util.cons(n, bools));
//    }
//
//    public static void main(String[] args) {
//        Model m = new CPModel();
//
//        IntegerVariable[] bools = Choco.makeBooleanVarArray("bools", 10);
//        IntegerVariable n = Choco.makeIntVar("n", 0, 10);
//
//        m.addConstraint(selectN(bools, n));
//
//        System.out.println(Util.allSolutions(m).getStatistics());
//
//        Solver solver = Util.newSolver(m);
//        solver.addGoal(new AssignVar(new MinDomain(solver), new IncreasingDomain()));
//        solver.addGoal(new AssignSetVar(new MinDomSet(solver), new MinEnv()));
//        System.out.println(Util.allSolutions(solver).getStatistics());
//        
//        // #11 solutions 5 Time (ms), 21 Nodes, 20 Backtracks, 0 Restarts - 
//        // #11 solutions 0 Time (ms), 21 Nodes, 20 Backtracks, 0 Restarts - 
//    }
//}
