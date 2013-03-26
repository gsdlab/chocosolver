//package org.clafer.constraint;
//
//import choco.Choco;
//import choco.cp.model.CPModel;
//import choco.cp.model.managers.MixedConstraintManager;
//import choco.cp.solver.CPSolver;
//import choco.cp.solver.search.integer.branching.AssignVar;
//import choco.cp.solver.search.integer.valiterator.DecreasingDomain;
//import choco.cp.solver.search.integer.valiterator.IncreasingDomain;
//import choco.cp.solver.search.integer.varselector.MinDomain;
//import choco.cp.solver.search.set.AssignSetVar;
//import choco.cp.solver.search.set.MinDomSet;
//import choco.cp.solver.search.set.MinEnv;
//import choco.kernel.common.logging.ChocoLogging;
//import choco.kernel.model.Model;
//import choco.kernel.model.ModelException;
//import choco.kernel.model.constraints.ComponentConstraint;
//import choco.kernel.model.constraints.Constraint;
//import choco.kernel.model.variables.Variable;
//import choco.kernel.model.variables.integer.IntegerVariable;
//import choco.kernel.model.variables.set.SetVariable;
//import choco.kernel.solver.Solver;
//import choco.kernel.solver.constraints.SConstraint;
//import java.util.List;
//import org.clafer.Util;
//
///**
// *
// * @author jimmy
// */
//public class SumSetManager extends MixedConstraintManager {
//
//    @Override
//    public SConstraint makeConstraint(Solver solver, Variable[] vars, Object parameters, List<String> options) {
//        if (solver instanceof CPSolver) {
//            return new SumSet(solver.getVar((SetVariable) vars[0]), solver.getVar((IntegerVariable) vars[1]));
//        }
//        throw new ModelException("Could not found a constraint manager in " + this.getClass() + " !");
//    }
//
//    public static Constraint sumSet(IntegerVariable i, SetVariable s) {
//        return sumSet(s, i);
//    }
//
//    public static Constraint sumSet(SetVariable s, IntegerVariable i) {
//        return new ComponentConstraint(SumSetManager.class, null, new Variable[]{s, i});
//    }
//
//    public static void main(String[] args) {
//        Model m = new CPModel();
//
//        IntegerVariable sum = Choco.makeIntVar("sum", 0, 50);
//        SetVariable set = Choco.makeSetVar("set", 0, 10);
//
//        m.addConstraint(sumSet(set, sum));
//
//        System.out.println(Util.allSolutions(m).getStatistics());
//
//        Solver solver = Util.newSolver(m);
//        solver.addGoal(new AssignVar(new MinDomain(solver), new IncreasingDomain()));
//        solver.addGoal(new AssignSetVar(new MinDomSet(solver), new MinEnv()));
//        System.out.println(Util.allSolutions(solver).getStatistics());
//
//        // pruneMinMaxSumSet
//        // #2034 solutions 401 Time (ms), 4081 Nodes, 4094 Backtracks, 0 Restarts - 
//        // #2034 solutions 1255 Time (ms), 11854 Nodes, 19857 Backtracks, 0 Restarts - 
//        // pruneSet1Left
//        // #2034 solutions 601 Time (ms), 4067 Nodes, 4066 Backtracks, 0 Restarts - 
//        // #2034 solutions 837 Time (ms), 8795 Nodes, 13739 Backtracks, 0 Restarts - 
//        // pruneSet2Left
//        // #2034 solutions 402 Time (ms), 4067 Nodes, 4066 Backtracks, 0 Restarts - 
//        // #2034 solutions 823 Time (ms), 7611 Nodes, 11371 Backtracks, 0 Restarts -
//        // prune cards - helps when sum is branched before card, which isn't the
//        // the case for either runs, hence no improvement.
//        // #2034 solutions 601 Time (ms), 4067 Nodes, 4066 Backtracks, 0 Restarts - 
//        // #2034 solutions 1072 Time (ms), 7611 Nodes, 11371 Backtracks, 0 Restarts - 
//    }
//}
