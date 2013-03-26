//package org.clafer.constraint;
//
//import choco.Choco;
//import choco.Options;
//import choco.cp.model.CPModel;
//import choco.cp.model.managers.MixedConstraintManager;
//import choco.cp.solver.CPSolver;
//import choco.cp.solver.search.integer.branching.AssignVar;
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
//public class SingletonManager extends MixedConstraintManager {
//
//    @Override
//    public SConstraint makeConstraint(Solver solver, Variable[] vars, Object parameters, List<String> options) {
//        if (solver instanceof CPSolver) {
//            return new Singleton(solver.getVar((SetVariable) vars[0]), solver.getVar((IntegerVariable) vars[1]));
//        }
//        throw new ModelException("Could not found a constraint manager in " + this.getClass() + " !");
//    }
//
//    public static Constraint singleton(IntegerVariable i, SetVariable s) {
//        return singleton(s, i);
//    }
//
//    public static Constraint singleton(SetVariable s, IntegerVariable i) {
//        return new ComponentConstraint(SingletonManager.class, null, new Variable[]{s, i});
//    }
//
//    public static void main(String[] args) {
//        Model m = new CPModel();
//
//        IntegerVariable i = Choco.makeIntVar("i", 0, 100);
//        SetVariable s = Choco.makeSetVar("set", 0, 1000);
//
//        m.addConstraint(singleton(i, s));
//
//        System.out.println(Util.allSolutions(m).getStatistics());
//
//        Solver solver = Util.newSolver(m);
//        solver.addGoal(new AssignVar(new MinDomain(solver), new IncreasingDomain()));
//        solver.addGoal(new AssignSetVar(new MinDomSet(solver), new MinEnv()));
//        System.out.println(Util.allSolutions(solver).getStatistics());
//        
//        // #101 solutions 13 Time (ms), 201 Nodes, 200 Backtracks, 0 Restarts - 
//        // #101 solutions 6 Time (ms), 102 Nodes, 101 Backtracks, 0 Restarts - 
//    }
//}
