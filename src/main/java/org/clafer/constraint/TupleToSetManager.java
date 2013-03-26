//package org.clafer.constraint;
//
//import choco.Choco;
//import choco.cp.model.CPModel;
//import choco.cp.model.managers.MixedConstraintManager;
//import choco.cp.solver.CPSolver;
//import choco.cp.solver.search.integer.branching.AssignVar;
//import choco.cp.solver.search.integer.valiterator.IncreasingDomain;
//import choco.cp.solver.search.integer.varselector.MinDomain;
//import choco.cp.solver.search.set.AssignSetVar;
//import choco.cp.solver.search.set.MinDomSet;
//import choco.cp.solver.search.set.MinEnv;
//import choco.kernel.common.util.tools.VariableUtils;
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
//public class TupleToSetManager extends MixedConstraintManager {
//
//    @Override
//    public SConstraint makeConstraint(Solver solver, Variable[] variables, Object parameters, List<String> options) {
//        if (solver instanceof CPSolver) {
//            return new TupleToSet(
//                    VariableUtils.getIntVar(solver, variables, 1, variables.length),
//                    solver.getVar((SetVariable) variables[0]));
//        }
//        throw new ModelException("Could not found a constraint manager in " + this.getClass() + " !");
//    }
//
//    public static Constraint tupleToSet(IntegerVariable[] tuple, SetVariable set) {
//        if (tuple.length == 0) {
//            throw new IllegalArgumentException();
//        }
//        Variable[] vars = new Variable[tuple.length + 1];
//        vars[0] = set;
//        System.arraycopy(tuple, 0, vars, 1, tuple.length);
//        return new ComponentConstraint(TupleToSetManager.class, null, vars);
//    }
//
//    public static void main(String[] args) {
//        Model m = new CPModel();
//
//        IntegerVariable[] tuple = Choco.makeIntVarArray("tuple", 3, 0, 5);
//        SetVariable set = Choco.makeSetVar("set", 0, 5);
//
//        m.addConstraint(tupleToSet(tuple, set));
//
//        System.out.println(Util.allSolutions(m).getStatistics());
//
//        Solver solver = Util.newSolver(m);
//        solver.addGoal(new AssignVar(new MinDomain(solver), new IncreasingDomain()));
//        solver.addGoal(new AssignSetVar(new MinDomSet(solver), new MinEnv()));
//        System.out.println(Util.allSolutions(solver).getStatistics());
//        // #120 solutions 59 Time (ms), 725 Nodes, 1600 Backtracks, 0 Restarts - 
//        // #120 solutions 13 Time (ms), 637 Nodes, 1356 Backtracks, 0 Restarts -    
//        // pickTuple
//        // #120 solutions 30 Time (ms), 395 Nodes, 760 Backtracks, 0 Restarts - 
//        // #120 solutions 39 Time (ms), 637 Nodes, 1356 Backtracks, 0 Restarts - 
//        // pickSet
//        // #120 solutions 30 Time (ms), 395 Nodes, 760 Backtracks, 0 Restarts - 
//        // #120 solutions 26 Time (ms), 157 Nodes, 156 Backtracks, 0 Restarts -
//        // set card
//        // #120 solutions 5 Time (ms), 219 Nodes, 218 Backtracks, 0 Restarts - 
//        // #120 solutions 2 Time (ms), 157 Nodes, 156 Backtracks, 0 Restarts - 
//    }
//}
