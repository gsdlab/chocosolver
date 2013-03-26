//package org.clafer.constraint;
//
//import choco.Choco;
//import choco.cp.model.CPModel;
//import choco.cp.model.managers.SetConstraintManager;
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
//import choco.kernel.model.variables.set.SetVariable;
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
//public class JoinManager extends SetConstraintManager {
//
//    @Override
//    public SConstraint makeConstraint(Solver solver, SetVariable[] vars, Object params, List<String> options) {
//        if (solver instanceof CPSolver) {
//            return new Join(
//                    solver.getVar(vars[0]),
//                    solver.getVar(Arrays.copyOfRange(vars, 1, vars.length - 1)),
//                    solver.getVar(vars[vars.length - 1]));
//        }
//        throw new ModelException("Could not found a constraint manager in " + this.getClass() + " !");
//    }
//
//    public static Constraint join(SetVariable take, SetVariable[] children, SetVariable to) {
//        SetVariable[] vars = new SetVariable[children.length + 2];
//        vars[0] = take;
//        System.arraycopy(children, 0, vars, 1, children.length);
//        vars[children.length + 1] = to;
//        return new ComponentConstraint(JoinManager.class, null, vars);
//    }
//
//    public static void main(String[] args) {
//        Model m = new CPModel();
//        SetVariable take = Choco.makeSetVar("take", 0, 2);
//        SetVariable[] children = Choco.makeSetVarArray("child", 3, 0, 4);
//        SetVariable to = Choco.makeSetVar("to", 0, 4);
//
//        m.addConstraint(join(take, children, to));
//        m.addConstraint(Choco.setDisjoint(children));
//
//        System.out.println(Util.allSolutions(m).getStatistics());
//
//        Solver solver = Util.newSolver(m);
//        solver.addGoal(new AssignVar(new MinDomain(solver), new IncreasingDomain()));
//        solver.addGoal(new AssignSetVar(new MinDomSet(solver), new MinEnv()));
//        System.out.println(Util.allSolutions(solver).getStatistics());
//        
//        // #8192 solutions 1401 Time (ms), 16383 Nodes, 16382 Backtracks, 0 Restarts - 
//        // #8192 solutions 1194 Time (ms), 45693 Nodes, 76900 Backtracks, 0 Restarts - 
//        // Last occurence
//        // #8192 solutions 1607 Time (ms), 16383 Nodes, 16382 Backtracks, 0 Restarts - 
//        // #8192 solutions 2467 Time (ms), 39392 Nodes, 64286 Backtracks, 0 Restarts -
//        // Last occurence on env
//        // #8192 solutions 1201 Time (ms), 16383 Nodes, 16382 Backtracks, 0 Restarts - 
//        // #8192 solutions 1093 Time (ms), 39250 Nodes, 63978 Backtracks, 0 Restarts - 
//        // Prune children
//        // #8192 solutions 1201 Time (ms), 16383 Nodes, 16382 Backtracks, 0 Restarts - 
//        // #8192 solutions 1025 Time (ms), 33575 Nodes, 52628 Backtracks, 0 Restarts - 
//        // Pick take also picks to
//        // #8192 solutions 1602 Time (ms), 16383 Nodes, 16382 Backtracks, 0 Restarts - 
//        // #8192 solutions 1295 Time (ms), 33511 Nodes, 52500 Backtracks, 0 Restarts - 
//    }
//}
