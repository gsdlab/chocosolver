//package org.clafer.constraint;
//
//import static choco.Choco.*;
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
//import java.util.ArrayList;
//import java.util.List;
//import org.clafer.Util;
//
///**
// *
// * @author jimmy
// */
//public class JoinRefManager extends MixedConstraintManager {
//
//    @Override
//    public SConstraint makeConstraint(Solver solver, Variable[] vars, Object params, List<String> options) {
//        if (solver instanceof CPSolver) {
//            return new JoinRef(
//                    solver.getVar((SetVariable) vars[0]),
//                    VariableUtils.getIntVar(solver, vars, 1, vars.length - 1),
//                    solver.getVar((SetVariable) vars[vars.length - 1]));
//        }
//        throw new ModelException("Could not found a constraint manager in " + this.getClass() + " !");
//    }
//
//    public static Constraint joinRef(SetVariable take, IntegerVariable[] refs, SetVariable to) {
//        if (refs.length == 0) {
//            throw new IllegalArgumentException();
//        }
//        Variable[] vars = new Variable[refs.length + 2];
//        vars[0] = take;
//        System.arraycopy(refs, 0, vars, 1, refs.length);
//        vars[refs.length + 1] = to;
//        return new ComponentConstraint(JoinRefManager.class, null, vars);
//    }
//    static int varCount = 0;
//
//    static SetVariable setVar(String name, int low, int high) {
//        return makeSetVar(name + (varCount++), low, high);
//    }
//
//    public static Constraint[] joinRefP(SetVariable take, IntegerVariable[] refs, SetVariable to) {
//        List<Constraint> constraints = new ArrayList<Constraint>();
//        List<SetVariable> r = new ArrayList<SetVariable>();
//
//        for (int x = take.getLowB(); x <= take.getUppB(); x++) {
//            if (x < refs.length) {
//                SetVariable ref = setVar("joinInterRef", to.getLowB(), to.getUppB());
//                r.add(ref);
//                constraints.add(implies(member(x, take), and(eqCard(ref, 1), member(refs[x], ref))));
//                constraints.add(implies(notMember(x, take), eq(ref, emptySet())));
//            } else {
//                constraints.add(notMember(x, take));
//            }
//        }
//
//        constraints.add(setUnion(r.toArray(new SetVariable[]{}), to));
//
//        return constraints.toArray(new Constraint[]{});
//    }
//
//    public static void main(String[] args) {
////        ChocoLogging.toSearch();
//
//        Model m = new CPModel();
//        SetVariable take = makeSetVar("take", 0, 4);
//        IntegerVariable[] i = makeIntVarArray("ref", 3, 0, 4);
//        SetVariable to = makeSetVar("to", 0, 4);
//
//        m.addConstraint(joinRef(take, i, to));
////        m.addConstraint(eqCard(take, 1));
//
//        //- Solution #1,000 found. 3611 Time (ms), 3250 Nodes, 5308 Backtracks, 0 Restarts.
//        //4089
////        m.addConstraint(eq(to, constant(new int[]{3})));
//
//
//        System.out.println(Util.allSolutions(m).getStatistics());
//
//        Solver solver = Util.newSolver(m);
//        solver.addGoal(new AssignVar(new MinDomain(solver), new IncreasingDomain()));
//        solver.addGoal(new AssignSetVar(new MinDomSet(solver), new MinEnv()));
//        System.out.println(Util.allSolutions(solver).getStatistics());
//
//        // #1000 solutions 201 Time (ms), 2045 Nodes, 4559 Backtracks, 0 Restarts - 
//        // #1000 solutions 846 Time (ms), 3250 Nodes, 5312 Backtracks, 0 Restarts - 
//        // #1000 solutions 201 Time (ms), 1431 Nodes, 1430 Backtracks, 0 Restarts - 
//        // #1000 solutions 613 Time (ms), 3250 Nodes, 5264 Backtracks, 0 Restarts - 
//        // #1000 solutions 216 Time (ms), 1431 Nodes, 1430 Backtracks, 0 Restarts - 
//        // #1000 solutions 422 Time (ms), 2475 Nodes, 3714 Backtracks, 0 Restarts - 
//        // #1000 solutions 408 Time (ms), 1431 Nodes, 1430 Backtracks, 0 Restarts - 
//        // #1000 solutions 362 Time (ms), 1818 Nodes, 2112 Backtracks, 0 Restarts -
//        // #1000 solutions 201 Time (ms), 1431 Nodes, 1430 Backtracks, 0 Restarts - 
//        // #1000 solutions 417 Time (ms), 1811 Nodes, 2097 Backtracks, 0 Restarts - 
//        // Add "sameRefs" to card propogation.
//        // #1000 solutions 207 Time (ms), 1431 Nodes, 1430 Backtracks, 0 Restarts - 
//        // #1000 solutions 337 Time (ms), 1806 Nodes, 2079 Backtracks, 0 Restarts - 
//        // max same ref with cards instead of ker and env
//        // #1000 solutions 201 Time (ms), 1431 Nodes, 1430 Backtracks, 0 Restarts - 
//        // #1000 solutions 328 Time (ms), 1806 Nodes, 2052 Backtracks, 0 Restarts - 
//    }
//}
