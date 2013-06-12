package org.clafer.compiler;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstraint;
import org.clafer.scope.Scope;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstUtil;
import org.clafer.ast.analysis.UnsatAnalyzer;
import org.clafer.ast.compiler.AstCompiler;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.choco.constraint.Constraints;
import org.clafer.choco.constraint.OrConstraint;
import org.clafer.choco.constraint.propagator.PropSetNotEqualC;
import org.clafer.collection.Pair;
import org.clafer.common.Util;
import org.clafer.compiler.ClaferObjective.Objective;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntConstant;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetConstant;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import org.clafer.scope.ScopeBuilder;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.constraints.nary.sum.Sum;
import solver.search.loop.monitors.IMonitorSolution;
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.SetStrategyFactory;
import solver.search.strategy.strategy.StrategiesSequencer;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 * Compiles from AST -> Choco
 *
 * @author jimmy
 */
public class ClaferCompiler {

    private ClaferCompiler() {
    }

    public static ClaferSolver compile(AstModel in, ScopeBuilder scope) {
        return compile(in, scope.toScope());
    }

    public static ClaferSolver compile(AstModel in, Scope scope) {
        Solver solver = new Solver();
        IrModule module = new IrModule();

        AstSolutionMap astSolution = AstCompiler.compile(in, scope, module);
        IrSolutionMap irSolution = IrCompiler.compile(module, solver);
        ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                SetStrategyFactory.setLex(solution.getIrSolution().getSetDecisionVars()),
                IntStrategyFactory.firstFail_InDomainMax(solution.getIrSolution().getIntDecisionVars()),
                IntStrategyFactory.firstFail_InDomainMax(solution.getIrSolution().getBoolDecisionVars())));
        return new ClaferSolver(solver, solution);
    }

    public static ClaferObjective compileMaximize(AstModel in, ScopeBuilder scope, AstRef ref) {
        return compileMaximize(in, scope.toScope(), ref);
    }

    public static ClaferObjective compileMaximize(AstModel in, Scope scope, AstRef ref) {
        Solver solver = new Solver();
        IrModule module = new IrModule();

        AstSolutionMap astSolution = AstCompiler.compile(in, scope, module);
        IrSolutionMap irSolution = IrCompiler.compile(module, solver);
        ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

        IntVar[] score = irSolution.getIntVars(astSolution.getRefVars(ref));
        int[] bounds = Sum.getSumBounds(score);
        IntVar sum = VF.bounded("Score", bounds[0], bounds[1], solver);
        solver.post(ICF.sum(score, sum));

        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                SetStrategyFactory.setLex(solution.getIrSolution().getSetDecisionVars()),
                IntStrategyFactory.firstFail_InDomainMax(score),
                IntStrategyFactory.firstFail_InDomainMax(solution.getIrSolution().getIntDecisionVars()),
                IntStrategyFactory.firstFail_InDomainMax(solution.getIrSolution().getBoolDecisionVars())));
        return new ClaferObjective(solver, solution, Objective.Maximize, sum);
    }

    public static ClaferObjective compileMinimize(AstModel in, ScopeBuilder scope, AstRef ref) {
        return compileMinimize(in, scope.toScope(), ref);
    }

    public static ClaferObjective compileMinimize(AstModel in, Scope scope, AstRef ref) {
        Solver solver = new Solver();
        IrModule module = new IrModule();

        AstSolutionMap astSolution = AstCompiler.compile(in, scope, module);
        IrSolutionMap irSolution = IrCompiler.compile(module, solver);
        ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

        IntVar[] score = irSolution.getIntVars(astSolution.getRefVars(ref));
        int[] bounds = Sum.getSumBounds(score);
        IntVar sum = VF.bounded("Score", bounds[0], bounds[1], solver);
        solver.post(ICF.sum(score, sum));

        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                SetStrategyFactory.setLex(solution.getIrSolution().getSetDecisionVars()),
                IntStrategyFactory.firstFail_InDomainMin(score),
                IntStrategyFactory.firstFail_InDomainMax(solution.getIrSolution().getIntDecisionVars()),
                IntStrategyFactory.firstFail_InDomainMax(solution.getIrSolution().getBoolDecisionVars())));
        return new ClaferObjective(solver, solution, Objective.Minimize, sum);
    }

    public static ClaferUnsat compileUnsat(AstModel in, ScopeBuilder scope) {
        return compileUnsat(in, scope.toScope());
    }

    public static ClaferUnsat compileUnsat(AstModel in, Scope scope) {
        Solver solver = new Solver();
        IrModule module = new IrModule();

        AstSolutionMap astSolution = AstCompiler.compile(in, scope, module,
                Util.cons(new UnsatAnalyzer(), AstCompiler.DefaultAnalyzers));
        IrSolutionMap irSolution = IrCompiler.compile(module, solver);
        ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

        Pair<AstConstraint, IrBoolVar>[] irSoftVarPairs = astSolution.getSoftVars();
        BoolVar[] softVars = new BoolVar[irSoftVarPairs.length];
        @SuppressWarnings("unchecked")
        Pair<AstConstraint, BoolVar>[] softVarPairs = new Pair[irSoftVarPairs.length];
        for (int i = 0; i < softVars.length; i++) {
            softVars[i] = irSolution.getBoolVar(irSoftVarPairs[i].getSnd());
            softVarPairs[i] = new Pair<AstConstraint, BoolVar>(irSoftVarPairs[i].getFst(), softVars[i]);
        }
        int[] bounds = Sum.getSumBounds(softVars);
        IntVar sum = VF.bounded("Score", bounds[0], bounds[1], solver);
        solver.post(ICF.sum(softVars, sum));

        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                IntStrategyFactory.firstFail_InDomainMax(softVars),
                IntStrategyFactory.firstFail_InDomainMax(solution.getIrSolution().getIntDecisionVars()),
                SetStrategyFactory.setLex(solution.getIrSolution().getSetDecisionVars()),
                IntStrategyFactory.firstFail_InDomainMax(solution.getIrSolution().getBoolDecisionVars())));
        return new ClaferUnsat(solver, solution, softVarPairs, sum);
    }

    public static ClaferSolver compilePartial(AstModel in, ScopeBuilder scope, AstConcreteClafer... concretize) {
        return compilePartial(in, scope.toScope(), concretize);
    }

    public static ClaferSolver compilePartial(AstModel in, Scope scope, AstConcreteClafer... concretize) {
        final Set<AstConcreteClafer> transitiveConcretize = new HashSet<AstConcreteClafer>();
        for (AstConcreteClafer clafer : concretize) {
            concretize(clafer, transitiveConcretize);
        }
        final ClaferSolver solver = compile(in, scope);
        final List<IntVar> intVars = new ArrayList<IntVar>();
        final List<SetVar> setVars = new ArrayList<SetVar>();
        for (AstConcreteClafer clafer : transitiveConcretize) {
            IrSetVar[] siblingVars = solver.getSolutionMap().getAstSolution().getSiblingVars(clafer);
            for (IrSetVar var : siblingVars) {
                if (!(var instanceof IrSetConstant)) {
                    setVars.add(solver.getSolutionMap().getIrSolution().getSetVar(var));
                }
            }
            AstRef ref = AstUtil.getInheritedRef(clafer);
            if (ref != null) {
                IrIntVar[] refVars = solver.getSolutionMap().getAstSolution().getRefVars(ref);
                for (IrIntVar var : refVars) {
                    if (!(var instanceof IrIntConstant)) {
                        intVars.add(solver.getSolutionMap().getIrSolution().getIntVar(var));
                    }
                }
            }
        }
        solver.getInternalSolver().getSearchLoop().plugSearchMonitor(new IMonitorSolution() {
            @Override
            public void onSolution() {
                List<Constraint> constraints = new ArrayList<Constraint>();
                for (IntVar var : intVars) {
                    constraints.add(ICF.arithm(var, "!=", var.getValue()));
                }
                for (SetVar var : setVars) {
                    constraints.add(Constraints.notEqual(var, var.getValue()));
                }
                solver.getInternalSolver().postCut(Constraints.or(constraints));
            }
        });
        return solver;
    }

    private static void concretize(AstClafer clafer, Set<AstConcreteClafer> concretize) {
        if (clafer instanceof AstAbstractClafer) {
            concretize((AstAbstractClafer) clafer, concretize);
        } else {
            concretize((AstConcreteClafer) clafer, concretize);
        }
    }

    private static void concretize(AstConcreteClafer clafer, Set<AstConcreteClafer> concretize) {
        if (!AstUtil.isRoot(clafer) && concretize.add(clafer)) {
            concretize(clafer.getParent(), concretize);
            concretize(clafer.getSuperClafer(), concretize);
        }
    }

    private static void concretize(AstAbstractClafer clafer, Set<AstConcreteClafer> concretize) {
        if (!AstUtil.isTypeRoot(clafer)) {
            for (AstClafer sub : clafer.getSubs()) {
                concretize(sub, concretize);
            }
        }
    }
}
