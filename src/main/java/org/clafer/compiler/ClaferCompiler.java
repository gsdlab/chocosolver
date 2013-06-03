package org.clafer.compiler;

import org.clafer.ast.AstConstraint;
import org.clafer.scope.Scope;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.analysis.UnsatAnalyzer;
import org.clafer.ast.compiler.AstCompiler;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.collection.Pair;
import org.clafer.common.Util;
import org.clafer.compiler.ClaferObjective.Objective;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import org.clafer.scope.ScopeBuilder;
import solver.Solver;
import solver.constraints.ICF;
import solver.constraints.nary.Sum;
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.SetStrategyFactory;
import solver.search.strategy.strategy.StrategiesSequencer;
import solver.variables.BoolVar;
import solver.variables.IntVar;
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
        for(int i = 0; i < softVars.length;i++) {
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
}
