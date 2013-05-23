package org.clafer.compiler;

import org.clafer.ast.scope.Scope;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstUtil;
import org.clafer.ast.compiler.AstCompiler;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.compiler.ClaferObjective.Objective;
import org.clafer.ir.IrModule;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
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

    public static ClaferSolver compile(AstModel in, Scope scope) {
        Solver solver = new Solver();
        IrModule module = new IrModule();

        AstSolutionMap astSolution = AstCompiler.compile(in, scope, module);
        IrSolutionMap irSolution = IrCompiler.compile(module, solver);
        ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                SetStrategyFactory.setLex(solution.getIrSolution().getSetVars()),
                IntStrategyFactory.firstFail_InDomainMin(solution.getIrSolution().getIntVars()),
                IntStrategyFactory.firstFail_InDomainMin(solution.getIrSolution().getBoolVars())));
        return new ClaferSolver(solver, solution);
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
                SetStrategyFactory.setLex(solution.getIrSolution().getSetVars()),
                IntStrategyFactory.firstFail_InDomainMin(score),
                IntStrategyFactory.firstFail_InDomainMin(solution.getIrSolution().getIntVars()),
                IntStrategyFactory.firstFail_InDomainMax(solution.getIrSolution().getBoolVars())));
        return new ClaferObjective(solver, solution, Objective.Minimize, sum);
    }

    public static ClaferUnsat compileUnsat(AstModel in, Scope scope) {
        for (AstClafer clafer : AstUtil.getClafers(in)) {
            for (AstConstraint constraint : clafer.getConstraints()) {
                constraint.asSoft();
            }
        }

        Solver solver = new Solver();
        IrModule module = new IrModule();

        AstSolutionMap astSolution = AstCompiler.compile(in, scope, module);
        IrSolutionMap irSolution = IrCompiler.compile(module, solver);
        ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

        BoolVar[] softVars = irSolution.getBoolVars(astSolution.getSoftVars());
        int[] bounds = Sum.getSumBounds(softVars);
        IntVar sum = VF.bounded("Score", bounds[0], bounds[1], solver);
        solver.post(ICF.sum(softVars, sum));

        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                IntStrategyFactory.firstFail_InDomainMax(softVars),
                IntStrategyFactory.firstFail_InDomainMin(solution.getIrSolution().getIntVars()),
                SetStrategyFactory.setLex(solution.getIrSolution().getSetVars()),
                IntStrategyFactory.firstFail_InDomainMin(solution.getIrSolution().getBoolVars())));
        return new ClaferUnsat(solver, solution, softVars, sum);
    }
}
