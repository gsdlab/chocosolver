package org.clafer.compiler;

import org.clafer.Scope;
import org.clafer.analysis.AnalysisUtil;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.Asts;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.compiler.AstCompiler;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.compiler.ClaferObjective.Objective;
import org.clafer.ir.IrModule;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import solver.Solver;
import solver.constraints.IntConstraintFactory;
import solver.constraints.nary.Sum;
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.SetStrategyFactory;
import solver.search.strategy.strategy.StrategiesSequencer;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VariableFactory;

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
        IntVar sum = VariableFactory.bounded("Score", bounds[0], bounds[1], solver);
        solver.post(IntConstraintFactory.sum(score, sum));

        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                IntStrategyFactory.firstFail_InDomainMin(solution.getIrSolution().getBoolVars())
//                SetStrategyFactory.setLex(solution.getIrSolution().getSetVars()),
//                IntStrategyFactory.firstFail_InDomainMin(score),
//                IntStrategyFactory.firstFail_InDomainMin(solution.getIrSolution().getIntVars())
                ));
        return new ClaferObjective(solver, solution, Objective.Minimize, sum);
    }

    public static ClaferUnsat compileUnsat(AstModel in, Scope scope) {
        for (AstClafer clafer : AnalysisUtil.getClafers(in)) {
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
        IntVar sum = VariableFactory.bounded("Score", bounds[0], bounds[1], solver);
        solver.post(IntConstraintFactory.sum(softVars, sum));

        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                IntStrategyFactory.firstFail_InDomainMax(softVars),
                IntStrategyFactory.firstFail_InDomainMin(solution.getIrSolution().getIntVars()),
                SetStrategyFactory.setLex(solution.getIrSolution().getSetVars()),
                IntStrategyFactory.firstFail_InDomainMin(solution.getIrSolution().getBoolVars())));
        return new ClaferUnsat(solver, solution, softVars, sum);
    }

    public static void main(String[] args) {
        AstModel model = Asts.newModel();
        model.addTopClafer("Jimmy").withCard(2, 2).addChild("Degree").withCard(1, 2).refTo(Asts.IntType);

        ClaferSolver solver = compile(model, Scope.builder().defaultScope(5).intLow(-1).intHigh(1).toScope());
        System.out.println(solver);
        while (solver.find()) {
            System.out.println(solver.instance());
        }
        System.out.println(solver.getMeasures().getSolutionCount());
    }
}
