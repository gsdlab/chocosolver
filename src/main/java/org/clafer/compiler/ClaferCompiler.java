package org.clafer.compiler;

import org.clafer.Scope;
import org.clafer.ast.Ast;
import org.clafer.ast.AstModel;
import org.clafer.ast.compiler.AstCompiler;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.ir.IrModule;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import solver.Solver;
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.SetStrategyFactory;
import solver.search.strategy.strategy.StrategiesSequencer;

/**
 * Compiles from AST -> Choco
 * 
 * @author jimmy
 */
public class ClaferCompiler {

    /**
     * Higher-level, less control.
     */
    public static ClaferSolver compile(AstModel in, Scope scope) {
        Solver solver = new Solver();
        ClaferSolutionMap solution = compile(in, scope, solver);
        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                IntStrategyFactory.firstFail_InDomainMin(solution.getIrSolution().getIntVars()),
                SetStrategyFactory.setLex(solution.getIrSolution().getSetVars())));
        return new ClaferSolver(solver, solution);
    }

    /**
     * Lower-level, more control.
     */
    public static ClaferSolutionMap compile(AstModel in, Scope scope, Solver out) {
        IrModule module = new IrModule();
        AstSolutionMap astSolution = AstCompiler.compile(in, scope, module);

        IrSolutionMap irSolution = IrCompiler.compile(module, out);

        return new ClaferSolutionMap(astSolution, irSolution);
    }

    public static void main(String[] args) {
        AstModel model = Ast.newModel();
        model.addTopClafer("Jimmy").withCard(2, 2).addChild("Degree").withCard(1, 2).refTo(Ast.IntType);

        ClaferSolver solver = compile(model, Scope.builder().defaultScope(5).intLow(-1).intHigh(1).toScope());
        System.out.println(solver);
        while (solver.find()) {
            System.out.println(solver.instance());
        }
        System.out.println(solver.getMeasures().getSolutionCount());
    }
}
