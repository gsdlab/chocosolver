package org.clafer.compiler;

import org.clafer.Scope;
import org.clafer.ast.Ast;
import org.clafer.ast.AstModel;
import org.clafer.ast.compiler.AstCompiler;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.collection.Pair;
import org.clafer.ir.IrModule;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import solver.Solver;
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.SetStrategyFactory;
import solver.search.strategy.strategy.StrategiesSequencer;

/**
 *
 * @author jimmy
 */
public class Compiler {

    /**
     * Higher-level, less control.
     */
    public static ChocoSolver compile(AstModel in, Scope scope) {
        Solver solver = new Solver();
        SolutionMap solution = compile(in, scope, solver);
        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                IntStrategyFactory.firstFail_InDomainMin(solution.getIrSolution().getIntVars()),
                SetStrategyFactory.setLex(solution.getIrSolution().getSetVars())));
        return new ChocoSolver(solver, solution);
    }

    /**
     * Lower-level, more control.
     */
    public static SolutionMap compile(AstModel in, Scope scope, Solver out) {
        IrModule module = new IrModule();
        AstSolutionMap astSolution = AstCompiler.compile(in, scope, module);
        
        IrSolutionMap irSolution = IrCompiler.compile(module, out);

        return new SolutionMap(astSolution, irSolution);
    }

    public static void main(String[] args) {
        AstModel model = Ast.newModel();
        model.addTopClafer("Jimmy").withCard(2, 2).addChild("Degree").withCard(1, 2);

        ChocoSolver solver = compile(model, new Scope(100));

        while (solver.nextSolution()) {
            System.out.println(solver.solution());
        }
    }
}
