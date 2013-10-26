package org.clafer.compiler;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.scope.Scope;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstUtil;
import org.clafer.ast.analysis.UnsatAnalyzer;
import org.clafer.ast.compiler.AstCompiler;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.choco.constraint.Constraints;
import org.clafer.collection.Either;
import org.clafer.common.Util;
import org.clafer.objective.Objective;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.graph.Vertex;
import org.clafer.ir.IrIntConstant;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetConstant;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import org.clafer.scope.Scopable;
import org.clafer.scope.ScopeBuilder;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.search.loop.monitors.IMonitorSolution;
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.SetStrategyFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.strategy.StrategiesSequencer;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 * Compiles from AST -> Choco
 *
 * @author jimmy
 */
public class ClaferCompiler {

    private ClaferCompiler() {
    }

    private static SetVar[] getSetVars(AstModel model, ClaferSolutionMap map) {
        KeyGraph<AstClafer> dependency = new KeyGraph<AstClafer>();
        for (AstAbstractClafer abstractClafer : model.getAbstracts()) {
            Vertex<AstClafer> node = dependency.getVertex(abstractClafer);
            for (AstClafer sub : abstractClafer.getSubs()) {
                node.addNeighbour(dependency.getVertex(sub));
            }
        }
        for (AstConcreteClafer concreteClafer : AstUtil.getConcreteClafers(model)) {
            Vertex<AstClafer> node = dependency.getVertex(concreteClafer);
            if (concreteClafer.hasParent()) {
                node.addNeighbour(dependency.getVertex(concreteClafer.getParent()));
            }
        }
        List<SetVar> vars = new ArrayList<SetVar>();
        for (Set<AstClafer> component : GraphUtil.computeStronglyConnectedComponents(dependency)) {
            for (AstClafer clafer : component) {
                if (clafer instanceof AstConcreteClafer) {
                    for (IrSetVar setVar : map.getAstSolution().getSiblingVars(clafer)) {
                        if (!(setVar instanceof IrSetConstant)) {
                            Either<int[], SetVar> var = map.getIrSolution().getSetVar(setVar);
                            if (var.isRight()) {
                                vars.add(var.getRight());
                            }
                        }
                    }
                }
            }
        }
        return vars.toArray(new SetVar[vars.size()]);
    }

    private static IntVar[] getIntVars(AstModel model, ClaferSolutionMap map) {
        List<IntVar> vars = new ArrayList<IntVar>();
        for (AstClafer clafer : AstUtil.getClafers(model)) {
            if (clafer.hasRef()) {
                for (IrIntVar intVar : map.getAstSolution().getRefVars(clafer.getRef())) {
                    if (!(intVar instanceof IrIntConstant)) {
                        Either<Integer, IntVar> var = map.getIrSolution().getIntVar(intVar);
                        if (var.isRight()) {
                            vars.add(var.getRight());
                        }
                    }
                }
            }
        }
        return vars.toArray(new IntVar[vars.size()]);
    }

    private static AbstractStrategy<SetVar> setStrategy(SetVar[] vars, ClaferOptions options) {
        if (options.isPreferSmallerInstances()) {
            return SetStrategyFactory.remove_first(vars);
        }
        return SetStrategyFactory.force_first(vars);
    }

    public static ClaferSolver compile(AstModel in, Scopable scope) {
        return compile(in, scope, ClaferOptions.Default);
    }

    public static ClaferSolver compile(AstModel in, Scopable scope, ClaferOptions options) {
        Solver solver = new Solver();
        IrModule module = new IrModule();

        AstSolutionMap astSolution = AstCompiler.compile(in, scope.toScope(), module,
                options.isFullSymmetryBreaking());
        IrSolutionMap irSolution = IrCompiler.compile(module, solver, options.isFullOptimizations());
        ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                setStrategy(getSetVars(in, solution), options),
                IntStrategyFactory.firstFail_InDomainMin(getIntVars(in, solution))));
        return new ClaferSolver(solver, solution);
    }

    public static ClaferOptimizer compile(AstModel in, Scopable scope, Objective objective) {
        return compile(in, scope, objective, ClaferOptions.Default);
    }

    public static ClaferOptimizer compile(AstModel in, Scopable scope, Objective objective, ClaferOptions options) {
        Solver solver = new Solver();
        IrModule module = new IrModule();

        AstSolutionMap astSolution = AstCompiler.compile(
                in, scope.toScope(), objective, module,
                options.isFullSymmetryBreaking());
        IrSolutionMap irSolution = IrCompiler.compile(module, solver, options.isFullOptimizations());
        ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

        Either<Integer, IntVar> objectiveVar = irSolution.getIntVar(astSolution.getObjectiveVar(objective));
        IntVar[] objectiveVars = objectiveVar.isLeft() ? new IntVar[0] : new IntVar[]{objectiveVar.getRight()};

        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                setStrategy(getSetVars(in, solution), options),
                objective.isMaximize()
                ? IntStrategyFactory.firstFail_InDomainMax(objectiveVars)
                : IntStrategyFactory.firstFail_InDomainMin(objectiveVars),
                IntStrategyFactory.firstFail_InDomainMin(getIntVars(in, solution))));
        return new ClaferOptimizer(solver, solution, objective.isMaximize(), objectiveVar);
    }

    public static ClaferUnsat compileUnsat(AstModel in, Scopable scope) {
        return compileUnsat(in, scope.toScope(), ClaferOptions.Default);
    }

    public static ClaferUnsat compileUnsat(AstModel in, Scopable scope, ClaferOptions options) {
        Solver solver = new Solver();
        IrModule module = new IrModule();

        AstSolutionMap astSolution = AstCompiler.compile(in, scope.toScope(), module,
                Util.cons(new UnsatAnalyzer(), AstCompiler.DefaultAnalyzers),
                options.isFullSymmetryBreaking());
        IrSolutionMap irSolution = IrCompiler.compile(module, solver, options.isFullOptimizations());
        ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

        solver.set(new StrategiesSequencer(solver.getEnvironment(),
                IntStrategyFactory.firstFail_InDomainMax(Either.filterRights(irSolution.getBoolVars(astSolution.getSoftVars()))),
                setStrategy(getSetVars(in, solution), options),
                IntStrategyFactory.firstFail_InDomainMin(getIntVars(in, solution))));
        return new ClaferUnsat(solver, solution);
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
            for (IrSetVar siblingVar : siblingVars) {
                Either<int[], SetVar> var = solver.getSolutionMap().getIrSolution().getSetVar(siblingVar);
                if (var.isRight()) {
                    setVars.add(var.getRight());
                }
            }
            AstRef ref = AstUtil.getInheritedRef(clafer);
            if (ref != null) {
                IrIntVar[] refVars = solver.getSolutionMap().getAstSolution().getRefVars(ref);
                for (IrIntVar refVar : refVars) {
                    Either<Integer, IntVar> var = solver.getSolutionMap().getIrSolution().getIntVar(refVar);
                    if (var.isRight()) {
                        intVars.add(var.getRight());
                    }
                }
            }
        }
        solver.getInternalSolver().getSearchLoop().plugSearchMonitor(new IMonitorSolution() {
            private static final long serialVersionUID = 1L;

            @Override
            public void onSolution() {
                List<Constraint> constraints = new ArrayList<Constraint>();
                for (IntVar var : intVars) {
                    constraints.add(ICF.arithm(var, "!=", var.getValue()));
                }
                for (SetVar var : setVars) {
                    constraints.add(Constraints.notEqual(var, var.getValue()));
                }
                solver.getInternalSolver().postCut(Constraints.or(
                        constraints.toArray(new Constraint[constraints.size()])));
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
