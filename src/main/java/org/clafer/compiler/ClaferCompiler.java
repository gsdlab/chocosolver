package org.clafer.compiler;

import gnu.trove.impl.Constants;
import gnu.trove.map.TObjectIntMap;
import gnu.trove.map.hash.TObjectIntHashMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstStringClafer;
import org.clafer.ast.AstUtil;
import org.clafer.ast.analysis.UnsatAnalyzer;
import org.clafer.ast.compiler.AstCompiler;
import org.clafer.ast.compiler.AstSolutionMap;
import org.clafer.collection.Either;
import org.clafer.common.UnsatisfiableException;
import org.clafer.common.Util;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.graph.Vertex;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import org.clafer.objective.Objective;
import org.clafer.scope.Scopable;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.search.limits.NodeCounter;
import org.chocosolver.solver.search.loop.monitors.SMF;
import org.chocosolver.solver.search.strategy.ISF;
import org.chocosolver.solver.search.strategy.IntStrategyFactory;
import org.chocosolver.solver.search.strategy.strategy.AbstractStrategy;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.assertion.Assertion;
import org.clafer.ast.AstClafer;
import org.clafer.common.Check;
import org.clafer.ir.IrBoolVar;

/**
 * Compiles from AST -> Choco
 *
 * @author jimmy
 */
public class ClaferCompiler {

    private ClaferCompiler() {
    }

    private static TObjectIntMap<AstClafer> getBranchingPriorities(ClaferOption option) {
        int size = 0;
        for (Set<AstClafer> clafers : option.getBranchingPriority()) {
            size += clafers.size();
        }
        TObjectIntMap<AstClafer> priorities = new TObjectIntHashMap<>(size, Constants.DEFAULT_LOAD_FACTOR, option.getBranchingPriority().length);
        int priority = 0;
        for (Set<AstClafer> clafers : option.getBranchingPriority()) {
            for (AstClafer clafer : clafers) {
                priorities.put(clafer, priority);
            }
            priority++;
        }
        return priorities;
    }

    private static List<IntVar> getDecisionVars(AstClafer clafer, ClaferSolutionMap map) {
        List<IntVar> vars = new ArrayList<>();
        if (clafer instanceof AstConcreteClafer) {
            IrIntVar[] siblingBounds = map.getAstSolution().getSiblingBounds(clafer);
            if (siblingBounds == null) {
                for (IrBoolVar memberVar : map.getAstSolution().getMemberVars(clafer)) {
                    Either<Boolean, BoolVar> var = map.getIrSolution().getVar(memberVar);
                    if (var.isRight()) {
                        assert !var.getRight().isInstantiated();
                        vars.add(var.getRight());
                    }
                }
            } else {
                for (IrIntVar boundVar : siblingBounds) {
                    Either<Integer, IntVar> var = map.getIrSolution().getVar(boundVar);
                    if (var.isRight()) {
                        assert !var.getRight().isInstantiated();
                        vars.add(var.getRight());
                    }
                }
            }
        }
        if (clafer.hasRef()) {
            AstRef ref = clafer.getRef();
            if (ref.getTargetType() instanceof AstStringClafer) {
                for (IrStringVar stringVar : map.getAstSolution().getRefStrings(ref)) {
                    Either<Integer, IntVar> lengthVar = map.getIrSolution().getVar(stringVar.getLengthVar());
                    if (lengthVar.isRight()) {
                        assert !lengthVar.getRight().isInstantiated();
                        vars.add(lengthVar.getRight());
                    }
                    for (IrIntVar charVar : stringVar.getCharVars()) {
                        Either<Integer, IntVar> var = map.getIrSolution().getVar(charVar);
                        if (var.isRight()) {
                            assert !var.getRight().isInstantiated();
                            vars.add(var.getRight());
                        }
                    }
                }
            } else {
                for (IrIntVar intVar : map.getAstSolution().getRefVars(ref)) {
                    Either<Integer, IntVar> var = map.getIrSolution().getVar(intVar);
                    if (var.isRight()) {
                        assert !var.getRight().isInstantiated();
                        vars.add(var.getRight());
                    }
                }
            }
        }
        return vars;
    }

    private static List<IntVar> getDecisionVars(List<AstClafer> clafer, ClaferSolutionMap map) {
        return clafer.stream().flatMap(x -> getDecisionVars(x, map).stream()).collect(Collectors.toList());
    }

    private static List<List<IntVar>> getDecisionVars(AstModel model, ClaferOption option, ClaferSolutionMap map) {
        KeyGraph<AstClafer> dependency = new KeyGraph<>();
        for (AstAbstractClafer abstractClafer : model.getAbstracts()) {
            Vertex<AstClafer> node = dependency.getVertex(abstractClafer);
            for (AstClafer sub : abstractClafer.getSubs()) {
                node.addNeighbour(dependency.getVertex(sub));
            }
            if (abstractClafer.hasRef()) {
                node.addNeighbour(dependency.getVertex(abstractClafer.getRef().getTargetType()));
            }
        }
        for (AstConcreteClafer concreteClafer : AstUtil.getConcreteClafers(model)) {
            Vertex<AstClafer> node = dependency.getVertex(concreteClafer);
            if (concreteClafer.hasParent()) {
                node.addNeighbour(dependency.getVertex(concreteClafer.getParent()));
            }
            if (concreteClafer.hasRef()) {
                node.addNeighbour(dependency.getVertex(concreteClafer.getRef().getTargetType()));
            }
        }
        TObjectIntMap<AstClafer> priorities = getBranchingPriorities(option);
        return GraphUtil.computeStronglyConnectedComponents(dependency).stream()
                .flatMap(Set::stream)
                .collect(Collectors.groupingBy(priorities::get))
                .entrySet().stream()
                .sorted(Comparator.comparing(Entry::getKey))
                .map(Entry::getValue).map(x -> getDecisionVars(x, map)).collect(Collectors.toList());
    }

    private static void set(Solver solver, List<AbstractStrategy<?>> strategies) {
        AbstractStrategy<?>[] strats = strategies.toArray(new AbstractStrategy[strategies.size()]);
        if (strats.length > 0) {
            solver.set(strats);
        } else {
            // Give the solver a dummy strategy for trivial problems so the underlying Choco
            // framework does not warn of no search strategy.
            solver.set(ISF.lexico_LB(solver.ZERO()));
        }
    }

    private static Optional<AbstractStrategy<?>> intStrategy(IntVar[] vars, ClaferOption options) {
        if (vars.length == 0) {
            return Optional.empty();
        }
        switch (options.getStrategy()) {
            case PreferSmallerInstances:
                return firstFailInDomainMin(vars);
            case PreferLargerInstances:
                return firstFailInDomainMax(vars);
            case Random:
                return Optional.of(IntStrategyFactory.random_value(vars, System.nanoTime()));
            default:
                throw new IllegalArgumentException();
        }
    }

    private static List<AbstractStrategy<?>> intStrategies(List<List<IntVar>> vars, ClaferOption options) {
        List<AbstractStrategy<?>> strategies = new ArrayList<>();
        for (List<IntVar> ivars : vars) {
            intStrategy(ivars.toArray(new IntVar[ivars.size()]), options).ifPresent(strategies::add);
        }
        return strategies;
    }

    private static void restartPolicy(Solver solver, ClaferOption options) {
        switch (options.getStrategy()) {
            case Random:
                SMF.luby(solver, 16, 16, new NodeCounter(solver, 16), Integer.MAX_VALUE);
        }
    }

    private static Optional<AbstractStrategy<?>> firstFailInDomainMax(IntVar[] vars) {
        if (vars.length == 0) {
            return Optional.empty();
        }
        return Optional.of(IntStrategyFactory.minDom_UB(vars));
    }

    private static Optional<AbstractStrategy<?>> firstFailInDomainMin(IntVar[] vars) {
        if (vars.length == 0) {
            return Optional.empty();
        }
        return Optional.of(IntStrategyFactory.minDom_LB(vars));
    }

    public static ClaferSolver compile(AstModel in, Scopable scope) {
        return compile(in, scope, ClaferOption.Default);
    }

    public static ClaferSolver compile(AstModel in, Scopable scope, ClaferOption options) {
        try {
            Solver solver = new Solver();
            IrModule module = new IrModule();

            AstSolutionMap astSolution = AstCompiler.compile(in, scope.toScope(), module,
                    options.isFullSymmetryBreaking());
            IrSolutionMap irSolution = IrCompiler.compile(module, solver, options.isFullOptimizations());
            ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

            set(solver, intStrategies(getDecisionVars(in, options, solution), options));
            restartPolicy(solver, options);
            return new ClaferSolver(solver, solution, options.getStrategy() == ClaferSearchStrategy.Random);
        } catch (UnsatisfiableException e) {
            return new ClaferSolver();
        }
    }

    public static ClaferOptimizer compile(AstModel in, Scopable scope, Objective... objectives) {
        return compile(in, scope, objectives, ClaferOption.Default);
    }

    public static ClaferOptimizer compile(AstModel in, Scopable scope, Objective[] objectives, ClaferOption options) {
        Check.noNullsNotEmpty(objectives);
        try {
            Solver solver = new Solver();
            IrModule module = new IrModule();

            AstSolutionMap astSolution = AstCompiler.compile(
                    in, scope.toScope(), objectives, module,
                    options.isFullSymmetryBreaking());
            IrSolutionMap irSolution = IrCompiler.compile(module, solver, options.isFullOptimizations());
            ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

            IrIntVar[] objectiveIrVars = new IrIntVar[objectives.length];
            for (int i = 0; i < objectiveIrVars.length; i++) {
                objectiveIrVars[i] = astSolution.getObjectiveVar(objectives[i]);
            }
            Either<Integer, IntVar>[] objectiveVars = irSolution.getVars(objectiveIrVars);

            boolean[] maximizes = new boolean[objectives.length];
            IntVar[] scores = new IntVar[objectives.length];
            int variableScores = 0;
            Integer[] fixedScores = new Integer[objectives.length];
            for (int i = 0; i < objectives.length; i++) {
                if (objectiveVars[i].isLeft()) {
                    fixedScores[i] = objectiveVars[i].getLeft();
                } else {
                    maximizes[variableScores] = objectives[i].isMaximize();
                    scores[variableScores] = objectiveVars[i].getRight();
                    variableScores++;
                }
            }
            maximizes = Arrays.copyOf(maximizes, variableScores);
            scores = Arrays.copyOf(scores, variableScores);

            set(solver, intStrategies(getDecisionVars(in, options, solution), options));
            restartPolicy(solver, options);
            ClaferOptimizer optimizer = maximizes.length == 0
                    ? new ClaferNoObjectiveOptimizer(new ClaferSolver(solver, solution))
                    : maximizes.length == 1
                            ? new EquivalentParetoSolver(new ClaferSingleObjectiveOptimizer(solver, solution, maximizes[0], scores[0]))
                            : new EquivalentParetoSolver(new ClaferMultiObjectiveOptimizerGIA(solver, solution, maximizes, scores));

            return variableScores < fixedScores.length
                    ? new PartiallyKnownOptimizer(optimizer, fixedScores)
                    : optimizer;
        } catch (UnsatisfiableException e) {
            return new ClaferNoObjectiveOptimizer(new ClaferSolver());
        }
    }

    public static ClaferAsserter compile(AstModel in, Scopable scope, Assertion... assertions) {
        return compile(in, scope, assertions, ClaferOption.Default);
    }

    public static ClaferAsserter compile(AstModel in, Scopable scope, Assertion[] assertions, ClaferOption options) {
        Check.noNullsNotEmpty(assertions);
        try {
            Solver solver = new Solver();
            IrModule module = new IrModule();

            AstSolutionMap astSolution = AstCompiler.compile(
                    in, scope.toScope(), assertions, module,
                    options.isFullSymmetryBreaking());
            IrSolutionMap irSolution = IrCompiler.compile(module, solver, options.isFullOptimizations());
            ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

            Map<Assertion, Either<Boolean, BoolVar>> assertionMap = new HashMap<>(assertions.length);
            for (Assertion assertion : assertions) {
                assertionMap.put(assertion, irSolution.getVar(astSolution.getAssertionVar(assertion)));
            }

            set(solver, intStrategies(getDecisionVars(in, options, solution), options));
            restartPolicy(solver, options);
            return new ClaferAsserter(solver, solution, assertionMap);
        } catch (UnsatisfiableException e) {
            return new ClaferAsserter();
        }
    }

    public static ClaferUnsat compileUnsat(AstModel in, Scopable scope) {
        return compileUnsat(in, scope.toScope(), ClaferOption.Default);
    }

    public static ClaferUnsat compileUnsat(AstModel in, Scopable scope, ClaferOption options) {
        Solver solver = new Solver();
        IrModule module = new IrModule();

        AstSolutionMap astSolution = AstCompiler.compile(in, scope.toScope(), module,
                Util.cons(new UnsatAnalyzer(), AstCompiler.DefaultAnalyzers),
                options.isFullSymmetryBreaking());
        IrSolutionMap irSolution = IrCompiler.compile(module, solver, options.isFullOptimizations());
        ClaferSolutionMap solution = new ClaferSolutionMap(astSolution, irSolution);

        set(solver,
                Util.maybeCons(firstFailInDomainMax(Either.filterRight(irSolution.getVars(astSolution.getSoftVars()))),
                        intStrategies(getDecisionVars(in, options, solution), options)));
        restartPolicy(solver, options);
        return new ClaferUnsat(solver, solution);
    }

//    public static ClaferSolver compilePartial(AstModel in, ScopeBuilder scope, AstConcreteClafer... concretize) {
//        return compilePartial(in, scope.toScope(), concretize);
//    }
//
//    public static ClaferSolver compilePartial(AstModel in, Scope scope, AstConcreteClafer... concretize) {
//        final Set<AstConcreteClafer> transitiveConcretize = new HashSet<>();
//        for (AstConcreteClafer clafer : concretize) {
//            concretize(clafer, transitiveConcretize);
//        }
//        final ClaferSolver solver = compile(in, scope);
//        final List<IntVar> intVars = new ArrayList<>();
//        final List<SetVar> setVars = new ArrayList<>();
//        for (AstConcreteClafer clafer : transitiveConcretize) {
//            IrSetVar[] siblingVars = solver.getSolutionMap().getAstSolution().getSiblingVars(clafer);
//            for (IrSetVar siblingVar : siblingVars) {
//                Either<int[], SetVar> var = solver.getSolutionMap().getIrSolution().getSetVar(siblingVar);
//                if (var.isRight()) {
//                    setVars.add(var.getRight());
//                }
//            }
//            AstRef ref = AstUtil.getInheritedRef(clafer);
//            if (ref != null) {
//                IrIntVar[] refVars = solver.getSolutionMap().getAstSolution().getRefVars(ref);
//                for (IrIntVar refVar : refVars) {
//                    Either<Integer, IntVar> var = solver.getSolutionMap().getIrSolution().getIntVar(refVar);
//                    if (var.isRight()) {
//                        intVars.add(var.getRight());
//                    }
//                }
//            }
//        }
//        solver.getInternalSolver().getSearchLoop().plugSearchMonitor(new IMonitorSolution() {
//            private static final long serialVersionUID = 1L;
//
//            @Override
//            public void onSolution() {
//                List<Constraint> constraints = new ArrayList<>();
//                for (IntVar var : intVars) {
//                    constraints.add(ICF.arithm(var, "!=", var.getValue()));
//                }
//                for (SetVar var : setVars) {
//                    constraints.add(Constraints.notEqual(var, var.getValue()));
//                }
//                solver.getInternalSolver().postCut(Constraints.or(
//                        constraints.toArray(new Constraint[constraints.size()])));
//            }
//        });
//        return solver;
//    }
//
//    private static void concretize(AstClafer clafer, Set<AstConcreteClafer> concretize) {
//        if (clafer instanceof AstAbstractClafer) {
//            concretize((AstAbstractClafer) clafer, concretize);
//        } else {
//            concretize((AstConcreteClafer) clafer, concretize);
//        }
//    }
//
//    private static void concretize(AstConcreteClafer clafer, Set<AstConcreteClafer> concretize) {
//        if (!AstUtil.isRoot(clafer) && concretize.add(clafer)) {
//            concretize(clafer.getParent(), concretize);
//            concretize(clafer.getSuperClafer(), concretize);
//        }
//    }
//
//    private static void concretize(AstAbstractClafer clafer, Set<AstConcreteClafer> concretize) {
//        if (!AstUtil.isTypeRoot(clafer)) {
//            for (AstClafer sub : clafer.getSubs()) {
//                concretize(sub, concretize);
//            }
//        }
//    }
}
