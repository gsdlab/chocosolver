package org.clafer.compiler;

import gnu.trove.iterator.TIntObjectIterator;
import gnu.trove.map.TIntObjectMap;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstUtil;
import org.clafer.collection.Either;
import org.clafer.common.Check;
import org.clafer.collection.Pair;
import org.clafer.instance.InstanceModel;
import org.clafer.ir.IrBoolVar;
import solver.ResolutionPolicy;
import solver.Solver;
import solver.constraints.ICF;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import util.ESat;

/**
 * Either call {@link #minUnsat()} xor {@link #unsatCore()} at most once. If you
 * need to invoke both, you need to two ClaferUnsat objects.
 *
 * @author jimmy
 */
public class ClaferUnsat {

    private final Solver solver;
    private final ClaferSolutionMap solutionMap;
    private final Pair<AstConstraint, Either<Boolean, BoolVar>>[] softVars;
    private final Either<Integer, IntVar> score;

    ClaferUnsat(Solver solver, ClaferSolutionMap solutionMap) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
        TIntObjectMap<AstConstraint> constraintMap =
                AstUtil.getConstraintMap(solutionMap.getAstSolution().getModel());
        TIntObjectMap<IrBoolVar> softVarsMap = solutionMap.getAstSolution().getSoftVarsMap();
        @SuppressWarnings("unchecked")
        Pair<AstConstraint, Either<Boolean, BoolVar>>[] soft = new Pair[softVarsMap.size()];
        TIntObjectIterator<IrBoolVar> iter = softVarsMap.iterator();
        for (int i = 0; i < soft.length; i++) {
            iter.advance();
            soft[i] = new Pair<AstConstraint, Either<Boolean, BoolVar>>(
                    constraintMap.get(iter.key()),
                    solutionMap.getIrSolution().getBoolVar(iter.value()));
        }
        this.softVars = soft;
        this.score = solutionMap.getIrSolution().getIntVar(solutionMap.getAstSolution().getSumSoftVar());
    }

    public Solver getInternalSolver() {
        return solver;
    }

    /**
     * Compute the minimal set of constraints that need to be removed before the
     * model is satisfiable. If the model is already satisfiable, then the set
     * is empty. Guaranteed to be minimum.
     *
     * @return the Min-Unsat and the corresponding near-miss example or null if
     * unknown
     */
    public Pair<Set<AstConstraint>, InstanceModel> minUnsat() {
        if (ESat.TRUE.equals(maximize())) {
            Set<AstConstraint> unsat = new HashSet<AstConstraint>();
            for (Pair<AstConstraint, Either<Boolean, BoolVar>> softVar : softVars) {
                Either<Boolean, BoolVar> var = softVar.getSnd();
                if (var.isLeft()
                        ? !var.getLeft().booleanValue()
                        : var.getRight().instantiatedTo(0)) {
                    unsat.add(softVar.getFst());
                }
            }
            return new Pair<Set<AstConstraint>, InstanceModel>(unsat, solutionMap.getInstance());
        }
        return null;
    }

    /**
     * Compute a small set of constraints that are mutually unsatisfiable.
     * Undefined behaviour if the model is satisfiable. This method is always
     * slower to compute than {@link #minUnsat()}. Not guaranteed to be minimum.
     *
     * @return the Unsat-Core or null if unknown
     */
    public Set<AstConstraint> unsatCore() {
        Set<AstConstraint> unsat = new HashSet<AstConstraint>();
        switch (maximize()) {
            case TRUE:
                boolean changed;
                do {
                    changed = false;
                    List<BoolVar> minUnsat = new ArrayList<BoolVar>();
                    for (Pair<AstConstraint, Either<Boolean, BoolVar>> softVar : softVars) {
                        Either<Boolean, BoolVar> var = softVar.getSnd();
                        if (var.isLeft()
                                ? !var.getLeft().booleanValue()
                                : var.getRight().instantiatedTo(0)) {
                            changed |= unsat.add(softVar.getFst());
                            if (var.isRight()) {
                                minUnsat.add(var.getRight());
                            }
                        }
                    }
                    solver.getSearchLoop().reset();
                    for (BoolVar var : minUnsat) {
                        solver.postCut(ICF.arithm(var, "=", 1));
                    }
                } while (changed && ESat.TRUE.equals(maximize()));
                return unsat;
            default:
                return null;
        }
    }

    private ESat maximize() {
        if (score.isLeft()) {
            solver.findSolution();
        } else {
            solver.findOptimalSolution(ResolutionPolicy.MAXIMIZE, score.getRight());
        }
        return solver.isFeasible();
    }
}
