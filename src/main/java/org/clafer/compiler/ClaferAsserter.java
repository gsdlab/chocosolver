package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.BoolVar;
import org.clafer.assertion.Assertion;
import org.clafer.choco.constraint.Constraints;
import org.clafer.collection.Either;
import org.clafer.common.Check;
import org.clafer.instance.InstanceModel;

/**
 *
 * @author jimmy
 */
public class ClaferAsserter implements ClaferSearch {

    private final Solver solver;
    private final ClaferSolutionMap solutionMap;
    private final Map<Assertion, Either<Boolean, BoolVar>> assertions;
    private int count = 0;
    private boolean more = true;

    public ClaferAsserter(Solver solver, ClaferSolutionMap solutionMap, Map<Assertion, Either<Boolean, BoolVar>> assertions) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
        this.assertions = Check.notNull(assertions);
    }

    public ClaferAsserter() {
        this.solver = new Solver();
        this.solutionMap = null;
        this.assertions = null;
        this.more = false;
    }

    @Override
    public boolean find() {
        if (!more) {
            return false;
        }
        more &= count == 0 ? solveFirst() : solveNext();
        if (solver.hasReachedLimit()) {
            more = false;
            throw new ReachedLimitException();
        }
        if (more) {
            count++;
        }
        return more;
    }

    private boolean solveFirst() {
        List<BoolVar> boolVars = new ArrayList<>(assertions.size());
        for (Either<?, BoolVar> assertion : assertions.values()) {
            if (assertion.isLeft() && assertion.getLeft().equals(Boolean.TRUE)) {
                return solver.findSolution();
            } else if (assertion.isRight()) {
                boolVars.add(assertion.getRight());
            }
        }
        if (boolVars.isEmpty()) {
            return false;
        }
        solver.post(Constraints.or(boolVars.toArray(new BoolVar[boolVars.size()])));
        return solver.findSolution();
    }

    private boolean solveNext() {
        List<BoolVar> boolVars = new ArrayList<>(assertions.size());
        for (Either<?, BoolVar> assertion : assertions.values()) {
            if (assertion.isRight()) {
                BoolVar boolVar = assertion.getRight();
                if (boolVar.isInstantiatedTo(0)) {
                    boolVars.add(boolVar);
                }
            }
        }
        if (boolVars.isEmpty()) {
            return false;
        }
        solver.post(Constraints.or(boolVars.toArray(new BoolVar[boolVars.size()])));
        return solver.nextSolution();
    }

    public Assertion[] failedAssertions() {
        if (count == 0 || !more) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        List<Assertion> failedAssertions = new ArrayList<>(assertions.size());
        for (Entry<Assertion, Either<Boolean, BoolVar>> assertion : assertions.entrySet()) {
            Either<Boolean, BoolVar> value = assertion.getValue();
            if (value.isLeft() && value.getLeft().equals(Boolean.TRUE)) {
                failedAssertions.add(assertion.getKey());
            } else if (value.isRight() && value.getRight().getValue() == 1) {
                failedAssertions.add(assertion.getKey());
            }
        }
        return failedAssertions.toArray(new Assertion[failedAssertions.size()]);
    }

    @Override
    public InstanceModel instance() {
        if (count == 0 || !more) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return solutionMap.getInstance();
    }

    @Override
    public int instanceCount() {
        return count;
    }

    @Override
    public Solver getInternalSolver() {
        return solver;
    }
}
