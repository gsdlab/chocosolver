package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.clafer.common.Check;
import org.clafer.instance.InstanceModel;
import solver.Solver;

/**
 *
 * @author jimmy
 */
public class ClaferSolver implements ClaferSearch {

    private final Solver solver;
    private final ClaferSolutionMap solutionMap;
    private int count = 0;
    private boolean more = true;

    ClaferSolver() {
        this.solver = new Solver();
        this.solver.post(solver.FALSE);
        this.solutionMap = null;
    }

    ClaferSolver(Solver solver, ClaferSolutionMap solutionMap) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
    }

    public ClaferSolutionMap getSolutionMap() {
        return solutionMap;
    }

    @Override
    public boolean find() {
        if (!more) {
            return false;
        }
        more &= count == 0 ? solver.findSolution() : solver.nextSolution();
        if (more) {
            count++;
        }
        return more;
    }

    @Override
    public InstanceModel instance() {
        if (count == 0 || !more) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return solutionMap.getInstance();
    }

    @Override
    public InstanceModel[] allInstances() {
        List<InstanceModel> instances = new ArrayList<>();
        while (find()) {
            instances.add(instance());
        }
        return instances.toArray(new InstanceModel[instances.size()]);
    }

    @Override
    public int instanceCount() {
        return count;
    }

    @Override
    public Solver getInternalSolver() {
        return solver;
    }

    @Override
    public String toString() {
        return solver.toString();
    }
}
