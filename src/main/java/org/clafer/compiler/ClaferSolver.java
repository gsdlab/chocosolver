package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.clafer.common.Check;
import org.clafer.instance.InstanceModel;
import solver.Solver;
import solver.search.measure.IMeasures;

/**
 *
 * @author jimmy
 */
public class ClaferSolver {

    private final Solver solver;
    private final ClaferSolutionMap solutionMap;
    private boolean first = true;
    private boolean more = true;

    ClaferSolver(Solver solver, ClaferSolutionMap solutionMap) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
    }

    public IMeasures getMeasures() {
        return solver.getMeasures();
    }

    public Solver getInternalSolver() {
        return solver;
    }

    public boolean find() {
        if (!more) {
            return false;
        }
        if (first) {
            first = false;
            more &= solver.findSolution();
            return more;
        }
        more &= solver.nextSolution();
        return more;
    }

    public InstanceModel instance() {
        return solutionMap.getInstance();
    }

    public InstanceModel[] allInstances() {
        List<InstanceModel> instances = new ArrayList<InstanceModel>();
        while (find()) {
            instances.add(instance());
        }
        return instances.toArray(new InstanceModel[instances.size()]);
    }

    @Override
    public String toString() {
        return solver.toString();
    }
}
