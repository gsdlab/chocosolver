package org.clafer;

import choco.cp.model.CPModel;
import choco.cp.solver.CPSolver;
import choco.kernel.model.Model;
import choco.kernel.solver.Configuration;
import choco.kernel.solver.Solver;
import java.io.IOException;
import java.util.Iterator;
import java.util.NoSuchElementException;
import org.clafer.tree.ClaferModel;

/**
 *
 * @author jimmy
 */
public class ExprsSolutions implements Iterator<String> {

    private final ClaferModel claferModel;
    private final Solver solver;
    private boolean hasNext = false;
    private boolean end = false;

    public ExprsSolutions(ClaferModel claferModel) {
        this.claferModel = Check.notNull(claferModel);

        Model model = new CPModel();
        claferModel.build(model);

        solver = new CPSolver();
        solver.read(model);
        solver.getConfiguration().putInt(Configuration.LOGGING_MAX_DEPTH, 300000);


        hasNext = solver.solve();
        end = !hasNext;
    }

    @Override
    public boolean hasNext() {
        if (hasNext) {
            return true;
        }
        if (end) {
            return false;
        }
        hasNext = solver.nextSolution();
        end = !hasNext;
        return hasNext;
    }

    @Override
    public String next() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        hasNext = false;
        StringBuilder result = new StringBuilder();
        try {
            claferModel.print(solver, result);
        } catch (IOException e) {
            throw new RuntimeException("StringBuilder should not throw IO exceptions!", e);
        }
        return result.toString();
    }

    public String getRuntimeStatistics() {
        return solver.runtimeStatistics();
    }

    public String getSolutionToString() {
        return solver.solutionToString();
    }

    public int getSolutionCount() {
        return solver.getSolutionCount();
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException("Remove not supported");
    }
}
