package org.clafer;

import choco.Choco;
import choco.Options;
import choco.cp.model.CPModel;
import choco.cp.solver.CPSolver;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerExpressionVariable;
import choco.kernel.model.variables.integer.IntegerVariable;
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

    public ExprsSolutions(ClaferModel claferModel, IntegerExpressionVariable ovar) {
        this.claferModel = Check.notNull(claferModel);

        Model model = new CPModel();
        claferModel.build(model);

        if (ovar != null) {
            if (ovar instanceof IntegerVariable) {
                model.addVariable(Options.V_OBJECTIVE, (IntegerVariable) ovar);
            } else {
                IntegerVariable ivar = Choco.makeIntVar("__objective", ovar.getLowB(), ovar.getUppB(),
                        Options.V_BOUND, Options.V_NO_DECISION, Options.V_OBJECTIVE);
                model.addConstraint(Choco.eq(ivar, ovar));
            }
        }

        solver = new CPSolver();
        solver.read(model);
        solver.getConfiguration().putInt(Configuration.LOGGING_MAX_DEPTH, 300000);

        hasNext = ovar == null ? solver.solve() : solver.minimize(false);
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
        return "Solution #" + getSolutionCount() + " - " + solver.runtimeStatistics();
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
