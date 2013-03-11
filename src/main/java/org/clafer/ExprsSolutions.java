package org.clafer;

import choco.cp.solver.CPSolver;
import choco.kernel.solver.Configuration;
import choco.kernel.solver.Solver;
import java.io.IOException;
import java.util.Iterator;
import java.util.NoSuchElementException;
import org.clafer.tree.RootClafer;

/**
 *
 * @author jimmy
 */
public class ExprsSolutions implements Iterator<String> {

    private final Solver solver;
    private final RootClafer root;
    private boolean hasNext = false;
    private boolean first = true;
    private boolean end = false;

    public ExprsSolutions(Exprs exprs) {
        Check.notNull(exprs);

        solver = new CPSolver();
        solver.read(exprs.getModel());
        solver.getConfiguration().putInt(Configuration.LOGGING_MAX_DEPTH, 300000);

        root = exprs.getRoot();
    }

    @Override
    public boolean hasNext() {
        if(hasNext) {
            return true;
        }
        if(end) {
            return false;
        }
        hasNext = first ? solver.solve() : solver.nextSolution();
        end = !hasNext;
        first = false;
        return hasNext;
    }

    @Override
    public String next() {
        if(!hasNext()) {
            throw new NoSuchElementException();
        }
        hasNext = false;
        StringBuilder result = new StringBuilder();
        try {
            root.print(solver, result);
        } catch (IOException e) {
            throw new RuntimeException("StringBuilder should not throw IO exceptions!", e);
        }
        return result.toString();
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException("Remove not supported");
    }
}
