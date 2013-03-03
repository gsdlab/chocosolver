package org.clafer.tree;

import java.util.List;
import choco.kernel.solver.Solver;
import java.util.ArrayList;
import java.util.Collections;
import org.clafer.Check;
import static org.clafer.Exprs.*;

/**
 *
 * @author jimmy
 */
public class AbstractClafer extends AtomicClafer {

    private final List<AtomicClafer> subs = new ArrayList<AtomicClafer>();

    public AbstractClafer(String name, int scope, AtomicClafer... subclafers) {
        super(name, scope, setVar(name, 0, scope - 1));
    }

    @Override
    protected void print(Solver solver, String indent, int parent) {
        for (Clafer child : getChildren()) {
            child.print(solver, indent + "  ", parent);
        }
    }

    public void addSubclafer(AtomicClafer sub) {
        subs.add(Check.notNull(sub));
    }

    public List<AtomicClafer> getSubs() {
        return Collections.unmodifiableList(subs);
    }
}
