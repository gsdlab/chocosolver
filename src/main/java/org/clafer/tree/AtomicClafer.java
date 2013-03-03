package org.clafer.tree;

import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public abstract class AtomicClafer extends Clafer {

    private final int scope;
    private final SetVariable set;
    private final List<Clafer> children = new ArrayList<Clafer>();

    public AtomicClafer(String name, int scope, SetVariable set) {
        super(name);
        this.scope = scope;
        this.set = Check.notNull(set);
    }

    public int getScope() {
        return scope;
    }

    public SetVariable getSet() {
        return set;
    }

    public void addChild(Clafer child) {
        children.add(child);
    }

    public List<Clafer> getChildren() {
        return Collections.unmodifiableList(children);
    }
}
