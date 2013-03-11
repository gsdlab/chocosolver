package org.clafer.tree;

import choco.kernel.model.variables.set.SetVariable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.clafer.Check;
import org.clafer.Util;

/**
 *
 * @author jimmy
 */
public abstract class AtomicClafer extends Clafer {

    private final SetVariable set;
    private final List<AtomicClafer> children = new ArrayList<AtomicClafer>();
    private RefClafer ref = null;

    public AtomicClafer(String name, int scope, SetVariable set) {
        super(name, scope);
        this.set = Check.notNull(set);
    }

    public SetVariable getSet() {
        return set;
    }

    void addChild(AtomicClafer child) {
        children.add(child);
    }

    public boolean hasChild(AtomicClafer child) {
        return children.contains(child);
    }

    public boolean hasChildren() {
        return !children.isEmpty();
    }

    public List<AtomicClafer> getChildren() {
        return Collections.unmodifiableList(children);
    }

    public boolean hasRef() {
        return ref != null;
    }

    public RefClafer getRef() {
        return ref;
    }

    void setRef(RefClafer ref) {
        if (hasRef()) {
            throw new IllegalStateException(getName() + " already has a reference");
        }
        this.ref = ref;
    }

    public List<Clafer> getRefAndChildren() {
        return hasRef()
                ? Util.cons(ref, children)
                : Collections.<Clafer>unmodifiableList(children);
    }

    public List<AtomicClafer> getNestedChildren() {
        List<AtomicClafer> nested = new ArrayList<AtomicClafer>();
        for (AtomicClafer child : getChildren()) {
            nested.add(child);
            nested.addAll(child.getNestedChildren());
        }
        return nested;
    }
}
