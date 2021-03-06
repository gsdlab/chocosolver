package org.clafer.instance;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.cli.Utils;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class InstanceClafer {

    private final AstClafer type;
    private final int id;
    private final Object ref;
    private final InstanceClafer[] children;

    public InstanceClafer(AstClafer type, int id, Object ref, InstanceClafer... children) {
        this.type = Check.notNull(type);
        this.id = id;
        this.ref = ref;
        this.children = Check.noNulls(children);
    }

    public AstClafer getType() {
        return type;
    }

    public int getId() {
        return id;
    }

    public boolean hasRef() {
        return ref != null;
    }

    public Object getRef() {
        return ref;
    }

    public boolean hasChildren() {
        return children.length != 0;
    }

    public InstanceClafer[] getChildren() {
        return children;
    }

    public InstanceClafer[] getChildren(AstConcreteClafer type) {
        List<InstanceClafer> typedChildren = new ArrayList<>();
        for (InstanceClafer child : children) {
            if (type.equals(child.getType())) {
                typedChildren.add(child);
            }
        }
        return typedChildren.toArray(new InstanceClafer[typedChildren.size()]);
    }

    public InstanceClafer getChild(AstConcreteClafer type) {
        InstanceClafer typedChild = null;
        for (InstanceClafer child : children) {
            if (type.equals(child.getType())) {
                if (typedChild != null) {
                    throw new IllegalArgumentException("More than one child Clafer with type " + type);
                }
                typedChild = child;
            }
        }
        if (typedChild == null) {
            throw new IllegalArgumentException("No child Clafer with type " + type);
        }
        return typedChild;
    }

    /**
     * Print solution to stdout.
     *
     * @throws IOException an IO error occurred
     */
    public void print() throws IOException {
        print(System.out);
    }

    /**
     * Print solution.
     *
     * @param out the stream to print to
     * @throws IOException an IO error occurred
     */
    public void print(Appendable out) throws IOException {
        print("", out);
    }

    private void print(String indent, Appendable out) throws IOException {
        out.append(indent).append(Utils.simpleName(getType().getName())).append(Utils.countSuffix(getId()));
        if (hasRef()) {
            out.append(" -> ");
            if (getRef() instanceof InstanceClafer) {
                InstanceClafer refClafer = (InstanceClafer) getRef();
                out.append(Utils.simpleName(refClafer.getType().getName())).append(Utils.countSuffix(refClafer.getId()));
            } else {
                out.append(getRef().toString());
            }
        }
        out.append('\n');
        for (InstanceClafer child : getChildren()) {
            child.print(indent + "  ", out);
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof InstanceClafer) {
            InstanceClafer other = (InstanceClafer) obj;
            return type.equals(other.type) && id == other.id
                    && Objects.equals(ref, other.ref) && Arrays.equals(children, other.children);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return type.hashCode() ^ id ^ Objects.hashCode(ref) ^ Arrays.hashCode(children);
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        try {
            print(result);
        } catch (IOException e) {
            // StringBuilder should not throw an IOException.
            throw new Error(e);
        }
        return result.toString();
    }
}
