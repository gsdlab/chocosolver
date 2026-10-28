package org.clafer.instance;

import java.io.IOException;
import java.math.BigDecimal;
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
        print(out, false);
    }

    /**
     * Print solution.
     *
     * @param out the stream to print to
     * @param addTypes whether to print the super clafer of each clafer
     * @throws IOException an IO error occurred
     */
    public void print(Appendable out, boolean addTypes) throws IOException {
        print("", out, addTypes);
    }

    private void print(String indent, Appendable out, boolean addTypes) throws IOException {
        out.append(indent).append(Utils.simpleName(getType().getName())).append(Utils.countSuffix(getId()));
        if (addTypes && getType().hasSuperClafer()) {
            String superName = getType().getSuperClafer().getName();
            // do not print the default super type "clafer"
            if (!superName.equals("#clafer#")) {
                out.append(" : ").append(Utils.simpleName(superName));
            }
        }
        if (hasRef()) {
            out.append(" -> ");
            if (getRef() instanceof InstanceClafer) {
                InstanceClafer refClafer = (InstanceClafer) getRef();
                out.append(Utils.simpleName(refClafer.getType().getName())).append(Utils.countSuffix(refClafer.getId()));
            } else if (getRef() instanceof BigDecimal) {
                // Never scientific notation, e.g. 0.0000001 rather than 1E-7.
                out.append(((BigDecimal) getRef()).toPlainString());
            } else {
                out.append(getRef().toString());
            }
        }
        out.append('\n');
        for (InstanceClafer child : getChildren()) {
            child.print(indent + "  ", out, addTypes);
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
