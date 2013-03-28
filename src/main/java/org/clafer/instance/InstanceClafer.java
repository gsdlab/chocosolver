package org.clafer.instance;

import java.io.IOException;
import org.clafer.Check;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstIntClafer;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstUtil;

/**
 *
 * @author jimmy
 */
public class InstanceClafer {

    private final AstClafer type;
    private final int id;
    private final int ref;
    private final InstanceClafer[] children;

    public InstanceClafer(AstClafer type, int id, int ref, InstanceClafer[] children) {
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

    public int getRef() {
        return ref;
    }

    public InstanceClafer[] getChildren() {
        return children;
    }

    /**
     * Print solution to stdout.
     */
    public void print() throws IOException {
        print(System.out);
    }

    /**
     * Print solution.
     */
    public void print(Appendable out) throws IOException {
        print("", out);
    }

    private void print(String indent, Appendable out) throws IOException {
        out.append(indent).append(type.getName()).append(Integer.toString(id));
        AstRef typeRef = AstUtil.getInheritedRef(type);
        if (typeRef != null) {
            out.append(" = ");
            if (!(typeRef.getTargetType() instanceof AstIntClafer)) {
                out.append(typeRef.getTargetType().getName());
            }
            out.append(Integer.toString(ref));
        }
        out.append('\n');
        for (InstanceClafer child : getChildren()) {
            child.print(indent + "    ", out);
        }
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
