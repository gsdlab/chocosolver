package org.clafer.instance;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.clafer.common.Check;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;

/**
 *
 * @author jimmy
 */
public class InstanceClafer {

    private final AstClafer type;
    private final int id;
    private final InstanceRef ref;
    private final InstanceClafer[] children;

    public InstanceClafer(AstClafer type, int id, InstanceRef ref, InstanceClafer... children) {
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

    public InstanceRef getRef() {
        return ref;
    }

    public boolean hasChildren() {
        return children.length != 0;
    }

    public InstanceClafer[] getChildren() {
        return children;
    }
    
    public InstanceClafer[] getChildren(AstConcreteClafer type) {
        List<InstanceClafer> typedChildren = new ArrayList<InstanceClafer>();
        for(InstanceClafer child : children) {
            if(type.equals(child.getType())) {
                typedChildren.add(child);
            }
        }
        return typedChildren.toArray(new InstanceClafer[typedChildren.size()]);
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
        out.append(indent).append(type.getName()).append("#").append(Integer.toString(id));
        if(hasRef()) {
            out.append(" = ").append(ref.toString());
        }
//        AstRef typeRef = AstUtil.getInheritedRef(type);
//        if (typeRef != null) {
//            out.append(" = ");
//            if (!(typeRef.getTargetType() instanceof AstIntClafer)) {
//                out.append(typeRef.getTargetType().getName()).append("#");
//            }
//            out.append(Integer.toString(ref));
//        }
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
