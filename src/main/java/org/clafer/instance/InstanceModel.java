package org.clafer.instance;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class InstanceModel {

    private final InstanceClafer[] topClafers;

    public InstanceModel(InstanceClafer... topClafers) {
        this.topClafers = Check.noNulls(topClafers);
    }

    public InstanceClafer[] getTopClafers() {
        return topClafers;
    }

    public InstanceClafer[] getTopClafers(AstConcreteClafer type) {
        List<InstanceClafer> typedTopClafers = new ArrayList<>();
        for (InstanceClafer topClafer : topClafers) {
            if (type.equals(topClafer.getType())) {
                typedTopClafers.add(topClafer);
            }
        }
        return typedTopClafers.toArray(new InstanceClafer[typedTopClafers.size()]);
    }

    public InstanceClafer getTopClafer(AstConcreteClafer type) {
        InstanceClafer typedTopClafer = null;
        for (InstanceClafer topClafer : topClafers) {
            if (type.equals(topClafer.getType())) {
                if (typedTopClafer != null) {
                    throw new IllegalArgumentException("More than one top Clafer with type " + type);
                }
                typedTopClafer = topClafer;
            }
        }
        if (typedTopClafer == null) {
            throw new IllegalArgumentException("No top Clafer with type " + type);
        }
        return typedTopClafer;
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
        for (InstanceClafer top : topClafers) {
            top.print(out);
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
