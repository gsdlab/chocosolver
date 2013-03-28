package org.clafer.instance;

import java.io.IOException;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class InstanceModel {

    private final InstanceClafer[] topClafers;

    public InstanceModel(InstanceClafer[] topClafers) {
        this.topClafers = Check.noNulls(topClafers);
    }

    public InstanceClafer[] getTopClafers() {
        return topClafers;
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
