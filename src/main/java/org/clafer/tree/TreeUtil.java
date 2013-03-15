package org.clafer.tree;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author jimmy
 */
public class TreeUtil {

    public static List<Clafer> getAllNestedClafers(AtomicClafer clafer) {
        List<Clafer> nestedClafers = new ArrayList<Clafer>();
        getAllNestedClafers(clafer, nestedClafers);
        return nestedClafers;
    }

    private static void getAllNestedClafers(AtomicClafer clafer, List<Clafer> nestedClafers) {
        nestedClafers.add(clafer);
        for (AtomicClafer child : clafer.getChildren()) {
            if (child.hasRef()) {
                nestedClafers.add(child.getRef());
            }
            getAllNestedClafers(child, nestedClafers);
        }
    }

//    private static List<ClaferConstraint> concatMapGetConstraints(Iterable<AtomicClafer> clafers) {
//        List<ClaferConstraint> constraints = new ArrayList<ClaferConstraint>();
//        for (AtomicClafer clafer : clafers) {
//            constraints.addAll(clafer.getConstraints());
//        }
//        return constraints;
//    }
}
