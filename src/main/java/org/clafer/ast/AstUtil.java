package org.clafer.ast;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Various static utility functions for working with AST.
 * 
 * @author jimmy
 */
public class AstUtil {

    private AstUtil() {
    }

    /**
     * Find all the Clafers nested below the root in no specific order.
     * 
     * @param model the Clafer model
     * @return the Clafers in the model
     */
    public static List<AstClafer> getClafers(AstModel model) {
        List<AstClafer> clafers = new ArrayList<AstClafer>();
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            clafers.add(abstractClafer);
            getNestedChildClafers(abstractClafer, clafers);
        }
        for (AstConcreteClafer topClafer : model.getTopClafers()) {
            clafers.add(topClafer);
            getNestedChildClafers(topClafer, clafers);
        }
        return clafers;
    }

    /**
     * Find all the concrete Clafers nested below the root in no specific order.
     * 
     * @param model the Clafer model
     * @return the Clafers in the model
     */
    public static List<AstConcreteClafer> getConcreteClafers(AstModel model) {
        List<AstConcreteClafer> clafers = new ArrayList<AstConcreteClafer>();
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            getNestedChildClafers(abstractClafer, clafers);
        }
        for (AstConcreteClafer topClafer : model.getTopClafers()) {
            clafers.add(topClafer);
            getNestedChildClafers(topClafer, clafers);
        }
        return clafers;
    }

    /**
     * Find the highest non-root Clafer above the supplied one.
     * 
     * @param clafer the Clafer
     * @return the highest non-root ancestor
     */
    public static AstClafer getTopParent(AstClafer clafer) {
        if (clafer instanceof AstConcreteClafer) {
            AstConcreteClafer concrete = (AstConcreteClafer) clafer;
            if (concrete.hasParent()) {
                return getTopParent(concrete.getParent());
            }
        }
        return clafer;
    }

    /**
     * Find all the Clafers below the supplied one, including itself.
     * 
     * @param clafer the Clafer
     * @return the nested Clafers
     */
    public static List<AstClafer> getNestedClafers(AstClafer clafer) {
        List<AstClafer> clafers = new ArrayList<AstClafer>();
        clafers.add(clafer);
        getNestedChildClafers(clafer, clafers);
        return clafers;
    }

    private static void getNestedChildClafers(AstClafer clafer, List<? super AstConcreteClafer> clafers) {
        for (AstConcreteClafer child : clafer.getChildren()) {
            clafers.add(child);
            getNestedChildClafers(child, clafers);
        }
    }

    /**
     * Find all the concrete subtypes of the supplied Clafer, including itself
     * if it is concrete.
     * 
     * @param clafer the Clafer
     * @return the concrete sub-Clafers.
     */
    public static List<AstConcreteClafer> getConcreteSubs(AstClafer clafer) {
        if (clafer instanceof AstConcreteClafer) {
            return Collections.singletonList((AstConcreteClafer) clafer);
        }
        List<AstConcreteClafer> subs = new ArrayList<AstConcreteClafer>();
        getConcreteSubs(clafer, subs);
        return subs;
    }

    private static void getConcreteSubs(AstClafer sub, Collection<AstConcreteClafer> subs) {
        if (sub instanceof AstAbstractClafer) {
            AstAbstractClafer sup = (AstAbstractClafer) sub;
            for (AstClafer subsub : sup.getSubs()) {
                getConcreteSubs(subsub, subs);
            }
        } else {
            subs.add((AstConcreteClafer) sub);
        }
    }

    /**
     * 
     * @param clafer
     * @return 
     */
    public static List<AstAbstractClafer> getSupers(final AstClafer clafer) {
        List<AstAbstractClafer> supers = new ArrayList<AstAbstractClafer>();
        AstAbstractClafer sup = clafer.getSuperClafer();
        while (sup != null) {
            supers.add(sup);
            sup = sup.getSuperClafer();
        }
        return supers;
    }

    /**
     * Detects if a set of {@code from} is also a set of {@code to}.
     * 
     * @param from the subtype
     * @param to the supertype
     * @return {@code true} if and only if to is a supertype of from,
     *         {@code false} otherwise
     */
    public static boolean isAssignable(AstClafer from, AstClafer to) {
        return to.equals(from)
                || (to instanceof AstAbstractClafer
                && getSupers(from).contains((AstAbstractClafer) to));
    }

    /**
     * Detects if a set of {@code t1} can share elements with a set of {@code t2}.
     * 
     * @param t1 first type
     * @param t2 second type
     * @return {@code true} if and only if the first and second type intersect,
     *         {@code false} otherwise
     */
    public static boolean hasNonEmptyIntersectionType(AstClafer t1, AstClafer t2) {
        if (t1.equals(t2)) {
            return true;
        }
        if (t1 instanceof AstAbstractClafer
                && getSupers(t2).contains((AstAbstractClafer) t1)) {
            return true;
        }
        if (t2 instanceof AstAbstractClafer
                && getSupers(t1).contains((AstAbstractClafer) t2)) {
            return true;
        }
        return false;
    }

    /**
     * Detects if the union type is fully covered by the partitions. For example,
     * {@code isUnionType(Animal, Arrays.asList(Primate, Human, Dolphin))} would
     * return {@code true} if and only if primates, humans, and dolphins are the
     * only animals. If humans are primates, the result would still hold.
     * 
     * @param union the union type
     * @param partitions the partition of the union type
     * @return {@code true} if and only if the partitions fully cover the union
     *         type, {@code false} otherwise
     */
    public static boolean isUnionType(AstClafer union, Collection<AstClafer> partitions) {
        Set<AstConcreteClafer> unionSubs = new HashSet<AstConcreteClafer>();
        getConcreteSubs(union, unionSubs);
        Set<AstConcreteClafer> partitionSubs = new HashSet<AstConcreteClafer>();
        for (AstClafer partition : partitions) {
            getConcreteSubs(partition, partitionSubs);
        }
        return unionSubs.equals(partitionSubs);
    }

    /**
     * Find the reference belonging to the Clafer or the reference it inherited.
     * 
     * @param clafer the Clafer
     * @return the reference, or {@code null} if none exist
     */
    public static AstRef getInheritedRef(AstClafer clafer) {
        if (clafer.hasRef()) {
            return clafer.getRef();
        }
        if (clafer.hasSuperClafer()) {
            return getInheritedRef(clafer.getSuperClafer());
        }
        return null;
    }
}
