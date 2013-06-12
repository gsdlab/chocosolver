package org.clafer.ast;

import gnu.trove.map.TIntObjectMap;
import gnu.trove.map.hash.TIntObjectHashMap;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.clafer.common.Check;

/**
 * Various static utility functions for working with AST.
 *
 * @author jimmy
 */
public class AstUtil {

    private AstUtil() {
    }

    /**
     * Find all the nested Clafers in no specific order.
     *
     * @param model the Clafer model
     * @return the Clafers in the model
     */
    public static List<AstClafer> getClafers(AstModel model) {
        List<AstClafer> clafers = new ArrayList<AstClafer>();
        clafers.add(model);
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            clafers.add(abstractClafer);
            getNestedChildClafers(abstractClafer, clafers);
        }
        for (AstConcreteClafer child : model.getChildren()) {
            clafers.add(child);
            getNestedChildClafers(child, clafers);
        }
        return clafers;
    }

    /**
     * Find all the nested concrete Clafers in no specific order.
     *
     * @param model the Clafer model
     * @return the Clafers in the model excluding the root
     */
    public static List<AstConcreteClafer> getConcreteClafers(AstModel model) {
        List<AstConcreteClafer> clafers = new ArrayList<AstConcreteClafer>();
        clafers.add(model);
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            getNestedChildClafers(abstractClafer, clafers);
        }
        for (AstConcreteClafer child : model.getChildren()) {
            clafers.add(child);
            getNestedChildClafers(child, clafers);
        }
        return clafers;
    }

    /**
     * Detects if the Clafer is the implicit root of the model.
     *
     * @param clafer the Clafer
     * @return {@code true} if and only if the Clafer is the root, {@code false}
     * otherwise
     */
    public static boolean isRoot(AstConcreteClafer clafer) {
        return clafer instanceof AstModel;
    }

    /**
     * Detects if the Clafer is the implicit type root of the model.
     *
     * @param clafer the Clafer
     * @return {@code true} if and only if the Clafer is the type root,
     * {@code false} otherwise
     */
    public static boolean isTypeRoot(AstAbstractClafer clafer) {
        return clafer instanceof AstAbstractClafer && !clafer.hasSuperClafer();
    }

    /**
     * Detects if the Clafer is directly under the root.
     *
     * @param clafer the Clafer
     * @return {@code true} if and only if the Clafer is a top Clafer,
     * {@code false} otherwise
     */
    public static boolean isTop(AstClafer clafer) {
        if (clafer instanceof AstConcreteClafer) {
            AstConcreteClafer concrete = (AstConcreteClafer) clafer;
            return concrete.getParent() instanceof AstConcreteClafer
                    && isRoot((AstConcreteClafer) concrete.getParent());
        }
        return clafer instanceof AstAbstractClafer;
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
            if (!concrete.hasParent()) {
                throw new IllegalArgumentException("Root does not have a non-root parent.");
            }
            if (!(concrete.getParent() instanceof AstConcreteClafer
                    && isRoot((AstConcreteClafer) concrete.getParent()))) {
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
     * Find all the constraints in the model.
     *
     * @param model the model
     * @return the nested constraints
     */
    public static List<AstConstraint> getNestedConstraints(AstModel model) {
        List<AstConstraint> constraints = new ArrayList<AstConstraint>();
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            getNestedConstraints(abstractClafer, constraints);
        }
        getNestedConstraints(model, constraints);
        return constraints;
    }

    private static void getNestedConstraints(AstClafer clafer, List<AstConstraint> constraints) {
        constraints.addAll(clafer.getConstraints());
        for (AstConcreteClafer child : clafer.getChildren()) {
            getNestedConstraints(child, constraints);
        }
    }

    /**
     * Map the identifiers to the constraints in the model.
     *
     * @param model the model
     * @return the mapping of identifiers to constraints
     */
    public static TIntObjectMap<AstConstraint> getConstraintMap(AstModel model) {
        List<AstConstraint> constraints = getNestedConstraints(model);
        TIntObjectMap<AstConstraint> map = new TIntObjectHashMap<AstConstraint>(constraints.size());
        for (AstConstraint constraint : constraints) {
            map.put(constraint.getId(), constraint);
        }
        return map;
    }

    /**
     * Finds all the supertypes of the Clafer in order of lowest to highest.
     *
     * @param clafer the Clafer
     * @return the supertypes
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
     * Finds the supertype hierarchy of the Clafer. Equivalent to the Haskell
     * code {@code clafer : getSupers clafer}.
     *
     * @param clafer the Clafer
     * @return the supertype hierarchy
     */
    public static List<AstClafer> getSuperHierarchy(final AstClafer clafer) {
        List<AstClafer> supers = new ArrayList<AstClafer>();
        supers.add(clafer);
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
     * {@code false} otherwise
     */
    public static boolean isAssignable(AstClafer from, AstClafer to) {
        return to.equals(from)
                || (to instanceof AstAbstractClafer
                && getSupers(from).contains((AstAbstractClafer) to));
    }

    /**
     * Detects if a set of {@code t1} can share elements with a set of
     * {@code t2}.
     *
     * @param t1 first type
     * @param t2 second type
     * @return {@code true} if and only if the first and second type intersect,
     * {@code false} otherwise
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
     * Detects if the union type is fully covered by the partitions. For
     * example,
     * {@code isUnionType(Animal, Arrays.asList(Primate, Human, Dolphin))} would
     * return {@code true} if and only if primates, humans, and dolphins are the
     * only animals. If humans are primates, the result would still hold.
     *
     * @param union the union type
     * @param partitions the partition of the union type
     * @return {@code true} if and only if the partitions fully cover the union
     * type, {@code false} otherwise
     */
    public static boolean isUnionType(AstClafer union, AstClafer[] partitions) {
        Set<AstConcreteClafer> unionSubs = new HashSet<AstConcreteClafer>();
        getConcreteSubs(union, unionSubs);
        Set<AstConcreteClafer> partitionSubs = new HashSet<AstConcreteClafer>();
        for (AstClafer partition : partitions) {
            getConcreteSubs(partition, partitionSubs);
        }
        return unionSubs.equals(partitionSubs);
    }

    private static List<AstClafer> getUnionTypeHierarchy(
            List<AstClafer> typeHierarchy1, List<AstClafer> typeHierarchy2) {
        if (typeHierarchy1.size() > typeHierarchy2.size()) {
            return getUnionTypeHierarchy(typeHierarchy2, typeHierarchy1);
        }
        for (int i = 1; i <= typeHierarchy1.size(); i++) {
            if (!typeHierarchy1.get(typeHierarchy1.size() - i).equals(
                    typeHierarchy2.get(typeHierarchy2.size() - i))) {
                return typeHierarchy1.subList(typeHierarchy1.size() - i + 1, typeHierarchy1.size());
            }
        }
        return typeHierarchy1;
    }

    /**
     * Finds the lowest common supertype or null if none.
     *
     * @param partitions the partitions of the union type
     * @return the lowest common supertype of {@code partitions}
     */
    public static AstClafer getUnionType(AstClafer... partitions) {
        List<AstClafer> supers = getSuperHierarchy(partitions[0]);
        for (int i = 1; i < partitions.length; i++) {
            List<AstClafer> otherSupers = getSuperHierarchy(partitions[i]);
            supers = getUnionTypeHierarchy(supers, otherSupers);
            if (supers.isEmpty()) {
                return null;
            }
        }
        return supers.get(0);
    }

    public static boolean hasInheritedRef(AstClafer clafer) {
        return getInheritedRef(clafer) != null;
    }

    /**
     * Find the reference belonging to the Clafer or the reference it inherited.
     *
     * @param clafer the Clafer
     * @return the reference, or {@code null} if none exist
     */
    public static AstRef getInheritedRef(AstClafer clafer) {
        AstClafer superClafer = Check.notNull(clafer);
        do {
            if (superClafer.hasRef()) {
                return superClafer.getRef();
            }
            superClafer = superClafer.getSuperClafer();
        } while (superClafer != null);
        return null;
    }

    /**
     * Retrieve the names of the variables. Use the names for error messages
     * rather than {@link Object#toString}.
     *
     * @param vars the variables
     * @return the names of the variables
     */
    public static String[] getNames(AstVar... vars) {
        String[] names = new String[vars.length];
        for (int i = 0; i < names.length; i++) {
            names[i] = vars[i].getName();
        }
        return names;
    }

    /**
     * Retrieve the names of the Clafers. Use the names for error messages
     * rather than {@link Object#toString}.
     *
     * @param vars the variables
     * @return the names of the variables
     */
    public static List<String> getNames(Iterable<? extends AstVar> vars) {
        List<String> names = new ArrayList<String>();
        for (AstVar var : vars) {
            names.add(var.getName());
        }
        return names;
    }
}
