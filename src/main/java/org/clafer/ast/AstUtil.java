package org.clafer.ast;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import org.clafer.ast.analysis.AnalysisException;
import org.clafer.common.Check;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.graph.Vertex;

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
        List<AstClafer> clafers = new ArrayList<>();
        clafers.add(model);
        for (AstAbstractClafer abstractClafer : model.getAbstracts()) {
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
     * @return the concrete Clafers in the model excluding the root
     */
    public static List<AstConcreteClafer> getConcreteClafers(AstModel model) {
        List<AstConcreteClafer> clafers = new ArrayList<>();
        clafers.add(model);
        for (AstAbstractClafer abstractClafer : model.getAbstracts()) {
            getNestedChildClafers(abstractClafer, clafers);
        }
        for (AstConcreteClafer child : model.getChildren()) {
            clafers.add(child);
            getNestedChildClafers(child, clafers);
        }
        return clafers;
    }

    /**
     * Find the Clafers such that the parents appear before the children, and
     * the subclafers appear before the superclafers.
     *
     * @param model the Clafer model
     * @return the Clafers in the above order
     */
    public static List<Set<AstClafer>> getClafersInParentAndSubOrder(AstModel model) {
        KeyGraph<AstClafer> dependency = new KeyGraph<>();
        for (AstAbstractClafer abstractClafer : model.getAbstracts()) {
            Vertex<AstClafer> node = dependency.getVertex(abstractClafer);
            for (AstClafer sub : abstractClafer.getSubs()) {
                node.addNeighbour(dependency.getVertex(sub));
            }
        }
        for (AstConcreteClafer concreteClafer : getConcreteClafers(model)) {
            if (concreteClafer.hasParent()) {
                dependency.addEdge(concreteClafer, concreteClafer.getParent());
            }
        }
        return GraphUtil.computeStronglyConnectedComponents(dependency);
    }

    /**
     * Find the abstract Clafers such that the subclafers appear before the
     * super clafers.
     *
     * @param model the Clafer model
     * @return the abstract Clafers in the above order
     */
    public static List<AstAbstractClafer> getAbstractClafersInSubOrder(AstModel model) {
        List<AstAbstractClafer> clafers = new ArrayList<>(model.getAbstracts());
        Collections.sort(clafers, new Comparator<AstAbstractClafer>() {
            @Override
            public int compare(AstAbstractClafer o1, AstAbstractClafer o2) {
                int depth1 = AstUtil.getSuperHierarchy(o1).size();
                int depth2 = AstUtil.getSuperHierarchy(o2).size();
                return depth1 > depth2 ? -1 : (depth1 == depth2 ? 0 : 1);
            }
        });
        return clafers;
    }

    /**
     * Find the concrete Clafers such that the parents appear before the
     * children.
     *
     * @param model the Clafer model
     * @return the concrete Clafers in the above order
     */
    public static List<AstConcreteClafer> getConcreteClafersInParentOrder(AstModel model) {
        // The current implementation of getConreteClafers implements parent order.
        return getConcreteClafers(model);
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
     * Find all the Clafers below the supplied one, including itself.
     *
     * @param clafer the Clafer
     * @return the nested Clafers
     */
    public static List<AstClafer> getNestedClafers(AstClafer clafer) {
        List<AstClafer> clafers = new ArrayList<>();
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
        List<AstConcreteClafer> subs = new ArrayList<>();
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
        List<AstConstraint> constraints = new ArrayList<>();
        for (AstAbstractClafer abstractClafer : model.getAbstracts()) {
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
     * Finds all the supertypes of the Clafer in order of lowest to highest.
     *
     * @param clafer the Clafer
     * @return the supertypes
     */
    public static List<AstAbstractClafer> getSupers(final AstClafer clafer) {
        List<AstAbstractClafer> supers = new ArrayList<>();
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
     * @throws AnalysisException a cycle in the type hierarchy
     */
    public static List<AstClafer> getSuperHierarchy(final AstClafer clafer) throws AnalysisException {
        List<AstClafer> supers = new ArrayList<>();
        AstClafer sup = clafer;
        while (sup != null) {
            if (supers.contains(sup)) {
                throw new AnalysisException("Cycle in type hierarchy " + supers);
            }
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
     * Finds the lowest common supertype.
     *
     * @param unionType the union type
     * @return the lowest common supertype of {@code unionType}
     */
    public static AstClafer getLowestCommonSupertype(Iterable<AstClafer> unionType) {
        Iterator<AstClafer> iter = unionType.iterator();
        if (!iter.hasNext()) {
            throw new IllegalArgumentException();
        }
        AstClafer first = iter.next();
        if (!iter.hasNext()) {
            return first;
        }
        List<AstClafer> supers = getSuperHierarchy(first);
        while (iter.hasNext()) {
            List<AstClafer> otherSupers = getSuperHierarchy(iter.next());
            supers = getUnionTypeHierarchy(supers, otherSupers);
            if (supers.isEmpty()) {
                return null;
            }
        }
        return supers.get(0);
    }

    /**
     * Finds the lowest common supertype.
     *
     * @param unionType the union type
     * @return the lowest common supertype of {@code unionType}
     */
    public static AstClafer getLowestCommonSupertype(AstClafer... unionType) {
        return getLowestCommonSupertype(Arrays.asList(unionType));
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
        List<String> names = new ArrayList<>();
        for (AstVar var : vars) {
            names.add(var.getName());
        }
        return names;
    }
}
