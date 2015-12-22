package org.clafer.ast;

import java.util.List;

/**
 * <p>
 * The Clafer model. Also acts as the implicit "root" Clafer which nests above
 * the top most Clafers.
 * </p>
 * <p>
 * For example:
 * <pre>
 * abstract Feature
 *     Cost -> integer
 * Database : Feature
 * </pre> Although the model is written syntactically like above, its
 * representation internally is more like:
 * <pre>
 * #root#
 *     abstract Feature
 *         Cost -> integer
 *     Database : Feature
 * </pre> {@code #root#} is represented by this class.
 * </p>
 *
 * @author jimmy
 */
public class AstModel {

    private final AstAbstractClafer abstractRootClafer;
    private final AstConcreteClafer rootClafer;
    // the type every non-primitive non-root type extends from.
    private final AstAbstractClafer typeRoot;

    AstModel() {
        this.abstractRootClafer = new AstAbstractClafer("#root#", null) {

            @Override
            protected AstAbstractClafer getClaferClafer() {
                return typeRoot;
            }
        };
        this.rootClafer = new AstConcreteClafer("root", null).extending(abstractRootClafer).withCard(1, 1);
        this.typeRoot = abstractRootClafer.addAbstractChild("#clafer#");
    }

    public AstAbstractClafer getAbstractRoot() {
        return abstractRootClafer;
    }

    public AstConcreteClafer getRoot() {
        return rootClafer;
    }

    public AstAbstractClafer getTypeRoot() {
        return typeRoot;
    }

    /**
     * Returns all the abstract Clafers
     *
     * @return all the abstract Clafers
     */
    public List<AstAbstractClafer> getAbstracts() {
        return abstractRootClafer.getAbstractChildren();
    }

    /**
     * Add a new abstract Clafer to the model. .
     *
     * @param name the name of the abtract Clafer
     * @return the new abstract Clafer
     */
    public AstAbstractClafer addAbstract(String name) {
        return abstractRootClafer.addAbstractChild(name);
    }

    public List<AstConcreteClafer> getChildren() {
        return abstractRootClafer.getChildren();
    }

    public AstConcreteClafer addChild(String name) {
        return abstractRootClafer.addChild(name);
    }

    public boolean hasConstraints() {
        return abstractRootClafer.hasConstraints();
    }

    public List<AstConstraint> getConstraints() {
        return abstractRootClafer.getConstraints();
    }

    public AstConstraint addConstraint(AstBoolExpr expr) {
        return abstractRootClafer.addConstraint(expr);
    }
}
