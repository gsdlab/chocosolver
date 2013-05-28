package org.clafer.ast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.clafer.common.Check;

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
public class AstModel extends AstConcreteClafer {

    // The topmost Clafer in the type hierarchy.
    private final AstAbstractClafer claferClafer;
    private final List<AstAbstractClafer> abstractClafers;

    AstModel(AstIdFactory idFactory) {
        super("#root#", idFactory);
        super.withCard(new Card(1, 1));
        this.claferClafer = new AstAbstractClafer("#clafer#", idFactory);
        this.abstractClafers = new ArrayList<AstAbstractClafer>();
        this.abstractClafers.add(claferClafer);
    }
    
    AstModel() {
        this(AstIdFactory.newIdFactory());
    }

    /**
     * Returns the type every non-primitive type extends from.
     *
     * @return the Clafer named "clafer"
     */
    public AstAbstractClafer getTypeHierarchyRoot() {
        return claferClafer;
    }

    /**
     * Returns all the abstract Clafers
     *
     * @return all the abstract Clafers
     */
    public List<AstAbstractClafer> getAbstractClafers() {
        return Collections.unmodifiableList(abstractClafers);
    }

    /**
     * Add a new abstract Clafer to the model. .
     *
     * @param name the name of the abtract Clafer
     * @return the new abstract Clafer
     */
    public AstAbstractClafer addAbstractClafer(String name) {
        AstAbstractClafer abstractClafer = new AstAbstractClafer(name, idFactory).extending(claferClafer);
        abstractClafers.add(abstractClafer);
        return abstractClafer;
    }

    /**
     * Replace all the abstract Clafers.
     *
     * @param abstractClafers the new abstract Clafers
     * @return this model
     */
    public AstModel withAbstractClafers(List<AstAbstractClafer> abstractClafers) {
        this.abstractClafers.clear();
        this.abstractClafers.addAll(abstractClafers);
        return this;
    }

    @Override
    public AstConcreteClafer addChild(String name) {
        return super.addChild(name).extending(claferClafer);
    }

    @Override
    public AstModel extending(AstAbstractClafer superClafer) {
        throw new UnsupportedOperationException("Cannot extend from " + getName());
    }

    @Override
    public AstModel refTo(AstClafer targetType) {
        throw new UnsupportedOperationException("Cannot ref from " + getName());
    }

    @Override
    public AstModel refToUnique(AstClafer targetType) {
        throw new UnsupportedOperationException("Cannot ref from " + getName());
    }

    @Override
    public AstConcreteClafer withCard(Card card) {
        throw new UnsupportedOperationException("Cannot set cardinality for " + getName());
    }

    @Override
    public AstModel withGroupCard(Card groupCard) {
        throw new UnsupportedOperationException("Cannot set group cardinality for " + getName());
    }
}
