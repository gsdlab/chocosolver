package org.clafer.ast;

import java.util.ArrayList;
import java.util.Collections;
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
public class AstModel extends AstConcreteClafer {

    private final List<AstAbstractClafer> abstractClafers;

    AstModel() {
        super("#root#");
        super.withCard(new Card(1, 1));
        this.abstractClafers = new ArrayList<AstAbstractClafer>();
    }

    public List<AstAbstractClafer> getAbstractClafers() {
        return Collections.unmodifiableList(abstractClafers);
    }

    public AstAbstractClafer addAbstractClafer(String name) {
        AstAbstractClafer abstractClafer = new AstAbstractClafer(name);
        abstractClafers.add(abstractClafer);
        return abstractClafer;
    }

    public AstModel withAbstractClafers(List<AstAbstractClafer> abstractClafers) {
        this.abstractClafers.clear();
        this.abstractClafers.addAll(abstractClafers);
        return this;
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
