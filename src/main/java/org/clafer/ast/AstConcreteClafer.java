package org.clafer.ast;

import java.util.List;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstConcreteClafer extends AstClafer {

    private final AstClafer parent;
    private Card card = new Card();

    AstConcreteClafer(String name) {
        super(name);
        this.parent = null;
    }

    AstConcreteClafer(String name, AstClafer parent) {
        super(name);
        this.parent = Check.notNull(parent);
    }

    public boolean hasParent() {
        return parent != null;
    }

    public AstClafer getParent() {
        return parent;
    }

    public Card getCard() {
        return card;
    }

    public AstConcreteClafer withCard(Card card) {
        this.card = Check.notNull(card);
        return this;
    }

    public AstConcreteClafer withCard(int low) {
        return withCard(new Card(low));
    }

    public AstConcreteClafer withCard(int low, int high) {
        return withCard(new Card(low, high));
    }

    @Override
    public AstConcreteClafer withGroupCard(Card groupCard) {
        super.withGroupCard(groupCard);
        return this;
    }

    @Override
    public AstConcreteClafer withGroupCard(int low) {
        return withGroupCard(new Card(low));
    }

    @Override
    public AstConcreteClafer withGroupCard(int low, int high) {
        return withGroupCard(new Card(low, high));
    }

    @Override
    public AstConcreteClafer extending(AstAbstractClafer superClafer) {
        super.extending(superClafer);
        return this;
    }

    @Override
    public AstConcreteClafer refTo(AstClafer targetType) {
        super.refTo(targetType);
        return this;
    }

    @Override
    public AstConcreteClafer refToUnique(AstClafer targetType) {
        super.refToUnique(targetType);
        return this;
    }

    @Override
    public AstConcreteClafer withChildren(List<AstConcreteClafer> newChildren) {
        super.withChildren(newChildren);
        return this;
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return getName().hashCode();
    }

    @Override
    public String toString() {
        return getName();
    }
}
