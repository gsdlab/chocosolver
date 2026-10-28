package org.clafer.ast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.clafer.common.Check;

/**
 * An abstract Clafer.
 *
 * @author jimmy
 */
public class AstAbstractClafer extends AstClafer {

    // An abstract or a concrete Clafer.
    private final AstClafer parent;
    private final List<AstClafer> subs = new ArrayList<>();

    AstAbstractClafer(String name, AstAbstractClafer claferClafer) {
        super(name, claferClafer);
        this.parent = null;
    }

    AstAbstractClafer(String name, AstClafer parent, AstAbstractClafer claferClafer) {
        super(name, claferClafer);
        this.parent = Check.notNull(parent);
    }

    @Override
    public AstAbstractClafer withGroupCard(Card groupCard) {
        super.withGroupCard(groupCard);
        return this;
    }

    @Override
    public AstAbstractClafer withGroupCard(int low) {
        return withGroupCard(new Card(low));
    }

    @Override
    public AstAbstractClafer withGroupCard(int low, int high) {
        return withGroupCard(new Card(low, high));
    }


    /**
     * Returns the Clafers that extend this Clafer
     *
     * @return this Clafers subtypes
     */
    public List<AstClafer> getSubs() {
        return Collections.unmodifiableList(subs);
    }

    void addSub(AstClafer sub) {
        this.subs.add(sub);
    }

    void removeSub(AstClafer sub) {
        this.subs.remove(sub);
    }

    @Override
    public AstAbstractClafer extending(AstAbstractClafer superClafer) {
        super.extending(superClafer);
        return this;
    }

    @Override
    public AstAbstractClafer refTo(AstClafer targetType) {
        super.refTo(targetType);
        return this;
    }

    @Override
    public AstAbstractClafer refToUnique(AstClafer targetType) {
        super.refToUnique(targetType);
        return this;
    }

    @Override
    public boolean hasParent() {
        return parent != null;
    }

    @Override
    public AstClafer getParent() {
        return parent;
    }

    @Override
    public String toString() {
        return "abstract " + getName();
    }
}
