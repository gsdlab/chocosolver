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

    private final AstAbstractClafer parent;
    private final List<AstAbstractClafer> abstractChildren = new ArrayList<>();
    private final List<AstClafer> subs = new ArrayList<>();

    AstAbstractClafer(String name, AstAbstractClafer claferClafer) {
        super(name, claferClafer);
        this.parent = null;
    }

    AstAbstractClafer(String name, AstAbstractClafer parent, AstAbstractClafer claferClafer) {
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

    public boolean hasAbstractChildren() {
        return !abstractChildren.isEmpty();
    }

    public List<AstAbstractClafer> getAbstractChildren() {
        return Collections.unmodifiableList(abstractChildren);
    }

    public AstAbstractClafer addAbstractChild(String name) {
        AstAbstractClafer child = new AstAbstractClafer(name, this, getClaferClafer());
        if (getClaferClafer() != null) {
            child.extending(getClaferClafer());
        }
        abstractChildren.add(child);
        return child;
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
    public AstAbstractClafer getParent() {
        return parent;
    }

    @Override
    public String toString() {
        return "abstract " + getName();
    }
}
