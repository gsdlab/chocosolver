package org.clafer.ast;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author jimmy
 */
public class AstAbstractClafer extends AstClafer {

    private final List<AstClafer> subs = new ArrayList<AstClafer>();

    AstAbstractClafer(String name) {
        super(name);
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

    public List<AstClafer> getSubs() {
        return subs;
    }

    void addSub(AstClafer sub) {
        this.subs.add(sub);
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
    public AstAbstractClafer withChildren(List<AstConcreteClafer> newChildren) {
        super.withChildren(newChildren);
        return this;
    }

    @Override
    public AstAbstractClafer withConstraints(List<AstBoolExpr> constraints) {
        super.withConstraints(constraints);
        return this;
    }

    @Override
    public String toString() {
        return "abstract " + getName();
    }
}
