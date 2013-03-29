package org.clafer.ast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class AstClafer {

    private final String name;
    private AstAbstractClafer superClafer;
    private AstRef ref;
    private Card groupCard = new Card();
    private final List<AstConcreteClafer> children = new ArrayList<AstConcreteClafer>();
    private final List<AstBoolExpr> constraints = new ArrayList<AstBoolExpr>();

    AstClafer(String name) {
        this.name = Check.notNull(name);
    }

    public String getName() {
        return name;
    }

    public boolean hasSuperClafer() {
        return superClafer != null;
    }

    public AstAbstractClafer getSuperClafer() {
        return superClafer;
    }

    public AstClafer extending(AstAbstractClafer superClafer) {
        if (hasSuperClafer()) {
            throw new IllegalArgumentException(this + " already has a super clafer");
        }
        Check.notNull(superClafer).addSub(this);
        this.superClafer = superClafer;
        return this;
    }

    public boolean hasRef() {
        return ref != null;
    }

    public AstRef getRef() {
        return ref;
    }

    public AstClafer refTo(AstClafer targetType) {
        if (hasRef()) {
            throw new IllegalArgumentException(this + " already has a ref");
        }
        this.ref = new AstRef(this, targetType, false);
        return this;
    }

    public AstClafer refToUnique(AstClafer targetType) {
        if (hasRef()) {
            throw new IllegalArgumentException(this + " already has a ref");
        }
        this.ref = new AstRef(this, targetType, true);
        return this;
    }

    public boolean hasGroupCard() {
        return groupCard != null;
    }

    public Card getGroupCard() {
        return groupCard;
    }

    public AstClafer withGroupCard(Card groupCard) {
        this.groupCard = Check.notNull(groupCard);
        return this;
    }

    public AstClafer withGroupCard(int low) {
        return withGroupCard(new Card(low));
    }

    public AstClafer withGroupCard(int low, int high) {
        return withGroupCard(new Card(low, high));
    }

    public boolean hasChildren() {
        return !children.isEmpty();
    }

    public List<AstConcreteClafer> getChildren() {
        return Collections.unmodifiableList(children);
    }

    public AstConcreteClafer addChild(String name) {
        AstConcreteClafer child = new AstConcreteClafer(name, this);
        children.add(child);
        return child;
    }

    public AstClafer withChildren(List<AstConcreteClafer> children) {
        this.children.clear();
        this.children.addAll(children);
        return this;
    }

    public boolean hasConstraints() {
        return !constraints.isEmpty();
    }

    public List<AstBoolExpr> getConstraints() {
        return Collections.unmodifiableList(constraints);
    }

    public void addConstraint(AstBoolExpr constraint) {
        constraints.add(constraint);
    }

    public AstClafer withConstraints(List<AstBoolExpr> constraints) {
        this.constraints.clear();
        this.constraints.addAll(constraints);
        return this;
    }
}
