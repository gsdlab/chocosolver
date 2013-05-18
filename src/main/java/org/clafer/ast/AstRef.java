package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstRef {

    private final AstClafer sourceType;
    private final AstClafer targetType;
    private final boolean unique;

    AstRef(AstClafer sourceType, AstClafer targetType, boolean unique) {
        this.sourceType = Check.notNull(sourceType);
        this.targetType = Check.notNull(targetType);
        this.unique = unique;
    }

    public AstClafer getSourceType() {
        return sourceType;
    }

    public AstClafer getTargetType() {
        return targetType;
    }

    public boolean isUnique() {
        return unique;
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return sourceType.hashCode() ^ targetType.hashCode();
    }

    @Override
    public String toString() {
        return sourceType.getName() + " . ref::" + targetType;
    }
}
