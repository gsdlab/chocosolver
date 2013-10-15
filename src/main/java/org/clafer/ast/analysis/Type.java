package org.clafer.ast.analysis;

import java.util.Collections;
import java.util.Set;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstUtil;

/**
 *
 * @author jimmy
 */
public class Type {

    private final Set<AstClafer> unionType;
    private final AstClafer commonSuperType;

    public Type(Set<AstClafer> unionType, AstClafer commonSupertype) {
        if (unionType.isEmpty()) {
            throw new IllegalArgumentException();
        }
        this.unionType = unionType;
        this.commonSuperType = commonSupertype;
    }

    public Type(Set<AstClafer> unionType) {
        if (unionType.isEmpty()) {
            throw new IllegalArgumentException();
        }
        this.unionType = unionType;
        this.commonSuperType = AstUtil.getLowestCommonSupertype(unionType);
    }

    public Set<AstClafer> getUnionType() {
        return unionType;
    }

    public AstClafer getCommonSuperType() {
        return commonSuperType;
    }

    public boolean isBasicType() {
        return unionType.size() == 1;
    }

    public AstClafer getBasicType() {
        assert isBasicType();
        return unionType.iterator().next();
    }

    public static Type basicType(AstClafer type) {
        return new Type(Collections.singleton(type));
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Type) {
            Type other = (Type) obj;
            return unionType.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return unionType.hashCode();
    }

    @Override
    public String toString() {
        return unionType.size() == 1 ? commonSuperType.toString() : unionType.toString();
    }
}
