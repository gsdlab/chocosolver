package org.clafer.ast.analysis;

import org.clafer.ast.ProductType;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import org.clafer.ast.AstClafer;

/**
 * Sum type.
 *
 * @author jimmy
 */
public class Type implements Iterable<ProductType> {

    private final Set<ProductType> unionType;
    private final ProductType commonSupertype;

    public Type(Set<AstClafer> unionType, AstClafer commonSupertype) {
        if (unionType.isEmpty()) {
            throw new IllegalArgumentException();
        }
        this.unionType = new LinkedHashSet<>(unionType.size());
        for (AstClafer type : unionType) {
            this.unionType.add(new ProductType(type));
        }
        this.commonSupertype = new ProductType(commonSupertype);
    }

    public Type(ProductType productType) {
        this(Collections.singleton(productType));
    }

    public Type(Set<ProductType> unionType) {
        if (unionType.isEmpty()) {
            throw new IllegalArgumentException();
        }
        this.unionType = unionType;
        this.commonSupertype = ProductType.getLowestCommonSupertype(unionType);
    }

    public Type(Set<ProductType> unionType, ProductType commonSupertype) {
        if (unionType.isEmpty()) {
            throw new IllegalArgumentException();
        }
        this.unionType = unionType;
        this.commonSupertype = commonSupertype;
    }

    public int arity() {
        return unionType.size();
    }

    public Set<ProductType> getUnionType() {
        return unionType;
    }

    public ProductType getCommonSupertype() {
        return commonSupertype;
    }

    public boolean isBasicType() {
        return unionType.size() == 1;
    }

    public ProductType getBasicType() {
        assert isBasicType();
        return unionType.iterator().next();
    }

    public static Type basicType(AstClafer type) {
        return new Type(Collections.singleton(new ProductType(type)));
    }

    public boolean isClaferType() {
        return unionType.size() == 1 && unionType.iterator().next().arity() == 1;
    }

    public AstClafer getClaferType() {
        assert isClaferType() : this + " is not a Clafer type";
        return unionType.iterator().next().getProduct()[0];
    }

    public boolean isPrimitive() {
        return unionType.size() == 1 && unionType.iterator().next().isPrimitive();
    }

    public boolean isBool() {
        return unionType.size() == 1 && unionType.iterator().next().isBool();
    }

    public boolean isInt() {
        return unionType.size() == 1 && unionType.iterator().next().isInt();
    }

    public boolean isString() {
        return unionType.size() == 1 && unionType.iterator().next().isString();
    }

    @Override
    public Iterator<ProductType> iterator() {
        return unionType.iterator();
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
        return unionType.size() == 1 ? commonSupertype.toString() : unionType.toString();
    }
}
