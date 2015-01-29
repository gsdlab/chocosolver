package org.clafer.ast;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import org.clafer.collection.ArrayIterator;
import org.clafer.common.Check;

/**
 * Product type.
 *
 * @author jimmy
 */
public class ProductType implements Iterable<AstClafer> {

    private final AstClafer[] types;

    public ProductType(AstClafer... types) {
        this.types = Check.noNullsNotEmpty(types);
    }

    public int arity() {
        return types.length;
    }

    public AstClafer get(int index) {
        return types[index];
    }

    public AstClafer[] getProduct() {
        return types;
    }

    public boolean isClaferType() {
        return types.length == 1;
    }

    public AstClafer getClaferType() {
        assert isClaferType();
        return types[0];
    }

    public boolean isPrimitive() {
        return types.length == 1 && types[0] instanceof AstPrimClafer;
    }

    public boolean isBool() {
        return types.length == 1 && types[0] instanceof AstBoolClafer;
    }

    public boolean isInt() {
        return types.length == 1 && types[0] instanceof AstIntClafer;
    }

    public boolean isString() {
        return types.length == 1 && types[0] instanceof AstStringClafer;
    }

    public static ProductType getLowestCommonSupertype(Collection<ProductType> ts) {
        return getLowestCommonSupertype(ts.toArray(new ProductType[ts.size()]));
    }

    public static ProductType getLowestCommonSupertype(ProductType... ts) {
        if (ts.length == 0) {
            throw new IllegalArgumentException();
        }
        int arity = ts[0].arity();
        for (int i = 1; i < ts.length; i++) {
            if (arity != ts[i].arity()) {
                throw new IllegalArgumentException();
            }
        }
        AstClafer[] product = new AstClafer[arity];
        for (int i = 0; i < product.length; i++) {
            AstClafer[] attribute = new AstClafer[ts.length];
            for (int j = 0; j < attribute.length; j++) {
                attribute[j] = ts[j].get(i);
            }
            product[i] = AstUtil.getLowestCommonSupertype(attribute);
            if (product[i] == null) {
                return null;
            }
        }
        return new ProductType(product);
    }

    @Override
    public Iterator<AstClafer> iterator() {
        return new ArrayIterator<>(types);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ProductType) {
            ProductType other = (ProductType) obj;
            return Arrays.equals(types, other.types);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(types);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append(types[0]);
        for (int i = 1; i < types.length; i++) {
            builder.append(" * ").append(types[i]);
        }
        return builder.toString();
    }
}
