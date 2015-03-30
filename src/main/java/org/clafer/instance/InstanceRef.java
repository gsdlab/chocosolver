package org.clafer.instance;

import org.clafer.ast.AstClafer;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class InstanceRef {

    private final AstClafer type;
    private final Object value;

    public InstanceRef(AstClafer type, Object value) {
        this.type = Check.notNull(type);
        this.value = value;
    }

    public AstClafer getType() {
        return type;
    }

    public Object getValue() {
        return value;
    }

    @Override
    public String toString() {
        return type.isPrimitive()
                ? value.toString()
                : type.getName() + "$" + value;
    }
}
