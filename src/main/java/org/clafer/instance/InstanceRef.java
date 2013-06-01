package org.clafer.instance;

import org.clafer.ast.AstClafer;
import org.clafer.ast.AstIntClafer;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class InstanceRef {

    private final AstClafer type;
    private final int value;

    public InstanceRef(AstClafer type, int value) {
        this.type = Check.notNull(type);
        this.value = value;
    }

    public AstClafer getType() {
        return type;
    }

    public int getValue() {
        return value;
    }

    @Override
    public String toString() {
        return type instanceof AstIntClafer
                ? Integer.toString(value)
                : type.getName() + "#" + Integer.toString(value);
    }
}
