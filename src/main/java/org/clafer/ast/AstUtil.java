package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstUtil {

    public static <T> T notNull(String message, T t) {
        if (t == null) {
            throw new AstException(message);
        }
        return t;
    }

    public static AstRef getInheritedRef(AstClafer clafer) {
        if (clafer.hasRef()) {
            return clafer.getRef();
        }
        if (clafer.hasSuperClafer()) {
            return getInheritedRef(clafer.getSuperClafer());
        }
        return null;
    }
}
