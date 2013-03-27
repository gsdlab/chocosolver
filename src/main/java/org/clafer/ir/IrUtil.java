package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public class IrUtil {

    public static <T> T notNull(String message, T t) {
        if (t == null) {
            throw new IrException(message);
        }
        return t;
    }

    public static boolean isTrue(IrBoolExpr bool) {
        if (bool instanceof IrBoolVar) {
            IrBoolVar var = (IrBoolVar) bool;
            return var.isTrue();
        }
        return false;
    }

    public static boolean isFalse(IrBoolExpr bool) {
        if (bool instanceof IrBoolVar) {
            IrBoolVar var = (IrBoolVar) bool;
            return var.isFalse();
        }
        return false;
    }
}
