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

    public static boolean isTrue(IrBoolExpr boolExpr) {
        if (boolExpr instanceof IrBoolVar) {
            IrBoolVar var = (IrBoolVar) boolExpr;
            return var.isTrue();
        }
        return false;
    }

    public static boolean isFalse(IrBoolExpr boolExpr) {
        if (boolExpr instanceof IrBoolVar) {
            IrBoolVar var = (IrBoolVar) boolExpr;
            return var.isFalse();
        }
        return false;
    }

    public static boolean isTrue(IrConstraint constraint) {
        if (constraint instanceof IrBoolConstraint) {
            IrBoolConstraint boolConstraint = (IrBoolConstraint) constraint;
            return isTrue(boolConstraint.getExpr());
        }
        return false;
    }

    public static boolean isFalse(IrConstraint constraint) {
        if (constraint instanceof IrBoolConstraint) {
            IrBoolConstraint boolConstraint = (IrBoolConstraint) constraint;
            return isFalse(boolConstraint.getExpr());
        }
        return false;
    }

    public static Integer getConstant(IrIntExpr intExpr) {
        IrDomain domain = intExpr.getDomain();
        if(domain.size() == 1) {
            return domain.getLowerBound();
        }
        return null;
    }

    public static int[] getConstant(IrSetExpr setExpr) {
        if (setExpr instanceof IrSetVar) {
            IrSetVar var = (IrSetVar) setExpr;
            return var.isConstant() ? var.getValue() : null;
        }
        return null;
    }
}
