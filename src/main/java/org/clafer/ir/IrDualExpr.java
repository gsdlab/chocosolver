package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrDualExpr extends IrBoolExpr {

    public IrBoolExpr opposite();
}
