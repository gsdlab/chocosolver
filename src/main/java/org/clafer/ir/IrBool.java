package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrBool {

    /**
     * @return the domain of values this expression can take
     */
    public IrBoolDomain getDomain();
}
