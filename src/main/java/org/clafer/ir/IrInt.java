package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrInt {

    /**
     * Domain cannot be empty.
     * 
     * @return the domain of values this expression can take
     */
    public IrDomain getDomain();
}
