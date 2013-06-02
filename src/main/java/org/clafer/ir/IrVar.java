package org.clafer.ir;

/**
 * A variable. Two variables of the same name are still two different variables.
 * Variables have two flavours, dynamic variables (named Ir&lt;Type&gt;Var) or
 * fixed variables (named Ir&lt;Type&gt;Var).
 *
 * @author jimmy
 */
public interface IrVar {

    /**
     * @return the name of this variable
     */
    public String getName();

    /**
     * @return a new decision variable with the same name.
     */
    public IrVar asDecision();

    /**
     * @return a new no-decision variable with the same name.
     */
    public IrVar asNoDecision();

    /**
     * @return {@code true} if and only if this variable is not a decision
     * variable, {@code false} otherwise
     */
    public boolean isDecision();
}
