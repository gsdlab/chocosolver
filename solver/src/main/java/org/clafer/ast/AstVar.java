package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public interface AstVar {

    /**
     * Returns the name of the variable. Two variables with the same name are still different
     * variables. The name is simply for readability.
     * 
     * @return the name of the variable
     */
    String getName();
}
