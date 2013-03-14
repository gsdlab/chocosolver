package org.clafer.tree;

/**
 *
 * @author jimmy
 */
public interface ThisFactory {

    public IntExpr newIntThis(int id);

    public SetExpr newSetThis(int id);
}
