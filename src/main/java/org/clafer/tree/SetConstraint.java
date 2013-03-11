package org.clafer.tree;

/**
 *
 * @author jimmy
 */
public interface SetConstraint {

    public BoolExpr apply(SetExpr arg);
}
