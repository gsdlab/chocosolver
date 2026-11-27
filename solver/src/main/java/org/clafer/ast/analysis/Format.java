package org.clafer.ast.analysis;

/**
 * <p>
 * There is two ways to represent a set.
 * </p>
 * <p>
 * Consider the model:
 * <pre>
 * parent *
 *    child 3
 * </pre>
 * </p>
 * <p>
 * Suppose parent = {0,2,3}.
 * </p>
 * <p>
 * If child is in LowGroup, then it must choose the lowest 9 elements in its
 * set. So child = {0,1,2,3,4,5,6,7,8}. This is good for many optimizations like
 * skipCard and works for any case but it makes constant operations navigating
 * down the tree more difficult because we don't know where the children lie
 * during compile time. Abstract Clafers cannot have LowGroup.
 * </p>
 * <p>
 * If child is in ParentGroup, then each childSet is grouped together but there
 * may be gaps between the childSets. A Clafer has ParentGroup ONLY IF it has
 * exact cardinality, or it is Abstract, otherwise it isn't worthwhile. One
 * reason why is that ParentGroup does not work with our symmetry breaking
 * rules, but exact cardinalities do not need to be broken. Following the
 * example, child = {0,1,2,6,7,8,9,10,11}.
 * </p>
 */
public enum Format {

    LowGroup,
    ParentGroup;
}
