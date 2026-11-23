/**
 * This package provides the IR used as in intermediate language during
 * compilation.
 * <center> AST -> <b>IR</b> -> Choco</center>
 * <p>
 * It resembles the Choco API except for two facts.
 * </p>
 * <p>
 * <ol>
 * <li> Boolean expressions in the IR unify Choco Constraints, BoolVars, and
 * LogOps. The IR will try to optimally compile to whichever Choco construct is
 * most efficient.
 * </li>
 * <li> Expressions can be nested. In Choco, expressions cannot be nested. This
 * simplifies compiling the AST since expressions are Clafer are nested. Having
 * expressions nested makes optimization easier, before it is flattened into
 * Choco.
 * </li>
 * </ol>
 * </p>
 * <p>
 * The IR is designed to make optimization easy at the expense of being harder
 * to use. The IR is internal, and most use cases should not use this package.
 * Use the {@link org.clafer.compiler}</code> package to go straight from AST to
 * IR.
 * </p>
 * <p>
 * The IR has 3 types: booleans, integers, and sets of integers. Each variable
 * and expression typed.
 * </p>
 *
 * @author jimmy
 */
package org.clafer.ir;