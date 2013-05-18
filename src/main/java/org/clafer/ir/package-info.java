/**
 * This package provides the the internal language (IR) used during compilation.
 * AST -> <b>IR</b> -> Choco.
 * <p>
 * It resembles the Choco API except for two facts.
 * </p>
 * <p>
 * <ol>
 * <li> Constraints cannot be nested. In Choco, constraints can be nested but requires
 *      reifying the constraints which is inefficient. I have not found a use for
 *      nesting constraints for compiling Clafer, hence the IR API forbids the case
 *      for efficiency. The IR compiler will reify some constraints when deemed to be
 *      necessary.
 * </li>
 * <li> Expressions can be nested. In Choco, expressions cannot be nested. This
 *      simplifies compiling the AST since expressions are Clafer are nested. Having
 *      expressions nested makes optimization easier, before it is flattened into
 *      Choco.
 * </li>
 * </ol>
 * </p>
 * 
 * The IR is designed to make optimization easy at the expense of being harder to use.
 * The IR is internal, and most use cases should not use this package. Use the
 * {@link org.clafer.compiler}</code> package to go straight from AST to IR.
 * 
 * The IR has 3 types: booleans, integers, and sets of integers. Variables and expressions
 * have a type.
 * 
 * @author jimmy
 */
package org.clafer.ir;