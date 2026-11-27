/**
 * This package provides the language for building Clafer AST.
 * <center><b>AST</b> -> IR -> Choco</center>
 * <p>
 * The AST is the input of the API for solving models. The AST is compiled to IR
 * before finally compiled to Choco.
 * </p>
 * <p>
 * The AST represents the desugared form of Clafer, where all implicit
 * assumptions like cardinality and joining on references must be made explicit.
 * </p>
 *
 * @author jimmy
 */
package org.clafer.ast;
