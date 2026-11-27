/**
 * This package provides the command-line interface of clafersg, the Clafer
 * solution generator (formerly chocosolver): the main class of
 * {@code clafersg.jar}.
 * <p>
 * It reads a model ({@code .cfr}, explicit or not, or a {@code .js} file),
 * compiling a {@code .cfr} model with the Java frontend or, for a
 * {@code [choco| |]} escape or with {@code --haskell-frontend}, with the
 * Clafer compiler. A static model is solved with Choco, in batch mode, with
 * validation ({@code -v}) or in the REPL ({@code --repl}); a temporal model is
 * solved with Alloy 6, as claferIG solves it.
 * </p>
 */
package org.clafer.cli;
