/**
 * This package provides the Java frontend of Clafer: a port of the Clafer
 * compiler that reads {@code .cfr} models without the Haskell compiler.
 * <p>
 * It parses the full Clafer language ({@link org.clafer.frontend.ClaferParser},
 * with its layout), builds the symbols, checks the model, resolves its names
 * and types, and gives the explicit IR ({@link org.clafer.frontend.Ir}) and the
 * scopes, as {@code clafer -m clafer} gives them; {@link org.clafer.frontend.ToCore}
 * lowers the rich explicit model to the core. It also reads and prints the
 * explicit languages, follows imports (local files and URLs), and provides
 * {@code claferc} ({@link org.clafer.frontend.ClaferCommand}), the compiler's
 * {@code -m clafer}, {@code --core}, {@code --meta-data} and {@code -m alloy}
 * in Java.
 * </p>
 */
package org.clafer.frontend;
