package org.clafer.ast;

/**
 * The primitive decimal Clafer: numbers with a fixed number of fractional
 * digits, the decimal scale of the model. A decimal is solved as the integer
 * {@code value * 10^scale}.
 *
 * @author Michał Antkiewicz
 */
public class AstDecimalClafer extends AstPrimClafer {

    public static final AstDecimalClafer Singleton = new AstDecimalClafer();

    private AstDecimalClafer() {
        super("decimal");
    }
}
