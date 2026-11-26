package org.clafer.frontend;

import java.util.List;
import java.util.Map;
import org.clafer.frontend.Symbols.ClaferSymbol;

/**
 * What the names of a model refer to, for the language server: the clafers,
 * whose {@link ClaferSymbol#nameSpan()} is where each is declared, and the
 * clafer that each written name refers to, by the span of the name.
 * <p>
 * The names are those of the constraints, assertions, objectives, initial
 * values and reference targets, as the resolver resolves them, and those of
 * the supers, as the symbols resolve them. In a path {@code a.b} or
 * {@code A\b}, each name has its own span. The special names ({@code this},
 * {@code parent}, {@code dref}, ...), the primitive types and the locals of
 * quantifiers and {@code let} are not in it. The spans are in the lines of
 * the model, the files of its imports first ({@link ModelFiles#position}).
 *
 * @param clafers the clafers of the model, enumerations and their literals
 * included, in declaration order
 * @param uses the clafer each written name refers to, by the name's span
 */
public record Names(List<ClaferSymbol> clafers, Map<SourceSpan, ClaferSymbol> uses) {

    /**
     * The names of a model that has errors before the resolver.
     */
    public static final Names None = new Names(List.of(), Map.of());

    public Names {
        clafers = List.copyOf(clafers);
        uses = java.util.Collections.unmodifiableMap(new java.util.LinkedHashMap<>(uses));
    }

    /**
     * The names of a model: the uses that the resolver found, and the supers.
     */
    static Names of(Symbols.Result symbols, Map<SourceSpan, ClaferSymbol> resolved) {
        Map<SourceSpan, ClaferSymbol> uses = new java.util.LinkedHashMap<>(resolved);
        for (ClaferSymbol c : symbols.all()) {
            if (c.superClafer().isPresent() && c.superExp().isPresent()) {
                lastName(c.superExp().get()).ifPresent(span -> uses.put(span, c.superClafer().get()));
            }
        }
        return new Names(symbols.all(), uses);
    }

    /**
     * The span of the last name of a super, {@code B} in {@code : B} or in
     * {@code : A.B}.
     */
    private static java.util.Optional<SourceSpan> lastName(Syntax.Exp e) {
        return switch (e) {
            case Syntax.ClaferId id -> java.util.Optional.of(id.name().path().get(id.name().path().size() - 1).span());
            case Syntax.Binary b when b.op() == Syntax.BinaryOp.Join -> lastName(b.right());
            default -> java.util.Optional.empty();
        };
    }
}
