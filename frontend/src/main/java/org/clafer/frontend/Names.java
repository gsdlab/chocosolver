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
 * the supers and the references to a clafer by name, as the symbols resolve
 * them. In a path {@code a.b} or
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
     * @param name a word
     * @return whether a clafer or a local can be named so: it lexes as one
     * name, and it is neither a reserved word nor one of the words that no
     * clafer can be named
     */
    public static boolean isClaferName(String name) {
        ClaferLexer.Result lexed = ClaferLexer.lex(name);
        return lexed.diagnostics().isEmpty() && lexed.others().isEmpty() && lexed.tokens().size() == 2
                && lexed.tokens().get(0).kind() == TokenKind.WORD && lexed.tokens().get(0).text().equals(name)
                && !ModelChecks.KeywordIdents.contains(name);
    }

    /**
     * The names of a model: the uses that the resolver found, and the supers.
     */
    static Names of(Symbols.Result symbols, Map<SourceSpan, ClaferSymbol> resolved) {
        Map<SourceSpan, ClaferSymbol> uses = new java.util.LinkedHashMap<>(resolved);
        for (ClaferSymbol c : symbols.all()) {
            if (c.superClafer().isPresent() && c.superExp().isPresent()) {
                superPath(symbols, c.superExp().get(), uses);
                lastName(c.superExp().get()).ifPresent(span -> uses.put(span, c.superClafer().get()));
            }
            // a reference to a clafer by its name, which the symbols resolve
            c.reference().ifPresent(r -> {
                List<Syntax.Ident> path = new java.util.ArrayList<>();
                if (r.clafer().isPresent() && flatten(r.target(), path)
                        && path.get(path.size() - 1).text().equals(r.clafer().get().name())) {
                    superPath(symbols, r.target(), uses);
                    uses.putIfAbsent(path.get(path.size() - 1).span(), r.clafer().get());
                }
            });
        }
        return new Names(symbols.all(), uses);
    }

    /**
     * The names before the last of a path of a super or a reference,
     * {@code A} in {@code : A.B}: a top-level clafer, then a child of the clafer before,
     * its own or an inherited one.
     */
    private static void superPath(Symbols.Result symbols, Syntax.Exp e, Map<SourceSpan, ClaferSymbol> uses) {
        List<Syntax.Ident> path = new java.util.ArrayList<>();
        if (!flatten(e, path) || path.size() < 2) {
            return;
        }
        List<ClaferSymbol> at = symbols.topLevel().stream().filter(c -> c.name().equals(path.get(0).text())).toList();
        for (int i = 0; i < path.size() - 1 && at.size() == 1; i++) {
            ClaferSymbol c = at.get(0);
            uses.putIfAbsent(path.get(i).span(), c);
            String next = path.get(i + 1).text();
            List<ClaferSymbol> children = new java.util.ArrayList<>();
            java.util.Set<ClaferSymbol> seen = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>());
            for (ClaferSymbol h = c; h != null && seen.add(h); h = h.superClafer().orElse(null)) {
                h.children().stream().filter(x -> x.name().equals(next)).forEach(children::add);
            }
            at = children;
        }
    }

    /**
     * The names of a path {@code A.B.C}, or false if it is not one.
     */
    private static boolean flatten(Syntax.Exp e, List<Syntax.Ident> out) {
        return switch (e) {
            case Syntax.ClaferId id -> {
                out.add(id.name().path().get(id.name().path().size() - 1));
                yield true;
            }
            case Syntax.Binary b when b.op() == Syntax.BinaryOp.Join -> flatten(b.left(), out) && flatten(b.right(), out);
            default -> false;
        };
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
