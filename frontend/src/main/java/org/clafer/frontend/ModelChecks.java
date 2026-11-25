package org.clafer.frontend;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import org.clafer.frontend.Symbols.ClaferSymbol;
import org.clafer.frontend.Symbols.Interval;

/**
 * The compiler's checks of a model before its names are resolved
 * ({@code compile} and {@code analyze} in {@code Language/Clafer.hs}), on
 * the clafers of the desugared model: every clafer, enumeration and literal
 * of an enumeration, and the clafer of each clafer use ({@code `A}), in
 * pre-order. A check is an error that ends the compilation, so the next check
 * runs only on a model without errors; the checks run in this order:
 * <ol>
 * <li>no clafer is named after a keyword ({@link #keywordNames});</li>
 * <li>an explicit model writes the clock of each {@code :=}
 * ({@link #clocklessInitializers});</li>
 * <li>a top-level abstract clafer with {@code :=} and no clock has the
 * multiplicity {@code *} ({@link #initializerBounds});</li>
 * <li>with {@code --check-duplicates}, the names are unique
 * ({@link #duplicateNames});</li>
 * <li>with {@code -f}, which {@code --explicit} implies, the names are UIDs
 * ({@link #nonUidName}).</li>
 * </ol>
 * After the names are resolved, the per-instance initializers of the abstract
 * clafers have values with at most one element
 * ({@link Resolver#initialValueErrors}), a model that declares itself the
 * core has no rich construct ({@link Core#richInCore}), and with
 * {@code --check-afm}, no concrete clafer has more than one instance
 * ({@link #notAttributedFeatureModel}).
 */
final class ModelChecks {

    private ModelChecks() {
    }

    /**
     * The words that no clafer can be named ({@code Common.keywordIdents}).
     */
    static final Set<String> KeywordIdents = Set.of(
            "clafer",
            "this", "parent", "dref", "root", "children", "noclafer",
            "string", "int", "integer", "decimal", "double", "real",
            "max", "min", "maximize", "minimize", "sum", "product",
            "xor", "in",
            "if", "then", "else",
            "no", "not", "some", "one", "all", "disj",
            "opt", "mux", "or", "lone",
            "abstract", "enum",
            "final", "initial",
            "each", "together",
            "W", "U", "X", "G", "F", "initially", "finally", "previous", "once", "historically", "since",
            "triggered",
            "never", "sometime", "lonce", "always", "must", "precede", "follow",
            "until", "weakuntil", "eventually", "globally", "next", "before", "after", "between", "and",
            "let");

    /**
     * A clafer of the desugared model: where its declaration is, and its
     * name.
     */
    record Named(SourceSpan span, String name, Syntax.Node declaration) {
    }

    /**
     * @return the clafers of the desugared model, in pre-order
     */
    static List<Named> clafers(Syntax.Model model) {
        List<Named> out = new ArrayList<>();
        for (Syntax.Declaration d : model.declarations()) {
            switch (d) {
                case Syntax.EnumDecl e -> {
                    out.add(new Named(e.span(), e.name().text(), e));
                    for (Syntax.Ident literal : e.literals()) {
                        out.add(new Named(literal.span(), literal.text(), literal));
                    }
                }
                case Syntax.Element e -> clafers(e, out);
            }
        }
        return out;
    }

    private static void clafers(Syntax.Element e, List<Named> out) {
        Optional<Syntax.Elements> elements = switch (e) {
            case Syntax.Clafer c -> {
                out.add(new Named(c.span(), c.name().text(), c));
                yield c.elements();
            }
            case Syntax.ClaferUse u -> {
                List<Syntax.Ident> path = u.name().path();
                out.add(new Named(u.span(), path.get(path.size() - 1).text(), u));
                yield u.elements();
            }
            default -> Optional.empty();
        };
        elements.ifPresent(es -> es.elements().forEach(x -> clafers(x, out)));
    }

    private static String places(List<SourceSpan> spans) {
        StringBuilder sb = new StringBuilder();
        for (SourceSpan s : spans) {
            sb.append("Line ").append(s.line()).append(" column ").append(s.column()).append('\n');
        }
        return sb.toString();
    }

    /**
     * @return the error of the clafers named after keywords, e.g.,
     * {@code integer}, which has no position: the positions are in the
     * message
     */
    static Optional<String> keywordNames(Syntax.Model model) {
        List<SourceSpan> spans = clafers(model).stream().filter(c -> KeywordIdents.contains(c.name()))
                .map(Named::span).toList();
        return spans.isEmpty() ? Optional.empty()
                : Optional.of("The model contains clafers with keywords as names in the following places:\n"
                        + places(spans));
    }

    /**
     * @return the clafers of the model, also the nested ones, in pre-order
     */
    private static List<Syntax.Clafer> declaredClafers(Syntax.Model model) {
        List<Syntax.Clafer> out = new ArrayList<>();
        for (Named n : clafers(model)) {
            if (n.declaration() instanceof Syntax.Clafer c) {
                out.add(c);
            }
        }
        return out;
    }

    /**
     * The errors of the initializers {@code :=} without a written clock in
     * an explicit model, at the {@code :=}
     * ({@code InitializerChecks.clocklessInitializers}).
     */
    static List<Diagnostic> clocklessInitializers(Syntax.Model model) {
        List<Diagnostic> out = new ArrayList<>();
        for (Syntax.Clafer c : declaredClafers(model)) {
            if (c.init().isPresent() && c.init().get().how() == Syntax.InitHow.Default) {
                String name = c.name().text();
                out.add(new Diagnostic(c.init().get().span(), "The initializer ':=' of " + name
                        + " has no clock, which an explicit model writes: ':= each' initializes each instance of "
                        + name + " at its birth, and ':= together' all the instances of " + name
                        + " under a parent together."));
            }
        }
        return out;
    }

    /**
     * The errors of the top-level abstract clafers with {@code :=} without a
     * written clock and a written multiplicity other than {@code *} or
     * {@code 0..*}, at the multiplicity
     * ({@code InitializerChecks.unsupportedInitializerBounds}).
     */
    static List<Diagnostic> initializerBounds(Syntax.Model model) {
        List<Diagnostic> out = new ArrayList<>();
        for (Syntax.Declaration d : model.declarations()) {
            if (d instanceof Syntax.Clafer c && c.isAbstract() && c.card().isPresent() && c.init().isPresent()
                    && c.init().get().how() == Syntax.InitHow.Default && !isAny(c.card().get())) {
                String name = c.name().text();
                out.add(new Diagnostic(c.card().get().span(), "The initializer ':=' of the top-level abstract clafer "
                        + name + " with the multiplicity " + written(c.card().get())
                        + " requires an explicit clock: ':= each' initializes each instance of " + name
                        + " at its birth, and ':= together' all the instances of " + name
                        + " together at time zero. Without a clock, ':=' initializes each instance without a"
                        + " multiplicity, and all the instances together with * (or 0..*)."));
            }
        }
        return out;
    }

    /**
     * Whether a multiplicity is {@code *}, also as {@code 0..*}.
     */
    private static boolean isAny(Syntax.Card card) {
        return switch (card) {
            case Syntax.CardSymbol s -> s.symbol().equals("*");
            case Syntax.NCard n -> n.high().isEmpty() && Long.parseLong(n.low()) == 0;
            case Syntax.CardNum n -> false;
        };
    }

    /**
     * A multiplicity as written, without spaces, e.g., {@code 2..3}.
     */
    private static String written(Syntax.Card card) {
        return switch (card) {
            case Syntax.CardSymbol s -> s.symbol();
            case Syntax.CardNum n -> n.number();
            case Syntax.NCard n -> n.low() + ".." + n.high().orElse("*");
        };
    }

    /**
     * @return the error of {@code --check-duplicates}, the names of more than
     * one clafer, sorted, which has no position
     * ({@code Optimizer.findDupModule})
     */
    static Optional<String> duplicateNames(Syntax.Model model) {
        Map<String, Integer> counts = new TreeMap<>();
        for (Named n : clafers(model)) {
            counts.merge(n.name(), 1, Integer::sum);
        }
        List<String> dups = counts.entrySet().stream().filter(e -> e.getValue() > 1).map(Map.Entry::getKey).toList();
        return dups.isEmpty() ? Optional.empty()
                : Optional.of("--check-duplicates: Duplicate clafer names: " + String.join(", ", dups));
    }

    /**
     * @return the error of the first clafer whose name cannot be its UID,
     * with {@code -f}: a name of more than one clafer, or with an apostrophe
     * ({@code Optimizer.nonUidNames})
     */
    static Optional<Diagnostic> nonUidName(Syntax.Model model) {
        List<Named> clafers = clafers(model);
        Map<String, Integer> counts = new HashMap<>();
        for (Named n : clafers) {
            counts.merge(n.name(), 1, Integer::sum);
        }
        for (Named n : clafers) {
            if (n.name().contains("'") || counts.get(n.name()) > 1) {
                return Optional.of(new Diagnostic(n.span(), "With -f (--skip-resolver), the names of the clafers are"
                        + " their UIDs, so they must be unique and without apostrophes, but " + n.name() + " is not"));
            }
        }
        return Optional.empty();
    }

    /**
     * @return the error of {@code --check-afm}, the concrete clafers that can
     * have more than one instance, which has no position
     */
    static Optional<String> notAttributedFeatureModel(Syntax.Model model, Symbols.Result symbols) {
        List<SourceSpan> spans = new ArrayList<>();
        for (Named n : clafers(model)) {
            Optional<ClaferSymbol> s = symbols.of(n.declaration());
            if (s.isPresent() && !s.get().isAbstract()) {
                int high = s.get().card().high();
                if (high > 1 || high == Interval.Unbounded) {
                    spans.add(n.span());
                }
            }
        }
        return spans.isEmpty() ? Optional.empty()
                : Optional.of("The model is not an attributed feature model.\n"
                        + "The following places contain cardinality larger than 1:\n" + places(spans));
    }
}
