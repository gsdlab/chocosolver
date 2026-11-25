package org.clafer.frontend;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.clafer.frontend.Symbols.ClaferSymbol;
import org.clafer.frontend.Symbols.Interval;
import org.clafer.frontend.Symbols.RefModifier;
import org.clafer.frontend.Syntax.Binary;
import org.clafer.frontend.Syntax.ClaferId;
import org.clafer.frontend.Syntax.Exp;
import org.clafer.frontend.Syntax.Ident;
import org.clafer.frontend.Syntax.Literal;

/**
 * Step 9b: the explicit IR of a model in the full syntax whose constraints are
 * already explicit. The clafers are the {@link Symbols}: their UIDs, supers,
 * references, cards, group cards, modifiers and initializers, with the
 * layout, the braces and the defaults gone. The constraints, objectives,
 * assertions and initial values are copied as written, since resolving names
 * and inserting {@code this}, {@code parent} and {@code dref} is step 10:
 * <ul>
 * <li>{@code this}, {@code noclafer}, and {@code .dref} and {@code .parent}
 * after a navigation become their nodes;</li>
 * <li>every other name is a quantifier's local, the UID of a clafer, or the
 * name of exactly one clafer, which becomes its UID; at the start of a
 * navigation, it is a top-level clafer or an enumeration's literal, since
 * the compiler resolves a nested one in its context;</li>
 * <li>{@code sum} and {@code product} take {@code x.dref}, and a clafer
 * with a primitive reference is compared or computed with through its
 * {@code dref}.</li>
 * </ul>
 * Anything else, such as a bare {@code parent} or {@code let}, is a
 * diagnostic that it needs step 10. As in the compiler, the constraints of a
 * reference to a set expression come first in its clafer, a nested assertion
 * is quantified over the instances of its clafer ({@code all this_ : A.b |}),
 * and the objectives and assertions are collected in pre-order.
 * <p>
 * Every rich explicit model is such a model, so it gives its own IR back.
 */
public final class ToIr {

    private ToIr() {
    }

    /**
     * @param model the explicit IR, or an empty model if the model has
     * errors before this pass
     * @param diagnostics the errors of the parser, the symbols and this pass
     */
    public record Result(Ir.Model model, List<Diagnostic> diagnostics) {
    }

    private static final Pattern OptionsLine = Pattern.compile("(?m)^//#\\s*OPTIONS\\b(.*)$");
    private static final Pattern TraceLength = Pattern.compile("--tl[= ](\\d+)");
    private static final Pattern DecimalDigits = Pattern.compile("--decimal-digits[= ](\\d+)");

    /**
     * Parses a model, finds its symbols, and builds its explicit IR.
     *
     * @param source the text of a model
     * @return its IR and the errors
     */
    public static Result toIr(String source) {
        ClaferParser.Result parsed = ClaferParser.parse(source);
        if (!parsed.diagnostics().isEmpty()) {
            return new Result(empty(), parsed.diagnostics());
        }
        Symbols.Result symbols = Symbols.analyze(parsed.model());
        if (!symbols.diagnostics().isEmpty()) {
            return new Result(empty(), symbols.diagnostics());
        }
        Matcher digits = DecimalDigits.matcher(optionsLine(source));
        return toIr(parsed.model(), symbols, options(source),
                digits.find() ? Optional.of(Integer.parseInt(digits.group(1))) : Optional.empty());
    }

    private static String optionsLine(String source) {
        Matcher m = OptionsLine.matcher(source);
        return m.find() ? m.group(1) : "";
    }

    private static Ir.Model empty() {
        SourceSpan span = new SourceSpan(1, 1, 1, 1);
        return new Ir.Model(span, Ir.Options.Rich, List.of(), List.of(), List.of());
    }

    /**
     * The options of the first {@code //# OPTIONS} line that the explicit
     * form keeps: {@code --core} and the trace length.
     */
    static Ir.Options options(String source) {
        Matcher m = OptionsLine.matcher(source);
        if (!m.find()) {
            return Ir.Options.Rich;
        }
        String line = m.group(1);
        boolean core = Pattern.compile("(^|\\s)--core(\\s|$)").matcher(line).find();
        Matcher tl = TraceLength.matcher(line);
        return new Ir.Options(core, tl.find() ? OptionalInt.of(Integer.parseInt(tl.group(1))) : OptionalInt.empty());
    }

    /**
     * @param model a parsed model without errors
     * @param symbols its symbols, without errors
     * @param options the options of the explicit model
     * @return its IR and the errors of this pass
     */
    public static Result toIr(Syntax.Model model, Symbols.Result symbols, Ir.Options options) {
        return toIr(model, symbols, options, Optional.empty());
    }

    /**
     * @param decimalDigits the decimal scale of {@code --decimal-digits},
     * if given
     */
    public static Result toIr(Syntax.Model model, Symbols.Result symbols, Ir.Options options,
            Optional<Integer> decimalDigits) {
        Resolver resolver = new Resolver(symbols, model, decimalDigits);
        // a temporal model has a trace length, 10 unless given, as in the compiler
        if (resolver.isDynamic() && options.traceLength().isEmpty()) {
            options = new Ir.Options(options.core(), OptionalInt.of(10));
        }
        Builder builder = new Builder(symbols, resolver);
        List<Ir.Element> elements = new ArrayList<>();
        for (Syntax.Declaration d : model.declarations()) {
            switch (d) {
                case Syntax.EnumDecl e -> {
                    ClaferSymbol en = symbols.of(e).orElseThrow();
                    List<Ir.Name> literals = new ArrayList<>();
                    for (Ident literal : e.literals()) {
                        literals.add(new Ir.Name(literal.span(), symbols.of(literal).orElseThrow().uid()));
                    }
                    elements.add(new Ir.EnumDecl(e.span(), new Ir.Name(e.name().span(), en.uid()), literals));
                }
                case Syntax.Element e -> builder.element(e, null, elements);
            }
        }
        List<Diagnostic> diagnostics = new ArrayList<>(builder.diagnostics);
        diagnostics.addAll(builder.resolver.diagnostics);
        diagnostics.sort((a, b) -> a.span().line() != b.span().line()
                ? Integer.compare(a.span().line(), b.span().line())
                : Integer.compare(a.span().column(), b.span().column()));
        return new Result(new Ir.Model(model.span(), options, elements, builder.objectives, builder.assertions),
                List.copyOf(diagnostics));
    }

    private static final class Builder {

        private final Symbols.Result symbols;
        final Resolver resolver;
        final List<Diagnostic> diagnostics = new ArrayList<>();
        final List<Ir.Objective> objectives = new ArrayList<>();
        final List<Ir.Assertion> assertions = new ArrayList<>();

        Builder(Symbols.Result symbols, Resolver resolver) {
            this.symbols = symbols;
            this.resolver = resolver;
        }

        /**
         * An element of a clafer, or of the model if the clafer is null.
         */
        void element(Syntax.Element e, ClaferSymbol parent, List<Ir.Element> out) {
            switch (e) {
                case Syntax.Clafer c -> clafer(c, parent, out);
                case Syntax.ClaferUse u -> clafer(u, parent, out);
                case Syntax.Constraint c -> {
                    SourceSpan span = c.exps().get(0).span();
                    resolver.constraint(c.exps(), parent, false)
                            .ifPresent(x -> out.add(new Ir.Constraint(span, x, Optional.empty())));
                }
                case Syntax.Assertion a -> {
                    SourceSpan span = a.exps().get(0).span();
                    resolver.constraint(a.exps(), parent, true).ifPresent(x -> assertions.add(new Ir.Assertion(span,
                            parent == null ? x : quantified(parent, x, span), Optional.empty())));
                }
                case Syntax.Goal g -> {
                    boolean maximize = g.kind() == Syntax.GoalKind.Maximize || g.kind() == Syntax.GoalKind.MaxDeprecated;
                    resolver.goal(g, parent).ifPresent(x -> objectives.add(new Ir.Objective(g.span(), maximize, x)));
                }
            }
        }

        /**
         * A nested assertion holds for each instance of its clafer:
         * {@code all this_ : A.b | ...}, with {@code this} as {@code this_}.
         */
        private Ir.Exp quantified(ClaferSymbol clafer, Ir.Exp body, SourceSpan span) {
            Ir.Name local = new Ir.Name(span, "this_");
            Ir.Decl decl = new Ir.Decl(span, false, List.of(local), instancesOf(clafer, span));
            return new Ir.Quantified(span, Ir.Quantifier.ALL, List.of(decl), thisToLocal(body));
        }

        private static Ir.Exp instancesOf(ClaferSymbol c, SourceSpan span) {
            Ir.NameExp name = new Ir.NameExp(span, c.uid());
            return c.parent().isEmpty() ? name : new Ir.Join(span, instancesOf(c.parent().get(), span), name);
        }

        /**
         * Replaces {@code this} with {@code this_}, in the expressions that an
         * explicit model's constraint can hold.
         */
        private static Ir.Exp thisToLocal(Ir.Exp e) {
            return switch (e) {
                case Ir.This t -> new Ir.NameExp(t.span(), "this_");
                case Ir.Join j -> new Ir.Join(j.span(), thisToLocal(j.left()), thisToLocal(j.right()));
                case Ir.Dref d -> new Ir.Dref(d.span(), thisToLocal(d.left()));
                case Ir.ParentOf p -> new Ir.ParentOf(p.span(), thisToLocal(p.left()));
                case Ir.Unary u -> new Ir.Unary(u.span(), u.op(), thisToLocal(u.arg()));
                case Ir.Binary b -> new Ir.Binary(b.span(), b.op(), b.operands().stream().map(Builder::thisToLocal).toList());
                case Ir.IfThenElse i -> new Ir.IfThenElse(i.span(), thisToLocal(i.condition()), thisToLocal(i.then()),
                        thisToLocal(i.otherwise()));
                case Ir.Quantified q -> new Ir.Quantified(q.span(), q.quantifier(), q.decls().stream()
                        .map(d -> new Ir.Decl(d.span(), d.disjoint(), d.locals(), thisToLocal(d.set()))).toList(),
                        thisToLocal(q.body()));
                case Ir.QuantifiedSet q -> new Ir.QuantifiedSet(q.span(), q.quantifier(), thisToLocal(q.set()));
                case Ir.Temporal t -> new Ir.Temporal(t.span(), t.op(), t.args().stream().map(Builder::thisToLocal).toList());
                case Ir.Anchor a -> new Ir.Anchor(a.span(), a.initially(), thisToLocal(a.body()));
                case Ir.Pattern p -> new Ir.Pattern(p.span(), p.kind(), p.scope(),
                        p.args().stream().map(Builder::thisToLocal).toList());
                case Ir.Transition t -> new Ir.Transition(t.span(), t.sync(), thisToLocal(t.from()),
                        t.guard().map(Builder::thisToLocal), thisToLocal(t.to()));
                default -> e;
            };
        }

        private void clafer(Syntax.Element declaration, ClaferSymbol parent, List<Ir.Element> out) {
            ClaferSymbol c = symbols.of(declaration).orElseThrow();
            // a clafer relocated under another (gsdlab/clafer#67) is built there
            if (c.parent().orElse(null) != parent) {
                return;
            }
            out.add(clafer(c));
        }

        private Ir.ClaferDecl clafer(ClaferSymbol c) {
            SourceSpan span = c.declaration().span();
            List<Ir.Modifier> modifiers = new ArrayList<>();
            if (c.isInitial()) {
                modifiers.add(new Ir.Modifier(span, Ir.ModifierKind.INITIAL));
            }
            if (c.isFinal()) {
                modifiers.add(new Ir.Modifier(span, Ir.ModifierKind.FINAL));
            } else {
                for (RefModifier m : c.refModifiers()) {
                    modifiers.add(new Ir.Modifier(span, switch (m) {
                        case FinalRef -> Ir.ModifierKind.FINALREF;
                        case FinalTarget -> Ir.ModifierKind.FINALTARGET;
                        case FinalRefTarget -> Ir.ModifierKind.FINAL;
                    }));
                }
            }
            Ir.Card groupCard = card(span, Symbols.printedGroupCard(c));
            Optional<Ir.Name> superName = c.superClafer().map(s -> new Ir.Name(span, s.uid()));
            Optional<Symbols.Reference> reference = c.reference().filter(Symbols.Reference::hasType);
            Optional<Ir.RefDecl> ref = reference.map(r -> new Ir.RefDecl(r.span(), r.isSet(),
                    new Ir.Name(r.span(), r.typeName())));
            Ir.Card card = card(span, c.card());
            Optional<Ir.AbstractMultiplicity> abstractMultiplicity = Optional.empty();
            if (c.isAbstract() && c.isTopLevel() && !c.card().equals(Interval.Any)) {
                abstractMultiplicity = Optional.of(new Ir.AbstractMultiplicity(span, card));
                card = card(span, Interval.Any);
            }
            Optional<Ir.Initializer> initializer = Optional.empty();
            if (c.initializer().isPresent()) {
                // the value as written, or the constant of -> 1
                Optional<Exp> value = c.declaration() instanceof Syntax.Clafer d && d.init().isPresent()
                        ? Optional.of(d.init().get().exp())
                        : c.reference().map(Symbols.Reference::target);
                if (value.isPresent()) {
                    SourceSpan initSpan = c.declaration() instanceof Syntax.Clafer d && d.init().isPresent()
                            ? d.init().get().span() : value.get().span();
                    Optional<Ir.Exp> v = resolver.initializer(c, value.get(), initSpan);
                    if (v.isPresent()) {
                        initializer = Optional.of(new Ir.Initializer(value.get().span(), c.initializer().get(), v.get()));
                    }
                }
            }
            List<Ir.Element> elements = new ArrayList<>();
            if (reference.isPresent() && isSetExp(reference.get().target())) {
                for (Ir.Exp x : resolver.referenceConstraints(c)) {
                    elements.add(new Ir.Constraint(reference.get().span(), x, Optional.empty()));
                }
            }
            Optional<Syntax.Elements> block = switch (c.declaration()) {
                case Syntax.Clafer d -> d.elements();
                case Syntax.ClaferUse u -> u.elements();
                default -> Optional.empty();
            };
            Set<ClaferSymbol> built = new HashSet<>();
            for (Syntax.Element e : block.map(Syntax.Elements::elements).orElse(List.of())) {
                element(e, c, elements);
                if (e instanceof Syntax.Clafer || e instanceof Syntax.ClaferUse) {
                    built.add(symbols.of(e).orElseThrow());
                }
            }
            // the transition of the clafer, a constraint after its elements
            if (c.declaration() instanceof Syntax.Clafer d && d.transition().isPresent()) {
                resolver.transition(c, d.transition().get())
                        .ifPresent(x -> elements.add(new Ir.Constraint(d.transition().get().span(), x, Optional.empty())));
            }
            // the clafers relocated under it, last
            for (ClaferSymbol child : c.children()) {
                if (!built.contains(child)) {
                    elements.add(clafer(child));
                }
            }
            return new Ir.ClaferDecl(span, c.isAbstract(), modifiers, groupCard, new Ir.Name(c.nameSpan(), c.uid()),
                    superName, ref, card, abstractMultiplicity, initializer, elements, Optional.empty());
        }

        private static Ir.Card card(SourceSpan span, Interval i) {
            return new Ir.Card(span, i.low(), i.high() == Interval.Unbounded ? Ir.Card.Unbounded : i.high());
        }

        private static boolean isSetExp(Exp e) {
            return e instanceof Binary b && switch (b.op()) {
                case Union, UnionCom, Intersection, IntersectionDeprecated, Difference -> true;
                default -> false;
            };
        }
    }
}
