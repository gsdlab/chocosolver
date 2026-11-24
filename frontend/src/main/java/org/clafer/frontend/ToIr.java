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
        return toIr(parsed.model(), symbols, options(source));
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
        Builder builder = new Builder(symbols);
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
        diagnostics.sort((a, b) -> a.span().line() != b.span().line()
                ? Integer.compare(a.span().line(), b.span().line())
                : Integer.compare(a.span().column(), b.span().column()));
        return new Result(new Ir.Model(model.span(), options, elements, builder.objectives, builder.assertions),
                List.copyOf(diagnostics));
    }

    /**
     * An expression that needs step 10, unwinding to its constraint.
     */
    private static final class NeedsResolution extends RuntimeException {

        private final Diagnostic diagnostic;

        NeedsResolution(SourceSpan span, String message) {
            super(message, null, false, false);
            this.diagnostic = new Diagnostic(span, message + " (name and type resolution, step 10)");
        }
    }

    private static final class Builder {

        private final Symbols.Result symbols;
        private final Map<String, ClaferSymbol> byUid = new HashMap<>();
        private final Map<String, List<ClaferSymbol>> byName = new HashMap<>();
        final List<Diagnostic> diagnostics = new ArrayList<>();
        final List<Ir.Objective> objectives = new ArrayList<>();
        final List<Ir.Assertion> assertions = new ArrayList<>();

        Builder(Symbols.Result symbols) {
            this.symbols = symbols;
            for (ClaferSymbol c : symbols.all()) {
                byUid.putIfAbsent(c.uid(), c);
                byName.computeIfAbsent(c.name(), k -> new ArrayList<>()).add(c);
            }
        }

        /**
         * An element of a clafer, or of the model if the clafer is null.
         */
        void element(Syntax.Element e, ClaferSymbol parent, List<Ir.Element> out) {
            switch (e) {
                case Syntax.Clafer c -> clafer(c, parent, out);
                case Syntax.ClaferUse u -> clafer(u, parent, out);
                case Syntax.Constraint c -> {
                    for (Exp exp : c.exps()) {
                        convert(exp, Set.of()).ifPresent(x -> out.add(new Ir.Constraint(exp.span(), x, Optional.empty())));
                    }
                }
                case Syntax.Assertion a -> {
                    for (Exp exp : a.exps()) {
                        convert(exp, Set.of()).ifPresent(x -> assertions.add(new Ir.Assertion(exp.span(),
                                parent == null ? x : quantified(parent, x, exp.span()), Optional.empty())));
                    }
                }
                case Syntax.Goal g -> {
                    boolean maximize = g.kind() == Syntax.GoalKind.Maximize || g.kind() == Syntax.GoalKind.MaxDeprecated;
                    for (Exp exp : g.exps()) {
                        convert(exp, Set.of()).ifPresent(x -> objectives.add(new Ir.Objective(g.span(), maximize, x)));
                    }
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
                    Optional<Ir.Exp> v = convert(value.get(), Set.of());
                    if (v.isPresent()) {
                        initializer = Optional.of(new Ir.Initializer(value.get().span(), c.initializer().get(), v.get()));
                    }
                }
            }
            List<Ir.Element> elements = new ArrayList<>();
            reference.ifPresent(r -> referenceConstraints(c, r, elements));
            if (c.declaration() instanceof Syntax.Clafer d && d.transition().isPresent()) {
                diagnostics.add(new NeedsResolution(d.transition().get().span(),
                        "A transition of a clafer is not lowered yet").diagnostic);
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

        // ------------------------------------------------------------------
        // The constraints of a reference to a set expression
        // ------------------------------------------------------------------
        private static boolean isSetExp(Exp e) {
            return e instanceof Binary b && switch (b.op()) {
                case Union, UnionCom, Intersection, IntersectionDeprecated, Difference -> true;
                default -> false;
            };
        }

        /**
         * The target of {@code -> a ++ b} is one of its values: for clafers
         * {@code this.dref in a ++ b}, for primitives a formula of
         * {@code this.dref = v}, true for a primitive type.
         */
        private void referenceConstraints(ClaferSymbol c, Symbols.Reference r, List<Ir.Element> out) {
            if (!isSetExp(r.target())) {
                return;
            }
            SourceSpan span = r.span();
            Ir.Exp dref = new Ir.Dref(span, new Ir.This(span));
            if (c.referenceIds.stream().anyMatch(Symbols.Primitives::contains)) {
                try {
                    Member m = member(r.target(), dref);
                    if (m.formula != null) {
                        out.add(new Ir.Constraint(span, m.formula, Optional.empty()));
                    } else if (!m.truth) {
                        out.add(new Ir.Constraint(span, new Ir.QuantifiedSet(span, Ir.Quantifier.NO,
                                new Ir.This(span)), Optional.empty()));
                    }
                } catch (NeedsResolution e) {
                    diagnostics.add(e.diagnostic);
                }
            } else {
                convert(r.target(), Set.of()).ifPresent(t -> out.add(new Ir.Constraint(span,
                        new Ir.Binary(span, Ir.BinaryOp.IN, List.of(dref, t)), Optional.empty())));
            }
        }

        /**
         * A formula, or a truth if {@code formula} is null.
         */
        private record Member(Ir.Exp formula, boolean truth) {
        }

        private Member member(Exp e, Ir.Exp dref) {
            SourceSpan span = e.span();
            if (e instanceof Binary b && isSetExp(b)) {
                Member l = member(b.left(), dref);
                Member r = member(b.right(), dref);
                return switch (b.op()) {
                    case Union, UnionCom -> or(span, l, r);
                    case Intersection, IntersectionDeprecated -> and(span, l, r);
                    default -> and(span, l, not(span, r));
                };
            }
            if (e instanceof ClaferId id && id.name().path().size() == 1
                    && Symbols.Primitives.contains(id.name().path().get(0).text())) {
                return new Member(null, true);
            }
            return new Member(new Ir.Binary(span, Ir.BinaryOp.EQ, List.of(dref, exp(e, Set.of()))), false);
        }

        private static Member or(SourceSpan span, Member a, Member b) {
            if (a.formula == null) {
                return a.truth ? a : b;
            }
            if (b.formula == null) {
                return b.truth ? b : a;
            }
            return new Member(new Ir.Binary(span, Ir.BinaryOp.OR, List.of(a.formula, b.formula)), false);
        }

        private static Member and(SourceSpan span, Member a, Member b) {
            if (a.formula == null) {
                return a.truth ? b : a;
            }
            if (b.formula == null) {
                return b.truth ? a : b;
            }
            return new Member(new Ir.Binary(span, Ir.BinaryOp.AND, List.of(a.formula, b.formula)), false);
        }

        private static Member not(SourceSpan span, Member a) {
            return a.formula == null ? new Member(null, !a.truth)
                    : new Member(new Ir.Unary(span, Ir.UnaryOp.NOT, a.formula), false);
        }

        // ------------------------------------------------------------------
        // Expressions
        // ------------------------------------------------------------------
        private Optional<Ir.Exp> convert(Exp e, Set<String> locals) {
            try {
                return Optional.of(exp(e, locals));
            } catch (NeedsResolution x) {
                diagnostics.add(x.diagnostic);
                return Optional.empty();
            }
        }

        private Ir.Exp exp(Exp e, Set<String> locals) {
            SourceSpan span = e.span();
            return switch (e) {
                case Syntax.TransitionExp t -> new Ir.Transition(span, t.arrow().sync(), exp(t.from(), locals),
                        t.arrow().guard().map(g -> exp(g, locals)), exp(t.to(), locals));
                case Syntax.Quantified q -> {
                    Set<String> inner = new HashSet<>(locals);
                    List<Ir.Name> names = new ArrayList<>();
                    for (Ident local : q.decl().locals()) {
                        names.add(new Ir.Name(local.span(), local.text()));
                        inner.add(local.text());
                    }
                    // the set of a declaration does not see its own locals
                    Ir.Decl decl = new Ir.Decl(q.decl().span(), q.disj(), names, exp(q.decl().exp(), locals));
                    yield new Ir.Quantified(span, quantifier(q.quant()), List.of(decl), exp(q.body(), inner));
                }
                case Syntax.IfThenElse i -> new Ir.IfThenElse(span, exp(i.condition(), locals), exp(i.then(), locals),
                        exp(i.otherwise(), locals));
                case Syntax.Let l -> throw new NeedsResolution(span, "let is not lowered yet");
                case Syntax.Pattern p -> pattern(p, locals);
                case Syntax.Anchor a -> new Ir.Anchor(span, a.initially(), exp(a.exp(), locals));
                case Binary b -> binary(b, locals);
                case Syntax.Unary u -> unary(u, locals);
                case Syntax.QuantExp q -> new Ir.QuantifiedSet(span, quantifier(q.quant()), exp(q.exp(), locals));
                case ClaferId id -> head(id, locals);
                case Literal l -> literal(l, false, span);
            };
        }

        private static Ir.Quantifier quantifier(Syntax.Quant q) {
            return switch (q) {
                case All -> Ir.Quantifier.ALL;
                case No, Not -> Ir.Quantifier.NO;
                case Lone -> Ir.Quantifier.LONE;
                case One -> Ir.Quantifier.ONE;
                case Some -> Ir.Quantifier.SOME;
            };
        }

        private Ir.Exp literal(Literal l, boolean negative, SourceSpan span) {
            String sign = negative ? "-" : "";
            return switch (l.kind()) {
                case Int -> {
                    try {
                        long value = Long.parseLong(l.text());
                        yield new Ir.IntLit(span, negative ? -value : value);
                    } catch (NumberFormatException x) {
                        throw new NeedsResolution(span, "The number " + l.text() + " is too large");
                    }
                }
                case Real -> new Ir.DecimalLit(span, sign + l.text());
                case Double -> new Ir.DoubleLit(span, sign + l.text());
                case Str -> new Ir.StringLit(span, l.text());
            };
        }

        private Ir.Exp pattern(Syntax.Pattern p, Set<String> locals) {
            List<Ir.Exp> args = new ArrayList<>();
            for (Exp x : p.exps()) {
                args.add(exp(x, locals));
            }
            Ir.PatternScope scope = Ir.PatternScope.GLOBALLY;
            if (p.scope().isPresent()) {
                scope = switch (p.scope().get().kind()) {
                    case Before -> Ir.PatternScope.BEFORE;
                    case After -> Ir.PatternScope.AFTER;
                    case BetweenAnd -> Ir.PatternScope.BETWEEN;
                    case AfterUntil -> Ir.PatternScope.AFTER_UNTIL;
                };
                for (Exp x : p.scope().get().exps()) {
                    args.add(exp(x, locals));
                }
            }
            Ir.PatternKind kind = switch (p.kind()) {
                case Never -> Ir.PatternKind.NEVER;
                case Sometime -> Ir.PatternKind.SOMETIME;
                case LessOrOnce -> Ir.PatternKind.LONCE;
                case Always -> Ir.PatternKind.ALWAYS;
                case Precede -> Ir.PatternKind.PRECEDE;
                case Follow -> Ir.PatternKind.FOLLOW;
            };
            return new Ir.Pattern(p.span(), kind, scope, args);
        }

        private Ir.Exp unary(Syntax.Unary u, Set<String> locals) {
            SourceSpan span = u.span();
            // the minus of a number is a constant, -1, also as -(1), as in the compiler
            if (u.op() == Syntax.UnaryOp.Minus && u.exp() instanceof Literal l && l.kind() != Syntax.LiteralKind.Str) {
                return literal(l, true, span);
            }
            Ir.Exp arg = exp(u.exp(), locals);
            Ir.TemporalOp temporal = switch (u.op()) {
                case LtlF, Eventually -> Ir.TemporalOp.F;
                case LtlG, Globally -> Ir.TemporalOp.G;
                case LtlX, Next -> Ir.TemporalOp.X;
                case Previous -> Ir.TemporalOp.PREVIOUS;
                case Once -> Ir.TemporalOp.ONCE;
                case Historically -> Ir.TemporalOp.HISTORICALLY;
                default -> null;
            };
            if (temporal != null) {
                return new Ir.Temporal(span, temporal, List.of(arg));
            }
            Ir.UnaryOp op = switch (u.op()) {
                case Neg -> Ir.UnaryOp.NOT;
                case GMax -> Ir.UnaryOp.MAX;
                case GMin -> Ir.UnaryOp.MIN;
                case Sum -> Ir.UnaryOp.SUM;
                case Product -> Ir.UnaryOp.PRODUCT;
                case Card -> Ir.UnaryOp.CARD;
                case Minus -> Ir.UnaryOp.MINUS;
                default -> throw new IllegalStateException(u.op().toString());
            };
            if (op == Ir.UnaryOp.SUM || op == Ir.UnaryOp.PRODUCT) {
                // the tree has the instances, the printer writes their dref
                if (!(arg instanceof Ir.Dref dref)) {
                    throw new NeedsResolution(span, u.op().syntax() + " without .dref");
                }
                arg = dref.left();
            } else if (op == Ir.UnaryOp.MINUS || op == Ir.UnaryOp.MAX || op == Ir.UnaryOp.MIN) {
                requireDref(u.exp(), arg);
            }
            return new Ir.Unary(span, op, arg);
        }

        private Ir.Exp binary(Binary b, Set<String> locals) {
            SourceSpan span = b.span();
            Ir.Exp left = exp(b.left(), locals);
            if (b.op() == Syntax.BinaryOp.Join) {
                return join(span, left, b.right(), locals);
            }
            Ir.Exp right = exp(b.right(), locals);
            Ir.TemporalOp temporal = switch (b.op()) {
                case LtlU, Until -> Ir.TemporalOp.U;
                case LtlW, WeakUntil -> Ir.TemporalOp.W;
                case Since -> Ir.TemporalOp.SINCE;
                case Triggered -> Ir.TemporalOp.TRIGGERED;
                default -> null;
            };
            if (temporal != null) {
                return new Ir.Temporal(span, temporal, List.of(left, right));
            }
            Ir.BinaryOp op = switch (b.op()) {
                case Iff -> Ir.BinaryOp.IFF;
                case Implies -> Ir.BinaryOp.IMPLIES;
                case Or -> Ir.BinaryOp.OR;
                case Xor -> Ir.BinaryOp.XOR;
                case And -> Ir.BinaryOp.AND;
                case Lt -> Ir.BinaryOp.LT;
                case Gt -> Ir.BinaryOp.GT;
                case Eq -> Ir.BinaryOp.EQ;
                case Lte -> Ir.BinaryOp.LE;
                case Gte -> Ir.BinaryOp.GE;
                case Neq -> Ir.BinaryOp.NE;
                case In -> Ir.BinaryOp.IN;
                case NotIn -> Ir.BinaryOp.NOT_IN;
                case Add -> Ir.BinaryOp.ADD;
                case Sub -> Ir.BinaryOp.SUB;
                case Mul -> Ir.BinaryOp.MUL;
                case Div -> Ir.BinaryOp.DIV;
                case Rem -> Ir.BinaryOp.MOD;
                case Domain -> Ir.BinaryOp.DOMAIN;
                case Range -> Ir.BinaryOp.RANGE;
                case Union, UnionCom -> Ir.BinaryOp.UNION;
                case Difference -> Ir.BinaryOp.DIFF;
                case Intersection, IntersectionDeprecated -> Ir.BinaryOp.INTER;
                default -> throw new IllegalStateException(b.op().toString());
            };
            switch (op) {
                case ADD, SUB, MUL, DIV, MOD, LT, GT, LE, GE -> {
                    requireDref(b.left(), left);
                    requireDref(b.right(), right);
                }
                case EQ, NE -> {
                    // a value compared with a clafer that has one
                    if (isValue(right)) {
                        requireDref(b.left(), left);
                    }
                    if (isValue(left)) {
                        requireDref(b.right(), right);
                    }
                }
                default -> {
                }
            }
            return new Ir.Binary(span, op, List.of(left, right));
        }

        private static boolean isValue(Ir.Exp e) {
            return e instanceof Ir.IntLit || e instanceof Ir.DecimalLit || e instanceof Ir.DoubleLit
                    || e instanceof Ir.StringLit || e instanceof Ir.Dref || e instanceof Ir.Unary
                    || (e instanceof Ir.Binary b && switch (b.op()) {
                        case ADD, SUB, MUL, DIV, MOD -> true;
                        default -> false;
                    });
        }

        /**
         * A navigation to a clafer with a primitive reference, used as a
         * value, needs its {@code dref}, which only type checking inserts.
         */
        private void requireDref(Exp written, Ir.Exp e) {
            String last = switch (e) {
                case Ir.NameExp n -> n.name();
                case Ir.Join j when j.right() instanceof Ir.NameExp n -> n.name();
                default -> null;
            };
            ClaferSymbol c = last == null ? null : byUid.get(last);
            if (c != null && c.reference().flatMap(Symbols.Reference::primitive).isPresent()) {
                throw new NeedsResolution(written.span(), "The value of " + last + " without .dref");
            }
        }

        /**
         * A step of a navigation: {@code .dref}, {@code .parent}, a clafer, or
         * an expression in parentheses.
         */
        private Ir.Exp join(SourceSpan span, Ir.Exp left, Exp right, Set<String> locals) {
            if (right instanceof ClaferId id && id.name().path().size() == 1) {
                Ident step = id.name().path().get(0);
                switch (step.text()) {
                    case "dref" -> {
                        return new Ir.Dref(span, left);
                    }
                    case "parent" -> {
                        return new Ir.ParentOf(span, left);
                    }
                    default -> {
                        return new Ir.Join(span, left, new Ir.NameExp(step.span(), clafer(step, false)));
                    }
                }
            }
            return new Ir.Join(span, left, exp(right, locals));
        }

        /**
         * A name that starts a navigation, or stands alone.
         */
        private Ir.Exp head(ClaferId id, Set<String> locals) {
            if (id.name().path().size() != 1) {
                throw new NeedsResolution(id.span(), "A qualified name");
            }
            Ident name = id.name().path().get(0);
            String text = name.text();
            if (locals.contains(text)) {
                return new Ir.NameExp(id.span(), text);
            }
            return switch (text) {
                case "this" -> new Ir.This(id.span());
                case "noclafer" -> new Ir.NoClafer(id.span());
                // the root, as the core's anchors write it: some root holds in the first state only
                case "root" -> new Ir.NameExp(id.span(), text);
                case "parent", "dref", "children" -> throw new NeedsResolution(id.span(), "A bare " + text);
                default -> new Ir.NameExp(id.span(), clafer(name, true));
            };
        }

        /**
         * The UID of the clafer that a name is: a UID, or the name of exactly
         * one clafer. A name that starts a navigation is a global, so it is a
         * top-level clafer or a literal.
         */
        private String clafer(Ident name, boolean head) {
            String text = name.text();
            ClaferSymbol c = byUid.get(text);
            if (c == null) {
                List<ClaferSymbol> named = byName.getOrDefault(text, List.of());
                if (named.size() != 1) {
                    throw new NeedsResolution(name.span(), named.isEmpty()
                            ? "No clafer has the name or UID " + text
                            : "The name " + text + " is not unique, and not a UID");
                }
                c = named.get(0);
            }
            if (head && !c.isTopLevel() && !c.isEnum()) {
                throw new NeedsResolution(name.span(), "The nested clafer " + text + " without a path from this or the top level");
            }
            return c.uid();
        }
    }
}
