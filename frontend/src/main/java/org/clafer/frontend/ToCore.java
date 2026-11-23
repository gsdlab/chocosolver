package org.clafer.frontend;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import org.clafer.frontend.Ir.*;

/**
 * The lowering of the rich explicit model to the explicit core model: the
 * port of the Clafer compiler's {@code lowerAbstractBounds TopLevelBounds .
 * lowerToCore} ({@code Intermediate/Core.hs}, {@code Initializers.hs},
 * {@code Finality.hs}, {@code AbstractMultiplicity.hs}), pass by pass and in
 * the same order, so that {@code ExplicitPrinter.print(toCore(m))} is the
 * compiler's {@code --core} output for the model of {@code m}.
 * <ol>
 * <li>The initializers become constraints: of the parent, right after the
 * clafer, or, for the per-instance {@code :=} of a temporal model whose value
 * has no temporal operator, of the clafer, at the end of its elements.</li>
 * <li>A constraint or assertion with a pattern or a transition gets the
 * origin of the first one.</li>
 * <li>The patterns become the LTL formulas of Dwyer et al.'s catalogue, and the
 * transitions implications.</li>
 * <li>An enumeration becomes an abstract clafer and a clafer for each literal;
 * in a temporal model, they are final.</li>
 * <li>The finality modifiers become constraints of their clafers, at the end
 * of their elements.</li>
 * <li>{@code initial x} becomes {@code [ initially (some this.x) ]} at the end
 * of the parent's elements.</li>
 * <li>The anchors become guards on the birth and the death of the context
 * instance, {@code this}, or the root at the top level.</li>
 * <li>The multiplicity of a top-level abstract clafer becomes a constraint
 * right after it.</li>
 * </ol>
 * A model is temporal if it has a trace length, {@code --tl=N}: then
 * {@code :=} gives the value at the start of each lifetime; in a static model,
 * it is {@code =}.
 * <p>
 * The core is a fixpoint: {@code toCore} of a core model is the model.
 * <p>
 * The provenance of the lowered clafers and constraints names the rich
 * construct and its position in the rich model, with the given file name. The
 * rich model writes the clock of every {@code :=}, so the origin of a lowered
 * {@code :=} is {@code := each} or {@code := together}, where the compiler,
 * from the source, writes {@code :=} for one without a written clock.
 */
public final class ToCore {

    private final boolean temporal;
    private final String file;

    private ToCore(boolean temporal, String file) {
        this.temporal = temporal;
        this.file = file;
    }

    /**
     * @param model a rich explicit model
     * @param file the name of its file, for the provenance
     * @return its explicit core model
     */
    public static Model toCore(Model model, String file) {
        ToCore lowering = new ToCore(model.options().traceLength().isPresent(), file);
        Model m = model;
        Set<String> taken = taken(m);
        m = lowering.lowerInitializers(m, taken);
        m = lowering.markRich(m);
        m = lowering.lowerPatternsAndTransitions(m);
        m = lowering.lowerEnums(m);
        m = lowering.lowerFinality(m);
        m = lowering.lowerInitial(m);
        m = lowering.lowerAnchors(m);
        m = lowering.lowerTopLevelBounds(m);
        return new Model(m.span(), new Options(true, m.options().traceLength()), m.elements(), m.objectives(),
                m.assertions());
    }

    /**
     * @param model a rich explicit model
     * @return its explicit core model, its provenance positions in
     * {@code model.des.cfr}
     */
    public static Model toCore(Model model) {
        return toCore(model, "model.des.cfr");
    }

    // ------------------------------------------------------------------
    // Expressions
    // ------------------------------------------------------------------
    private static Exp some(Exp set) {
        return new QuantifiedSet(set.span(), Quantifier.SOME, set);
    }

    private static Exp no(Exp set) {
        return new QuantifiedSet(set.span(), Quantifier.NO, set);
    }

    /**
     * @return the set of {@code some set}, if the formula is one
     */
    private static Optional<Exp> someOf(Exp p) {
        return p instanceof QuantifiedSet q && q.quantifier() == Quantifier.SOME ? Optional.of(q.set()) : Optional.empty();
    }

    /**
     * {@code !p}, and {@code no x} for {@code some x}, as the compiler
     * writes it.
     */
    private static Exp not(Exp p) {
        Optional<Exp> set = someOf(p);
        return set.isPresent() ? no(set.get()) : new Unary(p.span(), UnaryOp.NOT, p);
    }

    private static Exp bin(SourceSpan span, BinaryOp op, Exp a, Exp b) {
        return new Binary(span, op, List.of(a, b));
    }

    private static Exp tmp(SourceSpan span, TemporalOp op, Exp... args) {
        return new Temporal(span, op, List.of(args));
    }

    private static Exp join(Exp left, String name) {
        return new Join(left.span(), left, new NameExp(left.span(), name));
    }

    private static Exp local(SourceSpan span, String name) {
        return new NameExp(span, name);
    }

    private static Exp all(SourceSpan span, String v, Exp set, Exp body) {
        return new Quantified(span, Quantifier.ALL, List.of(new Decl(span, false, List.of(new Name(span, v)), set)), body);
    }

    /**
     * Rewrites the expression bottom-up.
     */
    private static Exp transform(Exp e, Function<Exp, Exp> f) {
        Function<Exp, Exp> t = x -> transform(x, f);
        Exp rebuilt = switch (e) {
            case Join x -> new Join(x.span(), t.apply(x.left()), t.apply(x.right()));
            case Dref x -> new Dref(x.span(), t.apply(x.left()));
            case ParentOf x -> new ParentOf(x.span(), t.apply(x.left()));
            case Unary x -> new Unary(x.span(), x.op(), t.apply(x.arg()));
            case Binary x -> new Binary(x.span(), x.op(), x.operands().stream().map(t).toList());
            case IfThenElse x -> new IfThenElse(x.span(), t.apply(x.condition()), t.apply(x.then()), t.apply(x.otherwise()));
            case Quantified x -> new Quantified(x.span(), x.quantifier(), x.decls().stream()
                    .map(d -> new Decl(d.span(), d.disjoint(), d.locals(), t.apply(d.set()))).toList(), t.apply(x.body()));
            case QuantifiedSet x -> new QuantifiedSet(x.span(), x.quantifier(), t.apply(x.set()));
            case Call x -> new Call(x.span(), x.function(), x.args().stream().map(t).toList());
            case Cast x -> new Cast(x.span(), x.up(), t.apply(x.base()), x.type());
            case Connected x -> new Connected(x.span(), t.apply(x.nodes()), t.apply(x.relation()), x.directed());
            case Temporal x -> new Temporal(x.span(), x.op(), x.args().stream().map(t).toList());
            case Anchor x -> new Anchor(x.span(), x.initially(), t.apply(x.body()));
            case Pattern x -> new Pattern(x.span(), x.kind(), x.scope(), x.args().stream().map(t).toList());
            case Transition x -> new Transition(x.span(), x.sync(), t.apply(x.from()), x.guard().map(t), t.apply(x.to()));
            default -> e;
        };
        return f.apply(rebuilt);
    }

    private static boolean hasTemporal(Exp e) {
        return Core.firstTemporal(e).isPresent();
    }

    private Optional<Origin> origin(String construct, SourceSpan span) {
        return Optional.of(new Origin(construct, Optional.of(new Position(file, span.line(), span.column()))));
    }

    // ------------------------------------------------------------------
    // Names
    // ------------------------------------------------------------------
    /**
     * The name of a clafer as written in the source, the last part of its
     * UID, which is qualified with {@code __} where the name is not unique.
     */
    private static String ident(String uid) {
        int i = uid.lastIndexOf("__");
        return i < 0 || i + 2 >= uid.length() ? uid : uid.substring(i + 2);
    }

    /**
     * The names that a local variable must not shadow: the UIDs and the
     * names of the clafers, and the variables of the quantifiers.
     */
    private static Set<String> taken(Model model) {
        Set<String> taken = new HashSet<>();
        collectTaken(model, taken);
        return taken;
    }

    private static void collectTaken(Node node, Set<String> taken) {
        switch (node) {
            case ClaferDecl c -> {
                taken.add(c.name().text());
                taken.add(ident(c.name().text()));
            }
            case EnumDecl e -> {
                taken.add(e.name().text());
                e.literals().forEach(l -> taken.add(l.text()));
            }
            case Decl d -> d.locals().forEach(l -> taken.add(l.text()));
            default -> {
            }
        }
        for (Node child : Core.children(node)) {
            collectTaken(child, taken);
        }
    }

    /**
     * {@code base_k} for the first k from 1 that no name has.
     */
    private static String fresh(Set<String> taken, String base) {
        for (int k = 1;; k++) {
            String name = base + "_" + k;
            if (!taken.contains(name)) {
                return name;
            }
        }
    }

    private static ClaferDecl withElements(ClaferDecl c, List<Element> elements) {
        return new ClaferDecl(c.span(), c.isAbstract(), c.modifiers(), c.groupCard(), c.name(), c.superName(), c.ref(),
                c.card(), c.abstractMultiplicity(), c.initializer(), elements, c.origin());
    }

    private static Model withElements(Model m, List<Element> elements) {
        return new Model(m.span(), m.options(), elements, m.objectives(), m.assertions());
    }

    // ------------------------------------------------------------------
    // 1. Initializers
    // ------------------------------------------------------------------
    private Model lowerInitializers(Model m, Set<String> taken) {
        return withElements(m, lowerInitializers(m.elements(), true, taken));
    }

    private List<Element> lowerInitializers(List<Element> elements, boolean topLevel, Set<String> taken) {
        List<Element> out = new ArrayList<>();
        for (Element e : elements) {
            if (!(e instanceof ClaferDecl c)) {
                out.add(e);
                continue;
            }
            List<Element> children = new ArrayList<>(lowerInitializers(c.elements(), false, taken));
            List<Element> parentConstraints = new ArrayList<>();
            if (c.initializer().isPresent()) {
                Initializer init = c.initializer().get();
                String construct = switch (init.kind()) {
                    case FIXED -> "=";
                    case EACH -> ":= each";
                    case TOGETHER -> ":= together";
                };
                Optional<Origin> origin = origin(construct, c.span());
                boolean lowered = !temporal || init.kind() != InitKind.EACH || hasTemporal(init.value());
                if (lowered) {
                    parentConstraints.add(new Constraint(init.span(), asConstraint(c, init, topLevel, taken), origin));
                } else {
                    children.add(new Constraint(init.span(), initialValue(init, topLevel), origin));
                }
            }
            out.add(new ClaferDecl(c.span(), c.isAbstract(), c.modifiers(), c.groupCard(), c.name(), c.superName(),
                    c.ref(), c.card(), c.abstractMultiplicity(), Optional.empty(), children, c.origin()));
            out.addAll(parentConstraints);
        }
        return out;
    }

    /**
     * The instances of the initialized clafer in the context of its parent:
     * {@code this.x}, or {@code x} at the top level.
     */
    private static Exp initialized(ClaferDecl x, boolean topLevel) {
        SourceSpan span = x.name().span();
        return topLevel ? new NameExp(span, x.name().text()) : join(new This(span), x.name().text());
    }

    private Exp asConstraint(ClaferDecl x, Initializer init, boolean topLevel, Set<String> taken) {
        SourceSpan span = init.span();
        Exp xs = initialized(x, topLevel);
        Exp value = init.value();
        Exp implied = bin(span, BinaryOp.IMPLIES, some(xs), bin(span, BinaryOp.EQ, new Dref(span, xs), value));
        boolean fixed = init.kind() == InitKind.FIXED;
        boolean together = init.kind() == InitKind.TOGETHER;
        if (temporal && !fixed && together) {
            return new Anchor(span, true, implied);
        }
        String v = ident(x.name().text());
        if (temporal && !fixed) {
            // each instance that appears has the value
            String c = taken.contains(v) ? fresh(taken, v) : v;
            Exp local = local(span, c);
            return all(span, c, xs, bin(span, BinaryOp.IMPLIES,
                    new Unary(span, UnaryOp.NOT, tmp(span, TemporalOp.PREVIOUS, bin(span, BinaryOp.IN, local, xs))),
                    bin(span, BinaryOp.EQ, new Dref(span, local), value)));
        }
        if (!fixed && !together && plural(x)) {
            String c = taken.contains(v) ? fresh(taken, v) : v;
            return all(span, c, xs, bin(span, BinaryOp.EQ, new Dref(span, local(span, c)), value));
        }
        return implied;
    }

    /**
     * A concrete clafer with more than one instance under a parent.
     */
    private static boolean plural(ClaferDecl x) {
        return !x.isAbstract() && x.card().high() != 0 && x.card().high() != 1;
    }

    /**
     * The per-instance initial value in the context of the clafer:
     * {@code initially (this.dref = e')}, where {@code e'} has
     * {@code this.parent}, or the root at the top level, for {@code this},
     * the parent instance.
     */
    private static Exp initialValue(Initializer init, boolean topLevel) {
        SourceSpan span = init.span();
        Exp value = transform(init.value(), e -> e instanceof This t
                ? (topLevel ? new NameExp(t.span(), "root") : new ParentOf(t.span(), t)) : e);
        return new Anchor(span, true, bin(span, BinaryOp.EQ, new Dref(span, new This(span)), value));
    }

    // ------------------------------------------------------------------
    // 2. The origins of the patterns and transitions
    // ------------------------------------------------------------------
    private Model markRich(Model m) {
        List<Assertion> assertions = m.assertions().stream()
                .map(a -> new Assertion(a.span(), a.exp(), a.origin().isPresent() ? a.origin() : richOrigin(a.exp())))
                .toList();
        return new Model(m.span(), m.options(), mapConstraints(m.elements(),
                c -> new Constraint(c.span(), c.exp(), c.origin().isPresent() ? c.origin() : richOrigin(c.exp()))),
                m.objectives(), assertions);
    }

    /**
     * The origin of the first pattern or transition of an expression.
     */
    private Optional<Origin> richOrigin(Exp e) {
        Optional<Node> first = firstRichOperator(e);
        return first.flatMap(n -> origin(switch (n) {
            case Pattern p -> patternOp(p);
            default -> "transition";
        }, position(n)));
    }

    /**
     * The position of a pattern or a transition, as the compiler gives it:
     * an infix expression is at its first operand, inside its parentheses,
     * and so on down.
     */
    private static SourceSpan position(Node n) {
        return switch (n) {
            case Pattern p when p.kind().arity() == 2 -> position(p.args().get(0));
            case Transition t -> position(t.from());
            case Binary b -> position(b.operands().get(0));
            case Join j -> position(j.left());
            case Dref d -> position(d.left());
            case ParentOf p -> position(p.left());
            case Temporal t when t.op().arity() == 2 -> position(t.args().get(0));
            default -> n.span();
        };
    }

    private static Optional<Node> firstRichOperator(Node node) {
        if (node instanceof Pattern || node instanceof Transition) {
            return Optional.of(node);
        }
        for (Node child : Core.children(node)) {
            Optional<Node> found = firstRichOperator(child);
            if (found.isPresent()) {
                return found;
            }
        }
        return Optional.empty();
    }

    private static String patternOp(Pattern p) {
        return p.kind().keyword() + switch (p.scope()) {
            case GLOBALLY -> "";
            case BEFORE -> " before";
            case AFTER -> " after";
            case BETWEEN -> " between";
            case AFTER_UNTIL -> " after until";
        };
    }

    /**
     * The elements with each constraint mapped, at every depth.
     */
    private static List<Element> mapConstraints(List<Element> elements, Function<Constraint, Constraint> f) {
        List<Element> out = new ArrayList<>();
        for (Element e : elements) {
            out.add(switch (e) {
                case Constraint c -> f.apply(c);
                case ClaferDecl c -> withElements(c, mapConstraints(c.elements(), f));
                case RootBlock r -> new RootBlock(r.span(), r.constraints().stream().map(f).toList());
                default -> e;
            });
        }
        return out;
    }

    // ------------------------------------------------------------------
    // 3. Patterns and transitions
    // ------------------------------------------------------------------
    private Model lowerPatternsAndTransitions(Model m) {
        return new Model(m.span(), m.options(),
                mapConstraints(m.elements(), c -> new Constraint(c.span(), lowerExp(c.exp()), c.origin())),
                m.objectives().stream().map(o -> new Objective(o.span(), o.maximize(), lowerExp(o.exp()))).toList(),
                m.assertions().stream().map(a -> new Assertion(a.span(), lowerExp(a.exp()), a.origin())).toList());
    }

    /**
     * The expression with its patterns and transitions in LTL.
     */
    private static Exp lowerExp(Exp e) {
        return switch (e) {
            case Transition t -> {
                Exp[] parts = transitionParts(t);
                yield bin(t.span(), BinaryOp.IMPLIES, parts[0], parts[1]);
            }
            case Pattern p -> pattern(p, p.args().stream().map(ToCore::lowerExp).toList());
            default -> shallow(e, ToCore::lowerExp);
        };
    }

    /**
     * The expression with the function applied to its direct subexpressions.
     */
    private static Exp shallow(Exp e, Function<Exp, Exp> t) {
        return switch (e) {
            case Join x -> new Join(x.span(), t.apply(x.left()), t.apply(x.right()));
            case Dref x -> new Dref(x.span(), t.apply(x.left()));
            case ParentOf x -> new ParentOf(x.span(), t.apply(x.left()));
            case Unary x -> new Unary(x.span(), x.op(), t.apply(x.arg()));
            case Binary x -> new Binary(x.span(), x.op(), x.operands().stream().map(t).toList());
            case IfThenElse x -> new IfThenElse(x.span(), t.apply(x.condition()), t.apply(x.then()), t.apply(x.otherwise()));
            case Quantified x -> new Quantified(x.span(), x.quantifier(), x.decls().stream()
                    .map(d -> new Decl(d.span(), d.disjoint(), d.locals(), t.apply(d.set()))).toList(), t.apply(x.body()));
            case QuantifiedSet x -> new QuantifiedSet(x.span(), x.quantifier(), t.apply(x.set()));
            case Call x -> new Call(x.span(), x.function(), x.args().stream().map(t).toList());
            case Cast x -> new Cast(x.span(), x.up(), t.apply(x.base()), x.type());
            case Connected x -> new Connected(x.span(), t.apply(x.nodes()), t.apply(x.relation()), x.directed());
            case Temporal x -> new Temporal(x.span(), x.op(), x.args().stream().map(t).toList());
            case Anchor x -> new Anchor(x.span(), x.initially(), t.apply(x.body()));
            case Pattern x -> new Pattern(x.span(), x.kind(), x.scope(), x.args().stream().map(t).toList());
            case Transition x -> new Transition(x.span(), x.sync(), t.apply(x.from()), x.guard().map(t), t.apply(x.to()));
            default -> e;
        };
    }

    /**
     * The precondition and the step of a transition, whose target is a
     * sequence. The context instance of {@code -->>} lasts with the
     * precondition, unless the precondition is the presence of the context,
     * {@code some this}, as the compiler writes the transition of a clafer.
     */
    private static Exp[] transitionParts(Transition t) {
        SourceSpan span = t.span();
        Exp a = lowerExp(t.from());
        Exp pre = t.guard().isPresent() ? bin(span, BinaryOp.AND, a, lowerExp(t.guard().get())) : a;
        Exp next = sequence(t.to());
        if (!t.sync()) {
            return new Exp[]{pre, tmp(span, TemporalOp.X, next)};
        }
        boolean ofThis = someOf(t.from()).map(s -> s instanceof This).orElse(false);
        Exp lasting = ofThis ? a : bin(span, BinaryOp.AND, some(new This(span)), a);
        return new Exp[]{pre, tmp(span, TemporalOp.U, lasting, next)};
    }

    /**
     * The target of a transition: in a chain, the states follow each other.
     */
    private static Exp sequence(Exp e) {
        if (e instanceof Transition t) {
            Exp[] parts = transitionParts(t);
            return bin(t.span(), BinaryOp.AND, parts[0], parts[1]);
        }
        return lowerExp(e);
    }

    /**
     * A pattern in a scope, with its arguments in LTL: the formulas of Dwyer et
     * al.'s catalogue, as {@code Core.pattern} writes them.
     */
    private static Exp pattern(Pattern pe, List<Exp> args) {
        SourceSpan s = pe.span();
        Function<Exp, Exp> f = a -> tmp(s, TemporalOp.F, a);
        Function<Exp, Exp> g = a -> tmp(s, TemporalOp.G, a);
        java.util.function.BinaryOperator<Exp> u = (a, b) -> tmp(s, TemporalOp.U, a, b);
        java.util.function.BinaryOperator<Exp> w = (a, b) -> tmp(s, TemporalOp.W, a, b);
        java.util.function.BinaryOperator<Exp> impl = (a, b) -> bin(s, BinaryOp.IMPLIES, a, b);
        java.util.function.BinaryOperator<Exp> and = (a, b) -> bin(s, BinaryOp.AND, a, b);
        java.util.function.BinaryOperator<Exp> or = (a, b) -> bin(s, BinaryOp.OR, a, b);
        int own = pe.kind().arity();
        Exp p = args.get(own - 1);
        Exp sArg = own == 2 ? args.get(0) : null;
        List<Exp> scope = args.subList(own, args.size());
        Exp q = null, r = null;
        switch (pe.scope()) {
            case BEFORE -> r = scope.get(0);
            case AFTER -> q = scope.get(0);
            case BETWEEN, AFTER_UNTIL -> {
                q = scope.get(0);
                r = scope.get(1);
            }
            default -> {
            }
        }
        return switch (pe.kind()) {
            case NEVER -> switch (pe.scope()) {
                case GLOBALLY -> g.apply(not(p));
                case BEFORE -> impl.apply(f.apply(r), u.apply(not(p), r));
                case AFTER -> g.apply(impl.apply(q, g.apply(not(p))));
                case BETWEEN -> g.apply(impl.apply(and.apply(and.apply(q, not(r)), f.apply(r)), u.apply(not(p), r)));
                case AFTER_UNTIL -> g.apply(impl.apply(and.apply(q, not(r)), w.apply(not(p), r)));
            };
            case SOMETIME -> switch (pe.scope()) {
                case GLOBALLY -> f.apply(p);
                case BEFORE -> w.apply(not(r), and.apply(p, not(r)));
                case AFTER -> or.apply(g.apply(not(q)), f.apply(and.apply(q, f.apply(p))));
                case BETWEEN -> g.apply(impl.apply(and.apply(q, not(r)), w.apply(not(r), and.apply(p, not(r)))));
                case AFTER_UNTIL -> g.apply(impl.apply(and.apply(q, not(r)), u.apply(not(r), and.apply(p, not(r)))));
            };
            case LONCE -> switch (pe.scope()) {
                case GLOBALLY -> w.apply(not(p), w.apply(p, g.apply(not(p))));
                case BEFORE -> impl.apply(f.apply(r), u.apply(and.apply(not(p), not(r)),
                        or.apply(r, u.apply(and.apply(p, not(r)), or.apply(r, u.apply(not(p), r))))));
                case AFTER -> impl.apply(f.apply(q), u.apply(not(q),
                        and.apply(q, w.apply(not(p), w.apply(p, g.apply(not(p)))))));
                case BETWEEN -> g.apply(impl.apply(and.apply(q, f.apply(r)), u.apply(and.apply(not(p), not(r)),
                        or.apply(r, u.apply(and.apply(p, not(r)), or.apply(r, u.apply(not(p), r)))))));
                case AFTER_UNTIL -> g.apply(impl.apply(q, u.apply(and.apply(not(p), not(r)),
                        or.apply(r, u.apply(and.apply(p, not(r)),
                                or.apply(or.apply(r, w.apply(not(p), r)), g.apply(p)))))));
            };
            case ALWAYS -> switch (pe.scope()) {
                case GLOBALLY -> g.apply(p);
                case BEFORE -> impl.apply(f.apply(r), u.apply(p, r));
                case AFTER -> g.apply(impl.apply(q, g.apply(p)));
                case BETWEEN -> g.apply(impl.apply(and.apply(and.apply(q, not(r)), f.apply(r)), u.apply(p, r)));
                case AFTER_UNTIL -> g.apply(impl.apply(and.apply(q, not(r)), w.apply(p, r)));
            };
            case PRECEDE -> switch (pe.scope()) {
                case GLOBALLY -> w.apply(not(p), sArg);
                case BEFORE -> impl.apply(f.apply(r), u.apply(not(p), or.apply(sArg, r)));
                case AFTER -> impl.apply(f.apply(q), u.apply(not(q), and.apply(q, w.apply(not(p), sArg))));
                case BETWEEN -> g.apply(impl.apply(and.apply(and.apply(q, not(r)), f.apply(r)),
                        u.apply(not(p), or.apply(sArg, r))));
                case AFTER_UNTIL -> g.apply(impl.apply(and.apply(q, not(r)), w.apply(not(p), or.apply(sArg, r))));
            };
            case FOLLOW -> switch (pe.scope()) {
                case GLOBALLY -> g.apply(impl.apply(p, f.apply(sArg)));
                case BEFORE -> impl.apply(f.apply(r), u.apply(impl.apply(p, u.apply(not(r), and.apply(sArg, not(r)))), r));
                case AFTER -> g.apply(impl.apply(q, g.apply(impl.apply(p, f.apply(sArg)))));
                case BETWEEN -> g.apply(impl.apply(and.apply(and.apply(q, not(r)), f.apply(r)),
                        u.apply(impl.apply(p, u.apply(not(r), and.apply(sArg, not(r)))), r)));
                case AFTER_UNTIL -> g.apply(impl.apply(and.apply(q, not(r)),
                        w.apply(impl.apply(p, u.apply(not(r), and.apply(sArg, not(r)))), r)));
            };
        };
    }

    // ------------------------------------------------------------------
    // 4. Enumerations
    // ------------------------------------------------------------------
    private Model lowerEnums(Model m) {
        List<Element> out = new ArrayList<>();
        for (Element e : m.elements()) {
            if (!(e instanceof EnumDecl en)) {
                out.add(e);
                continue;
            }
            // in a temporal model, the enumeration is final, and so are its literals
            List<Modifier> fin = temporal ? List.of(new Modifier(en.span(), ModifierKind.FINAL)) : List.of();
            SourceSpan span = en.span();
            out.add(new ClaferDecl(span, true, fin, new Card(span, 0, Card.Unbounded), en.name(), Optional.empty(),
                    Optional.empty(), new Card(span, 0, Card.Unbounded), Optional.empty(), Optional.empty(), List.of(),
                    origin("enum", span)));
            for (Name literal : en.literals()) {
                List<Modifier> literalFin = temporal ? List.of(new Modifier(literal.span(), ModifierKind.FINAL)) : List.of();
                out.add(new ClaferDecl(literal.span(), false, literalFin, new Card(literal.span(), 0, Card.Unbounded),
                        literal, Optional.of(en.name()), Optional.empty(), new Card(literal.span(), 1, 1), Optional.empty(),
                        Optional.empty(), List.of(), origin("enum", literal.span())));
            }
        }
        return withElements(m, out);
    }

    // ------------------------------------------------------------------
    // 5. Finality
    // ------------------------------------------------------------------
    /**
     * The modifiers of a reference, as the desugarer gives them.
     */
    private enum RefModifier {
        FINAL_REF_TARGET, FINAL_REF, FINAL_TARGET
    }

    private static Set<ModifierKind> kinds(ClaferDecl c) {
        Set<ModifierKind> kinds = EnumSet.noneOf(ModifierKind.class);
        c.modifiers().forEach(m -> kinds.add(m.kind()));
        return kinds;
    }

    /**
     * The modifiers of the clafer's own reference: {@code final} fixes it
     * whole, {@code finalref} and {@code finaltarget} their parts.
     */
    private static Set<RefModifier> refModifiers(ClaferDecl c) {
        Set<RefModifier> mods = EnumSet.noneOf(RefModifier.class);
        if (c.ref().isPresent()) {
            Set<ModifierKind> kinds = kinds(c);
            if (kinds.contains(ModifierKind.FINAL)) {
                mods.add(RefModifier.FINAL_REF_TARGET);
            }
            if (kinds.contains(ModifierKind.FINALREF)) {
                mods.add(RefModifier.FINAL_REF);
            }
            if (kinds.contains(ModifierKind.FINALTARGET)) {
                mods.add(RefModifier.FINAL_TARGET);
            }
        }
        return mods;
    }

    private Model lowerFinality(Model m) {
        Map<String, ClaferDecl> clafers = new HashMap<>();
        collectClafers(m.elements(), clafers);
        Set<String> taken = taken(m);
        return withElements(m, lowerFinality(m.elements(), true, clafers, taken));
    }

    private static void collectClafers(List<Element> elements, Map<String, ClaferDecl> clafers) {
        for (Element e : elements) {
            if (e instanceof ClaferDecl c) {
                clafers.put(c.name().text(), c);
                collectClafers(c.elements(), clafers);
            }
        }
    }

    private List<Element> lowerFinality(List<Element> elements, boolean topLevel, Map<String, ClaferDecl> clafers,
            Set<String> taken) {
        List<Element> out = new ArrayList<>();
        for (Element e : elements) {
            if (!(e instanceof ClaferDecl c)) {
                out.add(e);
                continue;
            }
            List<Element> children = new ArrayList<>(lowerFinality(c.elements(), false, clafers, taken));
            children.addAll(finality(c, topLevel, clafers, taken));
            List<Modifier> kept = c.modifiers().stream().filter(mod -> mod.kind() == ModifierKind.INITIAL).toList();
            out.add(new ClaferDecl(c.span(), c.isAbstract(), kept, c.groupCard(), c.name(), c.superName(), c.ref(),
                    c.card(), c.abstractMultiplicity(), c.initializer(), children, c.origin()));
        }
        return out;
    }

    /**
     * The constraints of the finality modifiers of a clafer, in its context.
     */
    private List<Constraint> finality(ClaferDecl x, boolean topLevel, Map<String, ClaferDecl> clafers, Set<String> taken) {
        Set<ModifierKind> kinds = kinds(x);
        boolean isFinal = kinds.contains(ModifierKind.FINAL);
        // the reference that x inherits: the first super clafer's with one
        ClaferDecl inheritedFrom = null;
        Set<String> seen = new HashSet<>();
        for (ClaferDecl s = superOf(x, clafers); s != null && seen.add(s.name().text()); s = superOf(s, clafers)) {
            if (s.ref().isPresent()) {
                inheritedFrom = s;
                break;
            }
        }
        boolean hasRef = x.ref().isPresent() || inheritedFrom != null;
        Set<RefModifier> own = refModifiers(x);
        boolean inheritedFixed = inheritedFrom != null && (refModifiers(inheritedFrom).contains(RefModifier.FINAL_REF_TARGET)
                || refModifiers(inheritedFrom).contains(RefModifier.FINAL_REF));
        boolean fixesEach;
        boolean fixesTargets;
        if (x.ref().isPresent()) {
            fixesEach = isFinal || own.contains(RefModifier.FINAL_REF_TARGET) || own.contains(RefModifier.FINAL_REF);
            fixesTargets = own.contains(RefModifier.FINAL_TARGET);
        } else {
            fixesEach = (isFinal || kinds.contains(ModifierKind.FINALREF)) && !inheritedFixed;
            fixesTargets = kinds.contains(ModifierKind.FINALTARGET);
        }
        String uid = x.name().text();
        SourceSpan span = x.span();
        String vc = fresh(taken, ident(uid));
        Set<String> takenVc = new HashSet<>(taken);
        takenVc.add(vc);
        String vt = fresh(takenVc, ident(uid));

        Exp thisX = new This(span);
        Exp parentOfThis = topLevel ? new NameExp(span, "root") : new ParentOf(span, new This(span));
        Exp siblings = topLevel ? new NameExp(span, uid) : join(parentOfThis, uid);
        Exp c = local(span, vc);
        Exp tv = local(span, vt);
        Function<Exp, Exp> since = a -> bin(span, BinaryOp.IMPLIES, tmp(span, TemporalOp.PREVIOUS, some(parentOfThis)),
                tmp(span, TemporalOp.PREVIOUS, a));

        List<Constraint> out = new ArrayList<>();
        if (isFinal && !x.isAbstract()) {
            Optional<Origin> o = origin("final", span);
            out.add(new Constraint(span, all(span, vc, thisX, tmp(span, TemporalOp.X, bin(span, BinaryOp.IN, c, siblings))), o));
            out.add(new Constraint(span, all(span, vc, thisX, since.apply(bin(span, BinaryOp.IN, c, siblings))), o));
        }
        if (fixesEach && hasRef) {
            Optional<Origin> o = origin(isFinal ? "final" : "finalref", span);
            Exp drefC = new Dref(span, c);
            out.add(new Constraint(span, all(span, vc, thisX, all(span, vt, drefC, tmp(span, TemporalOp.X,
                    bin(span, BinaryOp.IMPLIES, some(drefC), bin(span, BinaryOp.IN, tv, drefC))))), o));
        }
        if (fixesTargets && hasRef) {
            Optional<Origin> o = origin("finaltarget", span);
            Exp drefThis = new Dref(span, thisX);
            Exp drefSiblings = new Dref(span, siblings);
            out.add(new Constraint(span, all(span, vt, drefThis, tmp(span, TemporalOp.X,
                    bin(span, BinaryOp.IN, tv, drefSiblings))), o));
            out.add(new Constraint(span, all(span, vt, drefThis, since.apply(bin(span, BinaryOp.IN, tv, drefSiblings))), o));
        }
        return out;
    }

    private static ClaferDecl superOf(ClaferDecl c, Map<String, ClaferDecl> clafers) {
        return c.superName().map(s -> clafers.get(s.text())).orElse(null);
    }

    // ------------------------------------------------------------------
    // 6. initial
    // ------------------------------------------------------------------
    private Model lowerInitial(Model m) {
        return withElements(m, lowerInitial(m.elements(), true));
    }

    private List<Element> lowerInitial(List<Element> elements, boolean topLevel) {
        List<Element> out = new ArrayList<>();
        List<Element> initials = new ArrayList<>();
        for (Element e : elements) {
            if (!(e instanceof ClaferDecl c)) {
                out.add(e);
                continue;
            }
            boolean initial = c.modifiers().stream().anyMatch(mod -> mod.kind() == ModifierKind.INITIAL);
            out.add(new ClaferDecl(c.span(), c.isAbstract(), List.of(), c.groupCard(), c.name(), c.superName(), c.ref(),
                    c.card(), c.abstractMultiplicity(), c.initializer(), lowerInitial(c.elements(), false), c.origin()));
            // a top-level abstract clafer has no instances of its own
            if (initial && !(c.isAbstract() && topLevel)) {
                SourceSpan span = c.span();
                Exp instances = topLevel ? new NameExp(span, c.name().text()) : join(new This(span), c.name().text());
                initials.add(new Constraint(span, new Anchor(span, true, some(instances)), origin("initial", span)));
            }
        }
        out.addAll(initials);
        return out;
    }

    // ------------------------------------------------------------------
    // 7. Anchors
    // ------------------------------------------------------------------
    private Model lowerAnchors(Model m) {
        return new Model(m.span(), m.options(), lowerAnchors(m.elements(), true),
                m.objectives().stream().map(o -> new Objective(o.span(), o.maximize(), anchors(o.exp(), true))).toList(),
                m.assertions().stream().map(a -> new Assertion(a.span(), anchors(a.exp(), true), a.origin())).toList());
    }

    private List<Element> lowerAnchors(List<Element> elements, boolean topLevel) {
        List<Element> out = new ArrayList<>();
        for (Element e : elements) {
            out.add(switch (e) {
                case Constraint c -> new Constraint(c.span(), anchors(c.exp(), topLevel), c.origin());
                case ClaferDecl c -> withElements(c, lowerAnchors(c.elements(), false));
                case RootBlock r -> new RootBlock(r.span(), r.constraints().stream()
                        .map(c -> new Constraint(c.span(), anchors(c.exp(), true), c.origin())).toList());
                default -> e;
            });
        }
        return out;
    }

    /**
     * The anchors as guards on the birth or the death of the context
     * instance: it exists now, and not in the previous state of its lifetime,
     * or not in the next one.
     */
    private static Exp anchors(Exp exp, boolean topLevel) {
        return transform(exp, e -> {
            if (!(e instanceof Anchor a)) {
                return e;
            }
            SourceSpan span = a.span();
            Exp self = topLevel ? new NameExp(span, "root") : new This(span);
            if (a.initially()) {
                return bin(span, BinaryOp.IMPLIES,
                        new Unary(span, UnaryOp.NOT, tmp(span, TemporalOp.PREVIOUS, some(self))), a.body());
            }
            return bin(span, BinaryOp.IMPLIES,
                    bin(span, BinaryOp.AND, some(self), tmp(span, TemporalOp.X, no(self))), a.body());
        });
    }

    // ------------------------------------------------------------------
    // 8. The multiplicities of the top-level abstract clafers
    // ------------------------------------------------------------------
    private Model lowerTopLevelBounds(Model m) {
        List<Element> out = new ArrayList<>();
        for (Element e : m.elements()) {
            if (!(e instanceof ClaferDecl c) || c.abstractMultiplicity().isEmpty()) {
                out.add(e);
                continue;
            }
            Card bound = c.abstractMultiplicity().get().card();
            SourceSpan span = c.span();
            out.add(new ClaferDecl(c.span(), c.isAbstract(), c.modifiers(), c.groupCard(), c.name(), c.superName(),
                    c.ref(), new Card(bound.span(), 0, Card.Unbounded), Optional.empty(), c.initializer(), c.elements(),
                    c.origin()));
            Exp count = new Unary(span, UnaryOp.CARD, new NameExp(span, c.name().text()));
            List<Exp> comparisons = new ArrayList<>();
            if (bound.low() == bound.high()) {
                comparisons.add(bin(span, BinaryOp.EQ, count, new IntLit(span, bound.low())));
            } else {
                if (bound.low() > 0) {
                    comparisons.add(bin(span, BinaryOp.GE, count, new IntLit(span, bound.low())));
                }
                if (!bound.isUnbounded()) {
                    comparisons.add(bin(span, BinaryOp.LE, count, new IntLit(span, bound.high())));
                }
            }
            if (!comparisons.isEmpty()) {
                Exp p = comparisons.get(comparisons.size() - 1);
                for (int i = comparisons.size() - 2; i >= 0; i--) {
                    p = bin(span, BinaryOp.AND, comparisons.get(i), p);
                }
                out.add(new Constraint(span, p, origin("abstract multiplicity", span)));
            }
        }
        return withElements(m, out);
    }
}
