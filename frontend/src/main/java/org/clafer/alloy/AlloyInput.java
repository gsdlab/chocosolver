package org.clafer.alloy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import org.clafer.alloy.Mirror.IClafer;
import org.clafer.alloy.Mirror.IClaferId;
import org.clafer.alloy.Mirror.IConstraint;
import org.clafer.alloy.Mirror.IDecl;
import org.clafer.alloy.Mirror.IDeclPExp;
import org.clafer.alloy.Mirror.IFunExp;
import org.clafer.alloy.Mirror.IInt;
import org.clafer.alloy.Mirror.IReference;
import org.clafer.alloy.Mirror.IType;
import org.clafer.alloy.Mirror.Interval;
import org.clafer.alloy.Mirror.PExp;
import org.clafer.alloy.Mirror.TClafer;
import org.clafer.alloy.Mirror.TMap;
import org.clafer.alloy.Mirror.TPrim;
import org.clafer.frontend.Ir;
import org.clafer.frontend.Ir.Assertion;
import org.clafer.frontend.Ir.Binary;
import org.clafer.frontend.Ir.BinaryOp;
import org.clafer.frontend.Ir.Card;
import org.clafer.frontend.Ir.ClaferDecl;
import org.clafer.frontend.Ir.Constraint;
import org.clafer.frontend.Ir.Element;
import org.clafer.frontend.Ir.EnumDecl;
import org.clafer.frontend.Ir.Exp;
import org.clafer.frontend.Ir.Join;
import org.clafer.frontend.Ir.Model;
import org.clafer.frontend.Ir.ModifierKind;
import org.clafer.frontend.Ir.NameExp;
import org.clafer.frontend.Ir.Quantified;
import org.clafer.frontend.Ir.RootBlock;
import org.clafer.frontend.ExplicitPrinter;
import org.clafer.frontend.SourceSpan;
import org.clafer.frontend.ToCore;

/**
 * The input of the Alloy generator, as the compiler builds it in
 * {@code Clafer.hs} ({@code generate}) for {@code -m alloy}: from the rich
 * IR, the abstract clafers that nothing needs removed
 * ({@code removeUnusedAbstracts}); its core ({@code lowerToCore}) with the
 * bounds of all the abstract clafers as constraints
 * ({@code lowerAbstractBounds AllBounds}); the strings as integers
 * ({@code astrModule}); the clafers whose UIDs cannot be Alloy names renamed
 * ({@code alloyNames}); and from the rich IR, the static clafers
 * ({@code staticClafers}) and the literals of the enumerations
 * ({@code enumLiterals}). Then the {@link Mirror} that {@link AlloyLtl} reads.
 */
final class AlloyInput {

    /**
     * A construct that the port does not translate: the model goes to the
     * compiler.
     */
    static final class Unsupported extends RuntimeException {

        Unsupported(String message) {
            super(message, null, false, false);
        }
    }

    /**
     * @param names the Alloy name of each clafer of the core, by UID
     * @param strings each string literal's integer, in the order of first
     * occurrence
     */
    record Input(IClafer root, Set<String> statics, Set<String> staticRefs, Set<String> literals,
            List<Map.Entry<String, Long>> scopes, int traceLength, Map<String, String> names,
            Map<String, Integer> strings) {
    }

    /**
     * @param rich the rich explicit IR of a temporal model
     * @param scopes the scopes of the rich IR, by UID, in the order of the
     * UIDs
     * @param traceLength the trace length
     * @param keepUnused {@code -k}: no abstract clafer is removed
     */
    static Input input(Model rich, Map<String, Long> scopes, int traceLength, boolean keepUnused) {
        // what the compiler gives no Alloy output for, anywhere in the model (Clafer.hs)
        unavailable(rich);
        // the nested assertions, which the explicit IR quantifies over their clafer's instances
        List<NestedAssertion> assertions = rich.assertions().stream().map(AlloyInput::unnest).toList();
        Set<String> removed = new HashSet<>();
        Model solved = keepUnused ? rich : removeUnusedAbstracts(rich, assertions, removed);
        Set<String> statics = new LinkedHashSet<>();
        Set<String> staticRefs = new LinkedHashSet<>();
        staticClafers(solved.elements(), true, statics, staticRefs);
        Set<String> literals = new LinkedHashSet<>();
        for (Element e : solved.elements()) {
            if (e instanceof EnumDecl en) {
                en.literals().forEach(l -> literals.add(l.text()));
                // in a temporal model, an enumeration and its literals are final (ToCore.lowerEnums)
                if (rich.options().traceLength().isPresent()) {
                    statics.add(en.name().text());
                    en.literals().forEach(l -> statics.add(l.text()));
                }
            }
        }
        Model core = ToCore.toCore(solved, "model.des.cfr");
        List<Element> elements = lowerNestedBounds(core.elements());
        List<NestedAssertion> coreAssertions = core.assertions().stream().map(AlloyInput::unnest).toList();

        Map<String, String> names = AlloyNames.alloyNames(uids(elements));
        AlloyInput in = new AlloyInput(names);
        in.collect(elements, null);
        IClafer root = new IClafer(AlloyLtl.rootIdent, false, null, null, new Interval(1, 1), new Interval(0, -1), "");
        in.uidMap.put(root.uid, root);
        for (Element e : elements) {
            in.element(e, root);
        }
        for (NestedAssertion a : coreAssertions) {
            IClafer context = a.clafer() == null ? root : in.uidMap.get(in.r(a.clafer()));
            if (context == null) {
                throw new Unsupported("the assertion of a clafer that is not in the model");
            }
            PExp p = in.conv(a.exp(), context.uid, Map.of());
            context.elements.add(new IConstraint(false, new PExp(p.type(), p.pid(), a.span(), p.exp()),
                    Optional.empty()));
        }
        List<Map.Entry<String, Long>> scopeList = new ArrayList<>();
        scopes.forEach((uid, scope) -> {
            // the scope file has every clafer; getScopeStrategy gives the scopes other than 1
            if (!removed.contains(uid) && scope != 1) {
                scopeList.add(Map.entry(in.r(uid), scope));
            }
        });
        return new Input(root, rename(statics, in), rename(staticRefs, in), rename(literals, in), scopeList,
                traceLength, names, in.strings);
    }

    private static Set<String> rename(Set<String> uids, AlloyInput in) {
        Set<String> out = new LinkedHashSet<>();
        uids.forEach(u -> out.add(in.r(u)));
        return out;
    }

    // ----------------------------------------------------------------------
    // the nested assertions
    // ----------------------------------------------------------------------
    /**
     * An assertion, with the clafer it was written in, or null at the top
     * level.
     */
    record NestedAssertion(SourceSpan span, String clafer, Exp exp) {
    }

    private static final String ThisLocal = "this_";

    /**
     * The clafer and body of an assertion that {@code ToIr} quantified over
     * the instances of its clafer ({@code all this_ : A.b | ...}, the
     * quantifier, its declaration and the assertion with one span).
     */
    static NestedAssertion unnest(Assertion a) {
        if (a.exp() instanceof Quantified q && q.quantifier() == Ir.Quantifier.ALL && q.decls().size() == 1
                && q.span().equals(a.span())) {
            Ir.Decl d = q.decls().get(0);
            if (!d.disjoint() && d.locals().size() == 1 && d.locals().get(0).text().equals(ThisLocal)
                    && d.span().equals(a.span())) {
                String clafer = lastName(d.set());
                if (clafer != null) {
                    return new NestedAssertion(a.span(), clafer, localToThis(q.body()));
                }
            }
        }
        return new NestedAssertion(a.span(), null, a.exp());
    }

    private static String lastName(Exp e) {
        return switch (e) {
            case NameExp n -> n.name();
            case Join j when j.right() instanceof NameExp n && lastName(j.left()) != null -> n.name();
            default -> null;
        };
    }

    private static Exp localToThis(Exp e) {
        if (e instanceof NameExp n && n.name().equals(ThisLocal)) {
            return new Ir.This(n.span());
        }
        return mapChildren(e, AlloyInput::localToThis);
    }

    private static Exp mapChildren(Exp e, java.util.function.Function<Exp, Exp> t) {
        return switch (e) {
            case Join x -> new Join(x.span(), t.apply(x.left()), t.apply(x.right()));
            case Ir.Dref x -> new Ir.Dref(x.span(), t.apply(x.left()));
            case Ir.ParentOf x -> new Ir.ParentOf(x.span(), t.apply(x.left()));
            case Ir.Unary x -> new Ir.Unary(x.span(), x.op(), t.apply(x.arg()));
            case Binary x -> new Binary(x.span(), x.op(), x.operands().stream().map(t).toList());
            case Ir.IfThenElse x -> new Ir.IfThenElse(x.span(), t.apply(x.condition()), t.apply(x.then()),
                    t.apply(x.otherwise()));
            case Quantified x -> new Quantified(x.span(), x.quantifier(), x.decls().stream()
                    .map(d -> new Ir.Decl(d.span(), d.disjoint(), d.locals(), t.apply(d.set()))).toList(),
                    t.apply(x.body()));
            case Ir.QuantifiedSet x -> new Ir.QuantifiedSet(x.span(), x.quantifier(), t.apply(x.set()));
            case Ir.Call x -> new Ir.Call(x.span(), x.function(), x.args().stream().map(t).toList());
            case Ir.Temporal x -> new Ir.Temporal(x.span(), x.op(), x.args().stream().map(t).toList());
            case Ir.Anchor x -> new Ir.Anchor(x.span(), x.initially(), t.apply(x.body()));
            case Ir.Pattern x -> new Ir.Pattern(x.span(), x.kind(), x.scope(), x.args().stream().map(t).toList());
            case Ir.Transition x -> new Ir.Transition(x.span(), x.sync(), t.apply(x.from()), x.guard().map(t),
                    t.apply(x.to()));
            case Ir.Cast x -> new Ir.Cast(x.span(), x.up(), t.apply(x.base()), x.type());
            case Ir.Connected x -> new Ir.Connected(x.span(), t.apply(x.nodes()), t.apply(x.relation()), x.directed());
            default -> e;
        };
    }

    /**
     * The constructs for which the compiler gives no Alloy output: a decimal,
     * real or double literal, a reference to a decimal, real or double, and
     * the product operator, in a constraint, goal or assertion.
     */
    private static void unavailable(Model m) {
        List<Exp> exps = new ArrayList<>();
        unavailable(m.elements(), exps);
        m.objectives().forEach(o -> exps.add(o.exp()));
        m.assertions().forEach(a -> exps.add(a.exp()));
        for (Exp e : exps) {
            unavailable(e);
        }
    }

    private static void unavailable(List<Element> elements, List<Exp> exps) {
        for (Element e : elements) {
            switch (e) {
                case Constraint k -> exps.add(k.exp());
                case RootBlock b -> b.constraints().forEach(k -> exps.add(k.exp()));
                case ClaferDecl c -> {
                    c.ref().ifPresent(r -> {
                        switch (r.target().text()) {
                            case "real", "double", "decimal" -> throw new Unsupported("a reference to a "
                                    + r.target().text());
                            default -> {
                            }
                        }
                    });
                    c.initializer().ifPresent(i -> exps.add(i.value()));
                    unavailable(c.elements(), exps);
                }
                default -> {
                }
            }
        }
    }

    private static void unavailable(Exp e) {
        switch (e) {
            case Ir.DecimalLit x -> throw new Unsupported("a decimal literal");
            case Ir.DoubleLit x -> throw new Unsupported("a real or double literal");
            case Ir.Unary u when u.op() == Ir.UnaryOp.PRODUCT -> throw new Unsupported("the product operator");
            default -> mapChildren(e, x -> {
                unavailable(x);
                return x;
            });
        }
    }

    /**
     * The largest integer in the model, as claferIG's
     * {@code findNecessaryBitwidth} finds it: the absolute values of its
     * integer literals and the upper bounds of the cardinalities of its
     * clafers.
     */
    static long maxInModel(Model m) {
        List<Exp> exps = new ArrayList<>();
        long[] max = {0};
        maxCards(m.elements(), exps, max);
        m.objectives().forEach(o -> exps.add(o.exp()));
        m.assertions().forEach(a -> exps.add(a.exp()));
        for (Exp e : exps) {
            maxInts(e, max);
        }
        return max[0];
    }

    private static void maxCards(List<Element> elements, List<Exp> exps, long[] max) {
        for (Element e : elements) {
            switch (e) {
                case Constraint k -> exps.add(k.exp());
                case RootBlock b -> b.constraints().forEach(k -> exps.add(k.exp()));
                case ClaferDecl c -> {
                    max[0] = Math.max(max[0], c.card().high());
                    c.initializer().ifPresent(i -> exps.add(i.value()));
                    maxCards(c.elements(), exps, max);
                }
                default -> {
                }
            }
        }
    }

    private static void maxInts(Exp e, long[] max) {
        if (e instanceof Ir.IntLit i) {
            max[0] = Math.max(max[0], Math.abs(i.value()));
        }
        mapChildren(e, x -> {
            maxInts(x, max);
            return x;
        });
    }

    // ----------------------------------------------------------------------
    // Optimizer.removeUnusedAbstracts
    // ----------------------------------------------------------------------
    /**
     * Removes the top-level abstract clafers that nothing needs: no concrete
     * clafer, constraint or assertion names them or a clafer they lead to,
     * and they have no lower bound.
     */
    private static Model removeUnusedAbstracts(Model m, List<NestedAssertion> assertions, Set<String> removed) {
        List<ClaferDecl> tops = new ArrayList<>();
        for (Element e : m.elements()) {
            if (e instanceof ClaferDecl c) {
                tops.add(c);
            }
        }
        // neededRoots
        List<String> needed = new ArrayList<>();
        for (ClaferDecl c : tops) {
            if (!c.isAbstract()) {
                needed.add(c.name().text());
            }
        }
        collectNames(m.elements(), needed);
        for (NestedAssertion a : assertions) {
            names(a.exp(), needed);
        }
        for (ClaferDecl c : tops) {
            if (c.isAbstract() && c.abstractMultiplicity().map(a -> a.card().low() > 0).orElse(c.card().low() > 0)) {
                needed.add(c.name().text());
            }
        }
        // findUnneededAbs
        List<ClaferDecl> maybeNeeded = tops;
        while (!needed.isEmpty() && !maybeNeeded.isEmpty()) {
            List<ClaferDecl> neededNow = new ArrayList<>();
            List<ClaferDecl> rest = new ArrayList<>();
            for (ClaferDecl c : maybeNeeded) {
                (needed.contains(c.name().text()) ? neededNow : rest).add(c);
            }
            maybeNeeded = rest;
            LinkedHashSet<String> next = new LinkedHashSet<>();
            for (ClaferDecl c : neededNow) {
                getNeeded(c, next);
            }
            needed = new ArrayList<>(next);
        }
        List<ClaferDecl> unneeded = needed.isEmpty() ? maybeNeeded : List.of();
        if (unneeded.isEmpty()) {
            return m;
        }
        Set<ClaferDecl> drop = new HashSet<>(unneeded);
        List<Element> out = new ArrayList<>();
        for (Element e : m.elements()) {
            if (e instanceof ClaferDecl c && drop.contains(c)) {
                universe(c, removed);
            } else {
                out.add(e);
            }
        }
        return new Model(m.span(), m.options(), out, m.objectives(), m.assertions());
    }

    private static void universe(ClaferDecl c, Set<String> out) {
        out.add(c.name().text());
        for (Element e : c.elements()) {
            if (e instanceof ClaferDecl d) {
                universe(d, out);
            }
        }
    }

    private static void getNeeded(ClaferDecl c, Set<String> out) {
        c.superName().ifPresent(s -> out.add(s.text()));
        c.ref().ifPresent(r -> out.add(r.target().text()));
        for (Element e : c.elements()) {
            if (e instanceof ClaferDecl d) {
                getNeeded(d, out);
            }
        }
    }

    private static void collectNames(List<Element> elements, List<String> out) {
        for (Element e : elements) {
            switch (e) {
                case Constraint k -> names(k.exp(), out);
                case ClaferDecl c -> collectNames(c.elements(), out);
                case RootBlock b -> b.constraints().forEach(k -> names(k.exp(), out));
                default -> {
                }
            }
        }
    }

    private static void names(Exp e, List<String> out) {
        switch (e) {
            case NameExp n -> out.add(n.name());
            case Ir.RelationOf r -> out.add(r.clafer().text());
            case Ir.Cast c -> {
                c.type().forEach(n -> out.add(n.text()));
                names(c.base(), out);
            }
            default -> {
                List<Exp> children = new ArrayList<>();
                mapChildren(e, x -> {
                    children.add(x);
                    return x;
                });
                children.forEach(x -> names(x, out));
            }
        }
    }

    // ----------------------------------------------------------------------
    // Finality.staticClafers
    // ----------------------------------------------------------------------
    private static void staticClafers(List<Element> elements, boolean parentStatic, Set<String> statics,
            Set<String> staticRefs) {
        for (Element e : elements) {
            if (e instanceof ClaferDecl c) {
                boolean isStatic = parentStatic && c.modifiers().stream().anyMatch(m -> m.kind() == ModifierKind.FINAL);
                if (isStatic) {
                    statics.add(c.name().text());
                    // the compiler gives a final clafer's own reference its modifiers too
                    if (c.ref().isPresent()) {
                        staticRefs.add(c.name().text());
                    }
                }
                staticClafers(c.elements(), isStatic, statics, staticRefs);
            }
        }
    }

    // ----------------------------------------------------------------------
    // AbstractMultiplicity.lowerAbstractBounds AllBounds, the nested bounds
    // ----------------------------------------------------------------------
    /**
     * The bounds of the nested abstract clafers as constraints next to them,
     * {@code #this.A} in their parent; {@code ToCore} has lowered the
     * top-level ones.
     */
    private static List<Element> lowerNestedBounds(List<Element> elements) {
        List<Element> out = new ArrayList<>();
        for (Element e : elements) {
            if (e instanceof ClaferDecl c) {
                out.add(lowerIn(c));
            } else {
                out.add(e);
            }
        }
        return out;
    }

    private static ClaferDecl lowerIn(ClaferDecl c) {
        List<Element> out = new ArrayList<>();
        for (Element e : c.elements()) {
            if (e instanceof ClaferDecl d) {
                ClaferDecl lowered = lowerIn(d);
                Card b = d.card();
                if (d.isAbstract() && !b.isAny()) {
                    out.add(withCard(lowered, new Card(b.span(), 0, Card.Unbounded)));
                    boundOf(d, b).ifPresent(p -> out.add(new Constraint(d.span(), p,
                            Optional.of(new Ir.Origin("abstract " + b, Optional.empty())))));
                } else {
                    out.add(lowered);
                }
            } else {
                out.add(e);
            }
        }
        return new ClaferDecl(c.span(), c.isAbstract(), c.modifiers(), c.groupCard(), c.name(), c.superName(),
                c.ref(), c.card(), c.abstractMultiplicity(), c.initializer(), out, c.origin());
    }

    private static ClaferDecl withCard(ClaferDecl c, Card card) {
        return new ClaferDecl(c.span(), c.isAbstract(), c.modifiers(), c.groupCard(), c.name(), c.superName(),
                c.ref(), card, c.abstractMultiplicity(), c.initializer(), c.elements(), c.origin());
    }

    private static Optional<Exp> boundOf(ClaferDecl c, Card b) {
        SourceSpan s = c.span();
        Exp count = new Ir.Unary(s, Ir.UnaryOp.CARD, new Join(s, new Ir.This(s), new NameExp(s, c.name().text())));
        List<Exp> ps = new ArrayList<>();
        if (b.low() == b.high()) {
            ps.add(new Binary(s, BinaryOp.EQ, List.of(count, new Ir.IntLit(s, b.low()))));
        } else {
            if (b.low() > 0) {
                ps.add(new Binary(s, BinaryOp.GE, List.of(count, new Ir.IntLit(s, b.low()))));
            }
            if (b.high() >= 0) {
                ps.add(new Binary(s, BinaryOp.LE, List.of(count, new Ir.IntLit(s, b.high()))));
            }
        }
        if (ps.isEmpty()) {
            return Optional.empty();
        }
        Exp p = ps.get(ps.size() - 1);
        for (int i = ps.size() - 2; i >= 0; i--) {
            p = new Binary(s, BinaryOp.AND, List.of(ps.get(i), p));
        }
        return Optional.of(p);
    }

    // ----------------------------------------------------------------------
    // the mirror
    // ----------------------------------------------------------------------
    private final Map<String, String> names;
    private final Map<String, IClafer> uidMap = new HashMap<>();
    /**
     * The clafers of the core by their UIDs, for the types.
     */
    private final Map<String, ClaferDecl> decls = new HashMap<>();
    private final Map<String, String> parents = new HashMap<>();
    /**
     * {@code astrModule}: each string's integer, in the order of first
     * occurrence.
     */
    private final Map<String, Integer> strings = new LinkedHashMap<>();

    private AlloyInput(Map<String, String> names) {
        this.names = names;
    }

    private String r(String uid) {
        return names.getOrDefault(uid, uid);
    }

    private static List<String> uids(List<Element> elements) {
        List<String> out = new ArrayList<>();
        for (Element e : elements) {
            if (e instanceof ClaferDecl c) {
                out.add(c.name().text());
                out.addAll(uids(c.elements()));
            }
        }
        return out;
    }

    private void collect(List<Element> elements, String parent) {
        for (Element e : elements) {
            if (e instanceof ClaferDecl c) {
                decls.put(c.name().text(), c);
                parents.put(c.name().text(), parent == null ? AlloyLtl.rootIdent : parent);
                collect(c.elements(), c.name().text());
            }
        }
    }

    private void element(Element e, IClafer parent) {
        switch (e) {
            case ClaferDecl c -> {
                String uid = c.name().text();
                IReference ref = c.ref().map(x -> new IReference(x.isSet(), target(x.target().text()), null))
                        .orElse(null);
                // a reference to a set expression: the explicit IR has a reference to the common type and
                // its constraints first in the clafer, with the reference's span; the compiler's IR has
                // the expression, whose signatures the field's type names (refType)
                List<Element> elements = new ArrayList<>(c.elements());
                if (c.ref().isPresent()) {
                    SourceSpan refSpan = c.ref().get().span();
                    while (!elements.isEmpty() && elements.get(0) instanceof Constraint k && k.origin().isEmpty()
                            && k.span().equals(refSpan)) {
                        Exp target = referenceTarget(k.exp());
                        if (target == null) {
                            throw new Unsupported("the reference constraint " + ExplicitPrinter.print(k.exp()));
                        }
                        ref = new IReference(ref.isSet(), ref.target(), conv(target, r(uid), Map.of()));
                        elements.remove(0);
                    }
                }
                IClafer ic = new IClafer(r(uid), c.isAbstract(), c.superName().map(n -> r(n.text())).orElse(null), ref,
                        interval(c.card()), interval(c.groupCard()), parent.uid);
                uidMap.put(ic.uid, ic);
                parent.elements.add(ic);
                for (Element x : elements) {
                    element(x, ic);
                }
            }
            case Constraint k -> {
                Exp x = k.origin().map(o -> List.of("final", "finalref", "finaltarget").contains(o.construct()))
                        .orElse(false) ? rootJoins(k.exp(), Set.of()) : k.exp();
                parent.elements.add(new IConstraint(true, conv(x, parent.uid, Map.of()), k.origin()));
            }
            case RootBlock b -> b.constraints().forEach(k -> element(k, parent));
            case EnumDecl en -> throw new Unsupported("an enumeration in the core");
        }
    }

    /**
     * The compiler's finality constraints name the instances of a top-level
     * clafer as {@code root.x} ({@code Finality.siblings}), which the
     * explicit IR writes as {@code x}.
     */
    private Exp rootJoins(Exp e, Set<String> locals) {
        if (e instanceof NameExp n && !locals.contains(n.name())
                && AlloyLtl.rootIdent.equals(parents.get(n.name()))) {
            return new Join(n.span(), new NameExp(n.span(), AlloyLtl.rootIdent), n);
        }
        if (e instanceof Join j) {
            return new Join(j.span(), rootJoins(j.left(), locals), j.right());
        }
        if (e instanceof Quantified q) {
            Set<String> inner = new HashSet<>(locals);
            List<Ir.Decl> ds = new ArrayList<>();
            for (Ir.Decl d : q.decls()) {
                ds.add(new Ir.Decl(d.span(), d.disjoint(), d.locals(), rootJoins(d.set(), inner)));
                d.locals().forEach(l -> inner.add(l.text()));
            }
            return new Quantified(q.span(), q.quantifier(), ds, rootJoins(q.body(), inner));
        }
        return mapChildren(e, x -> rootJoins(x, locals));
    }

    /**
     * The set expression of a reference, from the constraint that
     * {@code ToIr} writes for it ({@code Resolver.referenceConstraints}):
     * {@code this.dref in e}; for numbers {@code this.dref = 1 || ...}, and
     * {@code !(this.dref = n)} for {@code integer -- n}.
     */
    private static Exp referenceTarget(Exp e) {
        if (e instanceof Binary b && b.op() == BinaryOp.IN && isThisDref(b.operands().get(0))) {
            return b.operands().get(1);
        }
        if (e instanceof Ir.Unary u && u.op() == Ir.UnaryOp.NOT && u.arg() instanceof Binary b
                && b.op() == BinaryOp.EQ && isThisDref(b.operands().get(0))) {
            return new Binary(e.span(), BinaryOp.DIFF, List.of(new NameExp(e.span(), "integer"), b.operands().get(1)));
        }
        return values(e);
    }

    private static Exp values(Exp e) {
        if (e instanceof Binary b && b.op() == BinaryOp.EQ && isThisDref(b.operands().get(0))) {
            return b.operands().get(1);
        }
        if (e instanceof Binary b && b.op() == BinaryOp.OR && b.operands().size() == 2) {
            Exp l = values(b.operands().get(0));
            Exp r = values(b.operands().get(1));
            return l == null || r == null ? null : new Binary(e.span(), BinaryOp.UNION, List.of(l, r));
        }
        return null;
    }

    private static boolean isThisDref(Exp e) {
        return e instanceof Ir.Dref d && d.left() instanceof Ir.This;
    }

    private String target(String t) {
        return AlloyLtl.isPrimitive(t) ? t : r(t);
    }

    private static Interval interval(Card c) {
        return new Interval(c.low(), c.isUnbounded() ? -1 : c.high());
    }

    // the types, by the UIDs of the core (before renaming)
    private String original(String alloyName) {
        for (Map.Entry<String, String> e : names.entrySet()) {
            if (e.getValue().equals(alloyName)) {
                return e.getKey();
            }
        }
        return alloyName;
    }

    private IType clafer(String uid) {
        return new TClafer(List.of(uid));
    }

    /**
     * The target type of the reference of the clafer (Alloy name), declared
     * or inherited.
     */
    private IType refTarget(String alloyName) {
        String uid = original(alloyName);
        Set<String> seen = new HashSet<>();
        while (uid != null && seen.add(uid)) {
            ClaferDecl c = decls.get(uid);
            if (c == null) {
                break;
            }
            if (c.ref().isPresent()) {
                String t = c.ref().get().target().text();
                return switch (t) {
                    case "int", "integer" -> TPrim.TInteger;
                    case "string" -> TPrim.TInteger;
                    case "real", "double", "decimal" -> throw new Unsupported("a reference to " + t);
                    default -> clafer(r(t));
                };
            }
            uid = c.superName().map(Ir.Name::text).orElse(null);
        }
        throw new Unsupported("dref of " + alloyName + ", which has no reference");
    }

    private String owner(String alloyName) {
        String uid = original(alloyName);
        Set<String> seen = new HashSet<>();
        while (uid != null && seen.add(uid)) {
            ClaferDecl c = decls.get(uid);
            if (c == null) {
                break;
            }
            if (c.ref().isPresent()) {
                return r(uid);
            }
            uid = c.superName().map(Ir.Name::text).orElse(null);
        }
        return alloyName;
    }

    private String parentOf(String alloyName) {
        String p = parents.get(original(alloyName));
        return p == null || p.equals(AlloyLtl.rootIdent) ? AlloyLtl.rootIdent : r(p);
    }

    private static String head(IType t) {
        return t instanceof TClafer c && !c.hi().isEmpty() ? c.hi().get(0) : null;
    }

    /**
     * The compiler's typed expression of an explicit expression in the
     * context of a clafer (Alloy name).
     */
    private PExp conv(Exp e, String ctx, Map<String, IType> locals) {
        SourceSpan s = e.span();
        return switch (e) {
            case Ir.This x -> new PExp(clafer(ctx), x, s, new IClaferId(AlloyLtl.thisIdent, true, false));
            case NameExp x -> name(x, x.name(), locals);
            // the compiler keeps a negative number as the negation of a literal, -(1)
            case Ir.IntLit x when x.value() < 0 -> new PExp(TPrim.TInteger, x, s, new IFunExp("-",
                    List.of(new PExp(TPrim.TInteger, x, s, new IInt(-x.value())))));
            case Ir.IntLit x -> new PExp(TPrim.TInteger, x, s, new IInt(x.value()));
            case Ir.StringLit x -> {
                Integer i = strings.get(x.text());
                if (i == null) {
                    i = strings.size();
                    strings.put(x.text(), i);
                }
                yield new PExp(TPrim.TInteger, x, s, new IInt(i));
            }
            case Ir.NoClafer x -> new PExp(new TClafer(List.of()), x, s, new IClaferId("noclafer", true, false));
            case Join x -> {
                PExp l = conv(x.left(), ctx, locals);
                PExp r;
                if (x.right() instanceof NameExp n && !locals.containsKey(n.name())) {
                    String u = r(n.name());
                    // root.x: the compiler's x is a top-level name
                    boolean top = x.left() instanceof NameExp ln && ln.name().equals(AlloyLtl.rootIdent)
                            && !locals.containsKey(ln.name());
                    r = new PExp(clafer(u), n, n.span(), new IClaferId(u, top, false));
                } else {
                    r = conv(x.right(), ctx, locals);
                }
                yield new PExp(r.type(), x, s, new IFunExp(".", List.of(l, r)));
            }
            case Ir.Dref x -> {
                PExp l = conv(x.left(), ctx, locals);
                yield dref(x, l);
            }
            case Ir.ParentOf x -> {
                PExp l = conv(x.left(), ctx, locals);
                String c = head(l.type());
                IType t = c == null ? null : clafer(parentOf(c));
                yield new PExp(t, x, s, new IFunExp(".", List.of(l,
                        new PExp(t, x, s, new IClaferId(AlloyLtl.parentIdent, false, false)))));
            }
            case Ir.Unary x -> {
                PExp a = conv(x.arg(), ctx, locals);
                yield switch (x.op()) {
                    case NOT -> new PExp(TPrim.TBoolean, x, s, new IFunExp("!", List.of(a)));
                    case CARD -> new PExp(TPrim.TInteger, x, s, new IFunExp("#", List.of(a)));
                    case MINUS -> new PExp(TPrim.TInteger, x, s, new IFunExp("-", List.of(a)));
                    case MAX -> new PExp(TPrim.TInteger, x, s, new IFunExp("max", List.of(a)));
                    case MIN -> new PExp(TPrim.TInteger, x, s, new IFunExp("min", List.of(a)));
                    case SUM -> new PExp(TPrim.TInteger, x, s, new IFunExp("sum", List.of(dref(x, a))));
                    case PRODUCT -> throw new Unsupported("the product operator");
                    default -> throw new Unsupported("the operator " + x.op().syntax().trim());
                };
            }
            case Binary x -> binary(x, ctx, locals);
            case Ir.IfThenElse x -> {
                PExp c = conv(x.condition(), ctx, locals);
                PExp a = conv(x.then(), ctx, locals);
                PExp b = conv(x.otherwise(), ctx, locals);
                IType t = a.type() == TPrim.TBoolean ? TPrim.TBoolean : a.type();
                yield new PExp(t, x, s, new IFunExp("ifthenelse", List.of(c, a, b)));
            }
            case Quantified x -> {
                Map<String, IType> inner = new HashMap<>(locals);
                List<IDecl> ds = new ArrayList<>();
                for (Ir.Decl d : x.decls()) {
                    PExp set = conv(d.set(), ctx, inner);
                    List<String> ls = d.locals().stream().map(Ir.Name::text).toList();
                    ds.add(new IDecl(d.disjoint(), ls, set));
                    ls.forEach(l -> inner.put(l, set.type()));
                }
                yield new PExp(TPrim.TBoolean, x, s, new IDeclPExp(x.quantifier(), ds, conv(x.body(), ctx, inner)));
            }
            case Ir.QuantifiedSet x -> new PExp(TPrim.TBoolean, x, s,
                    new IDeclPExp(x.quantifier(), List.of(), conv(x.set(), ctx, locals)));
            case Ir.Temporal x -> new PExp(TPrim.TBoolean, x, s, new IFunExp(x.op().keyword(),
                    x.args().stream().map(a -> conv(a, ctx, locals)).toList()));
            case Ir.DecimalLit x -> throw new Unsupported("a decimal literal");
            case Ir.DoubleLit x -> throw new Unsupported("a real or double literal");
            case Ir.Call x -> throw new Unsupported("the function " + x.function());
            case Ir.RelationOf x -> throw new Unsupported("a relation of a clafer");
            case Ir.Cast x -> throw new Unsupported("a cast");
            case Ir.Connected x -> throw new Unsupported("connected");
            case Ir.ConstantSet x -> throw new Unsupported("a constant set");
            case Ir.Anchor x -> throw new Unsupported("an anchor in the core");
            case Ir.Pattern x -> throw new Unsupported("a pattern in the core");
            case Ir.Transition x -> throw new Unsupported("a transition in the core");
        };
    }

    private PExp name(Exp x, String n, Map<String, IType> locals) {
        SourceSpan s = x.span();
        if (locals.containsKey(n)) {
            return new PExp(locals.get(n), x, s, new IClaferId(n, false, true));
        }
        return switch (n) {
            case "root" -> new PExp(clafer(AlloyLtl.rootIdent), x, s, new IClaferId(n, true, false));
            case "int", "integer" -> new PExp(TPrim.TInteger, x, s, new IClaferId(n, true, false));
            case "string" -> new PExp(TPrim.TString, x, s, new IClaferId(n, true, false));
            default -> {
                String u = r(n);
                yield new PExp(clafer(u), x, s, new IClaferId(u, true, false));
            }
        };
    }

    private PExp dref(Exp x, PExp l) {
        String c = head(l.type());
        if (c == null) {
            throw new Unsupported("dref of an expression of type " + l.type());
        }
        IType ta = refTarget(c);
        // the clafer that declares the reference, which the type of dref names (Finality.owner)
        PExp d = new PExp(new TMap(clafer(owner(c)), ta), x, x.span(), new IClaferId("dref", true, false));
        return new PExp(ta, x, x.span(), new IFunExp(".", List.of(l, d)));
    }

    private PExp binary(Binary x, String ctx, Map<String, IType> locals) {
        String op = switch (x.op()) {
            case ADD -> "+";
            case SUB -> "-";
            case MUL -> "*";
            case DIV -> "/";
            case MOD -> "%";
            case AND -> "&&";
            case OR -> "||";
            case IMPLIES -> "=>";
            case IFF -> "<=>";
            case XOR -> "xor";
            case LT -> "<";
            case LE -> "<=";
            case GT -> ">";
            case GE -> ">=";
            case EQ -> "=";
            case NE -> "!=";
            case IN -> "in";
            case NOT_IN -> "not in";
            case UNION -> "++";
            case DIFF -> "--";
            case INTER -> "**";
            case DOMAIN -> "<:";
            case RANGE -> ":>";
            case DECIMAL_MUL, DECIMAL_DIV -> throw new Unsupported("a decimal operator");
        };
        List<PExp> args = x.operands().stream().map(a -> conv(a, ctx, locals)).toList();
        PExp acc = args.get(0);
        for (int i = 1; i < args.size(); i++) {
            PExp b = args.get(i);
            IType t = switch (x.op()) {
                case ADD, SUB, MUL, DIV, MOD -> TPrim.TInteger;
                case UNION, DIFF, INTER, DOMAIN, RANGE -> acc.type();
                default -> TPrim.TBoolean;
            };
            acc = new PExp(t, x, x.span(), new IFunExp(op, List.of(acc, b)));
        }
        return acc;
    }
}
