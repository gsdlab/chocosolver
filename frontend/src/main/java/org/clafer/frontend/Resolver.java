package org.clafer.frontend;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.UnaryOperator;
import org.clafer.frontend.Symbols.ClaferSymbol;
import org.clafer.frontend.Symbols.Interval;

/**
 * Step 10: the names and types of the expressions of a model, resolved as the
 * compiler resolves them. Each constraint, objective, assertion, initial
 * value and set-expression reference goes through the compiler's stages, each
 * a port of its function of the same name:
 * <ol>
 * <li>the desugarer ({@code Desugarer.hs}: {@code desugarExp},
 * {@code desugarPath}, {@code reduceNav}, {@code desugarLet});</li>
 * <li>the name resolver ({@code ResolverName.hs});</li>
 * <li>the type checker ({@code ResolverType.hs}, {@code TypeSystem.hs}),
 * which tries the alternatives of each node in the compiler's order and keeps
 * the first that type checks;</li>
 * <li>the root's rewrites ({@code ResolverRoot.hs}), in static models;</li>
 * <li>the expression of the explicit model ({@code ChocoModel.hs}:
 * {@code rewrite}, {@code genConstraintExp}).</li>
 * </ol>
 * The stages work on a mirror of the compiler's intermediate expressions
 * ({@link P}, {@link E}), since the rules are written against their shape.
 * The first error of an expression is the compiler's, at its position; the
 * other expressions are still resolved.
 */
final class Resolver {

    // ======================================================================
    // Types (TypeSystem.hs)
    // ======================================================================
    sealed interface Type permits Prim, TClafer, TMap, TUnion {
    }

    enum Prim implements Type {
        TBoolean, TString, TInteger, TReal, TDouble, TDecimal
    }

    record TClafer(List<String> hi) implements Type {

        TClafer {
            hi = List.copyOf(hi);
        }
    }

    record TMap(Type so, Type ta) implements Type {
    }

    record TUnion(List<Type> un) implements Type {

        TUnion {
            un = List.copyOf(un);
        }
    }

    private static final String RootIdent = "root";
    private static final String BaseClafer = "clafer";
    private static final String NoClaferIdent = "noclafer";
    private static final TClafer RootT = new TClafer(List.of(RootIdent));
    private static final TClafer ClaferT = new TClafer(List.of(BaseClafer));
    private static final TClafer NoClaferT = new TClafer(List.of(NoClaferIdent));

    private static final Set<String> SpecialNames = Set.of("this", "parent", "dref", "root", "children", "noclafer");
    private static final Set<String> PrimitiveTypes = Set.of("string", "int", "integer", "decimal", "double", "real");
    private static final Set<String> LogBinOps = Set.of("<=>", "=>", "||", "xor", "&&", "U", "W", "since", "triggered");
    private static final Set<String> LtlBinOps = Set.of("W", "U", "since", "triggered");
    private static final Set<String> LtlUnOps = Set.of("X", "G", "F", "initially", "finally", "previous", "once",
            "historically");
    private static final Set<String> SetBinOps = Set.of("++", "--", "**", "<:", ":>", ".");
    private static final Set<String> TransitionOps = Set.of("-->", "-[]->", "-->>", "-[]->>");
    private static final List<String> PatternKinds = List.of("never", "sometime", "lonce", "always", "precede", "follow");
    private static final List<String> PatternScopes = List.of("", "before", "after", "between", "after until");
    private static final Set<String> PatternOps = patternOps();

    private static Set<String> patternOps() {
        Set<String> ops = new LinkedHashSet<>();
        for (String k : PatternKinds) {
            for (String s : PatternScopes) {
                ops.add(patternOp(k, s));
            }
        }
        return Set.copyOf(ops);
    }

    private static String patternOp(String kind, String scope) {
        return scope.isEmpty() ? kind : kind + " " + scope;
    }

    private static boolean isRichTemporal(String op) {
        return PatternOps.contains(op) || TransitionOps.contains(op);
    }

    static String show(Type t) {
        return switch (t) {
            case Prim p -> p.name();
            case TClafer c -> "TClafer {_hi = [" + String.join(",", c.hi().stream().map(s -> "\"" + s + "\"").toList())
                    + "]}";
            case TMap m -> "TMap {_so = " + show(m.so()) + ", _ta = " + show(m.ta()) + "}";
            case TUnion u -> "TUnion {_un = [" + String.join(",", u.un().stream().map(Resolver::show).toList()) + "]}";
        };
    }

    private static String showType(P p) {
        return p.type() == null ? "unknown type" : show(p.type());
    }

    private static boolean numeric(Type t) {
        return switch (t) {
            case Prim p -> p == Prim.TInteger || p == Prim.TDecimal || p == Prim.TReal || p == Prim.TDouble;
            case TMap m -> numeric(m.ta());
            case TUnion u -> u.un().stream().anyMatch(Resolver::numeric);
            default -> false;
        };
    }

    private static boolean allNumeric(Type t) {
        return switch (t) {
            case TMap m -> allNumeric(m.ta());
            case TUnion u -> u.un().stream().allMatch(Resolver::allNumeric);
            default -> numeric(t);
        };
    }

    private static boolean is(Type t, Prim p) {
        return switch (t) {
            case Prim q -> q == p;
            case TMap m -> is(m.ta(), p);
            case TUnion u -> u.un().stream().anyMatch(x -> is(x, p));
            default -> false;
        };
    }

    private static boolean isTBoolean(Type t) {
        return is(t, Prim.TBoolean);
    }

    private static List<String> unionType(Type t) {
        return switch (t) {
            case Prim p -> List.of(switch (p) {
                case TString -> "string";
                case TReal -> "real";
                case TDecimal -> "decimal";
                case TDouble -> "double";
                case TInteger -> "integer";
                case TBoolean -> "boolean";
            });
            case TClafer c -> c.hi();
            case TUnion u -> u.un().stream().flatMap(x -> unionType(x).stream()).toList();
            case TMap m -> unionType(m.ta());
        };
    }

    private static Type fromUnionType(List<String> u) {
        List<String> n = nub(u);
        if (n.size() == 1) {
            switch (n.get(0)) {
                case "string":
                    return Prim.TString;
                case "integer", "int":
                    return Prim.TInteger;
                case "decimal":
                    return Prim.TDecimal;
                case "double":
                    return Prim.TDouble;
                case "real":
                    return Prim.TReal;
                default:
                    break;
            }
        }
        return n.isEmpty() ? null : new TClafer(n);
    }

    private static <T> List<T> nub(List<T> xs) {
        return List.copyOf(new LinkedHashSet<>(xs));
    }

    /**
     * The union of two types ({@code +++}).
     */
    static Type union(Type t1, Type t2) {
        if (t1.equals(NoClaferT)) {
            return t2;
        }
        if (t2.equals(NoClaferT)) {
            return t1;
        }
        if (t1 instanceof Prim p1 && t2 instanceof Prim p2) {
            if (p1 == p2 && p1 != Prim.TBoolean || p1 == Prim.TBoolean && p2 == Prim.TBoolean) {
                return t1;
            }
            if (p1 == Prim.TDecimal && p2 == Prim.TInteger || p1 == Prim.TInteger && p2 == Prim.TDecimal) {
                return Prim.TDecimal;
            }
        }
        if (t1 instanceof TClafer c1 && t2 instanceof TClafer c2) {
            if (t1.equals(t2)) {
                return t1;
            }
            List<String> u = new ArrayList<>(c1.hi());
            u.addAll(c2.hi());
            return new TClafer(nub(u));
        }
        if (t1 instanceof TMap m1 && t2 instanceof TMap m2) {
            return new TMap(union(m1.so(), m2.so()), union(m1.ta(), m2.ta()));
        }
        if (t1 instanceof TMap m1) {
            return union(m1.ta(), t2);
        }
        if (t2 instanceof TMap m2) {
            return union(t1, m2.ta());
        }
        if (t1 instanceof TUnion u1 && t2 instanceof TUnion u2) {
            List<Type> u = new ArrayList<>(u1.un());
            u.addAll(u2.un());
            return collapse(new TUnion(nub(u)));
        }
        if (t1 instanceof TUnion u1) {
            List<Type> u = new ArrayList<>(u1.un());
            u.add(t2);
            return collapse(new TUnion(nub(u)));
        }
        if (t2 instanceof TUnion u2) {
            List<Type> u = new ArrayList<>();
            u.add(t1);
            u.addAll(u2.un());
            return collapse(new TUnion(nub(u)));
        }
        return t1.equals(t2) ? t1 : new TUnion(List.of(t1, t2));
    }

    private static Type collapse(TUnion u) {
        return u.un().size() == 1 ? u.un().get(0) : u;
    }

    /**
     * The common type of two primitives, as {@code intersection} and
     * {@code getIfThenElseType} have it, or null.
     */
    private static Prim primMeet(Prim a, Prim b) {
        if (a == b) {
            return a;
        }
        Set<Prim> s = Set.of(a, b);
        if (s.contains(Prim.TBoolean) || s.contains(Prim.TString)) {
            return null;
        }
        if (s.equals(Set.of(Prim.TDecimal, Prim.TInteger))) {
            return Prim.TDecimal;
        }
        if (s.equals(Set.of(Prim.TDouble, Prim.TInteger))) {
            return Prim.TDouble;
        }
        // with real, or decimal with double
        return Prim.TReal;
    }

    private Optional<Type> intersection(Type t1, Type t2) {
        if (t1 instanceof Prim a && t2 instanceof Prim b) {
            return Optional.ofNullable(primMeet(a, b));
        }
        if (t2.equals(ClaferT)) {
            return Optional.of(t1);
        }
        if (t1.equals(ClaferT)) {
            return Optional.of(t2);
        }
        if (t1.equals(NoClaferT) && (t2 instanceof TClafer || t2 instanceof TUnion)) {
            return Optional.of(t1);
        }
        if (t2.equals(NoClaferT) && (t1 instanceof TClafer || t1 instanceof TUnion)) {
            return Optional.of(t2);
        }
        if (t1 instanceof TUnion u1 && t2 instanceof TClafer) {
            return unionOf(u1.un().stream().map(t -> intersection(t2, t)).toList());
        }
        if (t1 instanceof TClafer && t2 instanceof TUnion u2) {
            return unionOf(u2.un().stream().map(t -> intersection(t1, t)).toList());
        }
        if (t1 instanceof TClafer c1 && t2 instanceof TClafer c2) {
            if (c1.hi().equals(c2.hi())) {
                return Optional.of(t1);
            }
            List<List<String>> h1 = c1.hi().stream().map(this::hierarchyUids).toList();
            List<List<String>> h2 = c2.hi().stream().map(this::hierarchyUids).toList();
            List<String> common = new ArrayList<>();
            for (List<String> u1 : h1) {
                for (List<String> u2 : h2) {
                    if (u2.contains(u1.get(0))) {
                        common.add(u1.get(0));
                    } else if (u1.contains(u2.get(0))) {
                        common.add(u2.get(0));
                    }
                }
            }
            return Optional.ofNullable(fromUnionType(common));
        }
        if (t1 instanceof TMap m1 && t2 instanceof TMap m2) {
            return intersection(m1.ta(), m2.ta());
        }
        if (t1 instanceof TMap m1) {
            Optional<Type> coerced = intersection(m1.ta(), t2);
            return coerced.equals(Optional.of(m1.ta())) ? coerced : Optional.empty();
        }
        if (t2 instanceof TMap m2) {
            Optional<Type> coerced = intersection(t1, m2.ta());
            return coerced.equals(Optional.of(m2.ta())) ? coerced : Optional.empty();
        }
        return Optional.empty();
    }

    private static Optional<Type> unionOf(List<Optional<Type>> ts) {
        List<Type> present = ts.stream().flatMap(Optional::stream).toList();
        return present.isEmpty() ? Optional.empty()
                : present.size() == 1 ? Optional.of(present.get(0)) : Optional.of(new TUnion(present));
    }

    private Optional<Type> ifThenElseType(Type t1, Type t2) {
        if (t1 instanceof Prim a && t2 instanceof Prim b) {
            return Optional.ofNullable(primMeet(a, b));
        }
        if (t1.equals(NoClaferT) && (t2 instanceof TClafer || t2 instanceof TUnion)) {
            return Optional.of(t2);
        }
        if (t2.equals(NoClaferT) && (t1 instanceof TClafer || t1 instanceof TUnion)) {
            return Optional.of(t1);
        }
        if (t1 instanceof TUnion u1) {
            return combineAll(u1.un().stream().map(t -> ifThenElseType(t, t2)).toList());
        }
        if (t2 instanceof TUnion u2) {
            return combineAll(u2.un().stream().map(t -> ifThenElseType(t1, t)).toList());
        }
        if (t1 instanceof TMap m1) {
            return ifThenElseType(m1.ta(), t2);
        }
        if (t2 instanceof TMap m2) {
            return ifThenElseType(t1, m2.ta());
        }
        if (t1 instanceof TClafer c1 && t2 instanceof TClafer c2) {
            if (c1.hi().equals(c2.hi())) {
                return Optional.of(t1);
            }
            List<String> ut = new ArrayList<>();
            for (String a : c1.hi()) {
                for (String b : c2.hi()) {
                    List<String> x = new ArrayList<>(hierarchyUids(a));
                    List<String> y = new ArrayList<>(hierarchyUids(b));
                    java.util.Collections.reverse(x);
                    java.util.Collections.reverse(y);
                    String acc = null;
                    for (int i = 0; i < Math.min(x.size(), y.size()); i++) {
                        if (!x.get(i).equals(y.get(i))) {
                            break;
                        }
                        acc = x.get(i);
                    }
                    if (acc != null) {
                        ut.add(acc);
                    }
                }
            }
            return Optional.ofNullable(fromUnionType(ut));
        }
        return Optional.empty();
    }

    private static Optional<Type> combineAll(List<Optional<Type>> ts) {
        if (ts.isEmpty() || ts.stream().anyMatch(Optional::isEmpty)) {
            return Optional.empty();
        }
        Type acc = ts.get(ts.size() - 1).get();
        for (int i = ts.size() - 2; i >= 0; i--) {
            acc = union(ts.get(i).get(), acc);
        }
        return Optional.of(acc);
    }

    private static Type coerce(Type a, Type b) {
        if (a instanceof TMap m) {
            return coerce(m.ta(), b);
        }
        if (b instanceof TMap m) {
            return coerce(a, m.ta());
        }
        if (a instanceof TUnion u) {
            return coerce(foldCoerce(u.un()), b);
        }
        if (b instanceof TUnion u) {
            return coerce(a, foldCoerce(u.un()));
        }
        if (a instanceof Prim p && b instanceof Prim q) {
            Prim r = primMeet(p, q);
            if (r != null) {
                return r;
            }
        }
        throw new IllegalStateException("TypeSystem.coerce: Cannot coerce not numeric: " + show(a) + " and " + show(b));
    }

    private static Type foldCoerce(List<Type> ts) {
        Type acc = ts.get(ts.size() - 1);
        for (int i = ts.size() - 2; i >= 0; i--) {
            acc = coerce(ts.get(i), acc);
        }
        return acc;
    }

    // ======================================================================
    // The clafers, as the compiler's IClafer after inheritance
    // ======================================================================
    static final class C {

        final String uid;
        final String ident;
        final String parentUid;
        final boolean isAbstract;
        final String superUid;
        final List<String> refIds;
        final List<C> children = new ArrayList<>();
        final ClaferSymbol symbol;
        final Interval card;
        Type refType;
        boolean refTyped;

        C(String uid, String ident, String parentUid, boolean isAbstract, String superUid, List<String> refIds,
                ClaferSymbol symbol, Interval card) {
            this.uid = uid;
            this.ident = ident;
            this.parentUid = parentUid;
            this.isAbstract = isAbstract;
            this.superUid = superUid;
            this.refIds = refIds;
            this.symbol = symbol;
            this.card = card;
        }

        boolean isTopLevel() {
            return parentUid.equals(RootIdent) || parentUid.equals(BaseClafer);
        }

        boolean isRootChild() {
            return parentUid.equals(RootIdent) && !isAbstract;
        }

        @Override
        public String toString() {
            return uid;
        }
    }

    private final Map<String, C> uidMap = new HashMap<>();
    private final Map<ClaferSymbol, C> bySymbol = new IdentityHashMap<>();
    // the clafers of the model, without the primitive ones, whose names a clafer may have
    private final Map<String, C> modelClafers = new HashMap<>();
    private final List<C> topLevel = new ArrayList<>();
    private final List<Node> aClafers;
    private final List<Node> cClafers;
    private final C root;
    private final boolean dynamic;
    private final long decimalScale;
    private final Map<C, Env> elementEnvs = new IdentityHashMap<>();
    final List<Diagnostic> diagnostics = new ArrayList<>();
    /**
     * The constraints, assertions and transitions after {@code ResolverRoot},
     * the compiler's {@code IEConstraint}s, which the scope analysis reads.
     */
    final List<P> constraints = new ArrayList<>();
    /**
     * The errors of the per-instance initializers of the abstract clafers
     * whose values may have more than one element
     * ({@code InitializerChecks.unsupportedInitialValues}), which the
     * compiler gives only for a model without other errors.
     */
    final List<Diagnostic> initialValueErrors = new ArrayList<>();

    Resolver(Symbols.Result symbols, Syntax.Model model, Optional<Integer> decimalDigits) {
        root = prim(RootIdent, null);
        root.refTyped = true;
        prim(BaseClafer, null);
        prim("integer", "decimal");
        prim("int", "decimal");
        prim("string", null);
        prim("decimal", "real");
        prim("double", "real");
        prim("real", null);
        prim("boolean", null);
        for (ClaferSymbol s : symbols.all()) {
            String parent = s.parent().map(ClaferSymbol::uid).orElse(s.isAbstract() ? BaseClafer : RootIdent);
            C c = new C(s.uid(), transIdent(s.name()), parent, s.isAbstract(),
                    s.superClafer().map(ClaferSymbol::uid).orElse(null), s.referenceIds, s, s.card());
            uidMap.putIfAbsent(c.uid, c);
            modelClafers.putIfAbsent(c.uid, c);
            bySymbol.put(s, c);
        }
        for (ClaferSymbol s : symbols.all()) {
            C c = bySymbol.get(s);
            for (ClaferSymbol child : s.children()) {
                c.children.add(bySymbol.get(child));
            }
        }
        for (ClaferSymbol s : symbols.topLevel()) {
            if (s.isTopLevel()) {
                topLevel.add(bySymbol.get(s));
            }
        }
        root.children.addAll(topLevel);
        aClafers = bfs(topLevel.stream().filter(c -> c.isAbstract).map(c -> new Node(c, List.of(c))).toList());
        cClafers = bfs(topLevel.stream().filter(c -> !c.isAbstract).map(c -> new Node(c, List.of(c))).toList());
        dynamic = isDynamic(symbols, model);
        decimalScale = decimalDigits.map(Integer::longValue).orElseGet(() -> maxFractionalDigits(model));
    }

    /**
     * @return whether the model is temporal: it has a temporal operator or
     * modifier
     */
    boolean isDynamic() {
        return dynamic;
    }

    /**
     * @return the number of fractional digits of decimals:
     * {@code --decimal-digits}, or the most of a decimal literal
     */
    long decimalScale() {
        return decimalScale;
    }

    /**
     * @return whether a clafer refers to decimals ({@code -> decimal})
     */
    boolean refersToDecimal() {
        return uidMap.values().stream().anyMatch(c -> c.symbol != null && c.refIds.contains("decimal"));
    }

    private C prim(String uid, String superUid) {
        C c = new C(uid, uid, "", false, superUid, List.of(), null, Interval.One);
        c.refTyped = true;
        uidMap.put(uid, c);
        return c;
    }

    private static String transIdent(String s) {
        return s.equals("ref") ? "dref" : s;
    }

    C of(ClaferSymbol s) {
        return s == null ? root : bySymbol.get(s);
    }

    /**
     * @return the clafer of the model with the UID, not a primitive one
     */
    C clafer(String uid) {
        return modelClafers.get(uid);
    }

    C find(String uid) {
        return uidMap.get(uid);
    }

    private List<String> getSuper(C c) {
        return c.superUid == null ? List.of() : List.of(c.superUid);
    }

    private List<String> getReference(C c) {
        return c.refIds;
    }

    private List<String> getSuperAndReference(C c) {
        List<String> s = new ArrayList<>(getSuper(c));
        s.addAll(getReference(c));
        return s;
    }

    /**
     * The clafer and its supers ({@code findHierarchy}), or its supers and
     * reference targets, ending at a clafer already on the path.
     */
    private List<C> findHierarchy(java.util.function.Function<C, List<String>> sFun, C c) {
        List<C> out = new ArrayList<>();
        hierarchy(sFun, new ArrayList<>(), c, out);
        return out;
    }

    private void hierarchy(java.util.function.Function<C, List<String>> sFun, List<String> path, C c, List<C> out) {
        out.add(c);
        if (path.contains(c.uid)) {
            return;
        }
        List<String> sups = sFun.apply(c);
        if (sups.isEmpty()) {
            return;
        }
        List<String> path2 = new ArrayList<>(path);
        path2.add(c.uid);
        for (String s : sups) {
            C x = find(s);
            if (x != null) {
                hierarchy(sFun, path2, x, out);
            }
        }
    }

    /**
     * The UIDs of the hierarchy of a clafer ({@code hierarchyMap _uid}); a
     * missing clafer fails the alternative.
     */
    private List<String> hierarchyUids(String uid) {
        C c = find(uid);
        if (c == null) {
            throw Drop.It;
        }
        return findHierarchy(this::getSuper, c).stream().map(x -> x.uid).toList();
    }

    // ======================================================================
    // The compiler's intermediate expressions
    // ======================================================================
    enum Bind {
        NONE, LOCAL, GLOBAL
    }

    record P(Type type, SourceSpan span, E exp) {

        P withType(Type t) {
            return new P(t, span, exp);
        }

        P withExp(E e) {
            return new P(type, span, e);
        }
    }

    sealed interface E permits Id, Fun, Q, IntE, DoubleE, DecimalE, StrE {
    }

    record Id(String mod, String id, boolean top, Bind bind, String bound) implements E {
    }

    record Fun(String op, List<P> args) implements E {

        Fun {
            args = List.copyOf(args);
        }
    }

    record Q(Ir.Quantifier quant, List<D> decls, P body) implements E {

        Q {
            decls = List.copyOf(decls);
        }
    }

    record D(boolean disj, List<String> locals, P body) {

        D {
            locals = List.copyOf(locals);
        }
    }

    record IntE(long value) implements E {
    }

    record DoubleE(double value) implements E {
    }

    record DecimalE(String text) implements E {
    }

    record StrE(String text) implements E {
    }

    private static Id id(String name, boolean top, Bind bind, String bound) {
        return new Id("", name, top, bind, bound);
    }

    private static P pexp(SourceSpan span, E e) {
        return new P(null, span, e);
    }

    private static Fun join(P l, P r) {
        return new Fun(".", List.of(l, r));
    }

    private static boolean isId(P p, String name) {
        return p.exp() instanceof Id i && i.id().equals(name);
    }

    /**
     * An error of the compiler, which ends the expression.
     */
    static final class Abort extends RuntimeException {

        final Diagnostic diagnostic;

        Abort(SourceSpan span, String message) {
            super(message, null, false, false);
            this.diagnostic = new Diagnostic(span, message);
        }
    }

    /**
     * {@code fail} in the type checker's list of alternatives: the
     * alternative is dropped.
     */
    static final class Drop extends RuntimeException {

        static final Drop It = new Drop();

        private Drop() {
            super(null, null, false, false);
        }
    }

    private static String showExp(E e) {
        return switch (e) {
            case Id i -> "IClaferId {_modName = \"" + i.mod() + "\", _sident = \"" + i.id() + "\"}";
            case Fun f -> "IFunExp {_op = \"" + f.op() + "\"}";
            case Q q -> "IDeclPExp {_quant = " + q.quant() + "}";
            case IntE i -> "IInt " + i.value();
            case DoubleE d -> "IDouble " + d.value();
            case DecimalE d -> "IDecimal \"" + d.text() + "\"";
            case StrE s -> "IStr " + s.text();
        };
    }

    // ======================================================================
    // The desugarer (Desugarer.hs)
    // ======================================================================
    P desugarExp(Syntax.Exp x) {
        // a qualified name A\b is at its last name, as in the compiler
        SourceSpan span = x instanceof Syntax.ClaferId c ? c.name().path().get(c.name().path().size() - 1).span()
                : x.span();
        return pexp(span, desugarExpPrime(x));
    }

    private E desugarExpPrime(Syntax.Exp x) {
        return switch (x) {
            case Syntax.Quantified q -> {
                List<String> locals = q.decl().locals().stream().map(l -> transIdent(l.text())).toList();
                D decl = new D(q.disj(), locals, desugarExp(q.decl().exp()));
                yield new Q(quant(q.quant()), List.of(decl), desugarPath(desugarExp(q.body())));
            }
            case Syntax.IfThenElse i -> dop("ifthenelse", List.of(i.condition(), i.then(), i.otherwise()));
            case Syntax.Binary b -> dop(binOp(b.op()), List.of(b.left(), b.right()));
            case Syntax.Unary u -> dop(unOp(u.op()), List.of(u.exp()));
            case Syntax.Anchor a -> dop(a.initially() ? "initially" : "finally", List.of(a.exp()));
            case Syntax.QuantExp q -> new Q(quant(q.quant()), List.of(), desugarExp(q.exp()));
            case Syntax.ClaferId c -> desugarName(c.name());
            case Syntax.Literal l -> switch (l.kind()) {
                case Int -> {
                    try {
                        yield new IntE(Long.parseLong(l.text()));
                    } catch (NumberFormatException e) {
                        throw new Abort(l.span(), "The number " + l.text() + " is too large");
                    }
                }
                case Double -> new DoubleE(Double.parseDouble(l.text()));
                case Real -> new DecimalE(l.text());
                case Str -> new StrE(l.text());
            };
            case Syntax.TransitionExp t -> desugarTransition(t.span(), t.from(), t.arrow(), t.to());
            case Syntax.Let l -> desugarExpPrime(substName(l.local().text(), l.value(), l.body()));
            case Syntax.Pattern p -> {
                List<Syntax.Exp> args = new ArrayList<>(p.exps());
                String scope = "";
                if (p.scope().isPresent()) {
                    scope = switch (p.scope().get().kind()) {
                        case Before -> "before";
                        case After -> "after";
                        case BetweenAnd -> "between";
                        case AfterUntil -> "after until";
                    };
                    args.addAll(p.scope().get().exps());
                }
                String kind = switch (p.kind()) {
                    case Never -> "never";
                    case Sometime -> "sometime";
                    case LessOrOnce -> "lonce";
                    case Always -> "always";
                    case Precede -> "precede";
                    case Follow -> "follow";
                };
                yield dop(patternOp(kind, scope), args);
            }
        };
    }

    private static Ir.Quantifier quant(Syntax.Quant q) {
        return switch (q) {
            case All -> Ir.Quantifier.ALL;
            case No, Not -> Ir.Quantifier.NO;
            case Lone -> Ir.Quantifier.LONE;
            case One -> Ir.Quantifier.ONE;
            case Some -> Ir.Quantifier.SOME;
        };
    }

    private static String binOp(Syntax.BinaryOp op) {
        return switch (op) {
            case LtlU, Until -> "U";
            case LtlW, WeakUntil -> "W";
            case UnionCom -> "++";
            case IntersectionDeprecated -> "**";
            case Rem -> "%";
            default -> op.syntax();
        };
    }

    private static String unOp(Syntax.UnaryOp op) {
        return switch (op) {
            case LtlF, Eventually -> "F";
            case LtlG, Globally -> "G";
            case LtlX, Next -> "X";
            default -> op.syntax();
        };
    }

    private static Id desugarName(Syntax.Name name) {
        List<Syntax.Ident> path = name.path();
        StringBuilder mod = new StringBuilder();
        for (int i = 0; i < path.size() - 1; i++) {
            mod.append(transIdent(path.get(i).text())).append('\\');
        }
        return new Id(mod.toString(), transIdent(path.get(path.size() - 1).text()), true, Bind.NONE, null);
    }

    private E dop(String op, List<Syntax.Exp> exps) {
        List<P> mapped = exps.stream().map(this::desugarExp).toList();
        if (op.equals("ifthenelse")) {
            return new Fun(op, List.of(desugarPath(mapped.get(0)), reducePExp(mapped.get(1)), reducePExp(mapped.get(2))));
        }
        boolean trans = op.equals("!") || LogBinOps.contains(op) || LtlUnOps.contains(op) || isRichTemporal(op);
        return new Fun(op, trans ? mapped.stream().map(Resolver::desugarPath).toList() : mapped);
    }

    private E desugarTransition(SourceSpan span, Syntax.Exp e1, Syntax.TransArrow arrow, Syntax.Exp e2) {
        List<Syntax.Exp> args = new ArrayList<>();
        args.add(e1);
        arrow.guard().ifPresent(args::add);
        args.add(e2);
        if (arrow.sync() && !isThis(e1)) {
            args.add(thisAt(span));
        }
        String op = arrow.guard().isPresent() ? (arrow.sync() ? "-[]->>" : "-[]->") : (arrow.sync() ? "-->>" : "-->");
        return dop(op, args);
    }

    static Syntax.ClaferId thisAt(SourceSpan span) {
        return new Syntax.ClaferId(span, new Syntax.Name(span, List.of(new Syntax.Ident(span, "this"))));
    }

    private static boolean isThis(Syntax.Exp e) {
        return switch (e) {
            case Syntax.ClaferId c -> c.name().path().size() == 1 && transIdent(c.name().path().get(0).text()).equals("this");
            case Syntax.QuantExp q -> q.quant() == Syntax.Quant.Some && isThis(q.exp());
            default -> false;
        };
    }

    static P desugarPath(P p) {
        E result;
        if (isSetExp(p.exp())) {
            result = new Q(Ir.Quantifier.SOME, List.of(), pexp(p.span(), p.exp()));
        } else if (p.exp() instanceof Fun f && f.op().equals("!") && f.args().size() == 1
                && f.args().get(0).exp() instanceof Q q && q.quant() == Ir.Quantifier.SOME && q.decls().isEmpty()) {
            result = new Q(Ir.Quantifier.NO, List.of(), q.body());
        } else {
            result = p.exp();
        }
        return reducePExp(new P(p.type(), p.span(), result));
    }

    private static boolean isSetExp(E e) {
        return e instanceof Id || e instanceof Fun f && SetBinOps.contains(f.op());
    }

    static P reducePExp(P p) {
        return p.withExp(reduceIExp(p.exp()));
    }

    private static E reduceIExp(E e) {
        return switch (e) {
            case Q q -> new Q(q.quant(), q.decls(), reducePExp(q.body()));
            case Fun f when f.op().equals(".") -> reduceNav(f);
            case Fun f -> new Fun(f.op(), f.args().stream().map(Resolver::reducePExp).toList());
            default -> e;
        };
    }

    private static E reduceNav(E x) {
        if (x instanceof Fun f && f.args().size() >= 2 && f.args().get(0).exp() instanceof Fun inner
                && inner.args().size() >= 2) {
            P pexp0 = inner.args().get(0);
            P pexp = inner.args().get(1);
            P pPexp = f.args().get(1);
            if (f.op().equals(".") && isId(pPexp, "parent") && isClaferName(pexp)) {
                return reduceNav(pexp0.exp());
            }
            List<P> args = new ArrayList<>(f.args());
            args.set(0, args.get(0).withExp(reduceIExp(inner)));
            return new Fun(f.op(), args);
        }
        return x;
    }

    private static boolean isClaferName(P p) {
        return p.exp() instanceof Id i && !SpecialNames.contains(i.id()) && !PrimitiveTypes.contains(i.id());
    }

    // ----------------------------------------------------------------------
    // let (desugarLet, substName)
    // ----------------------------------------------------------------------
    private static Syntax.Exp substName(String v, Syntax.Exp e, Syntax.Exp body) {
        Set<String> namesOfE = namesIn(e);
        return new Object() {
            Syntax.Exp go(Syntax.Exp x) {
                return switch (x) {
                    case Syntax.ClaferId c when c.name().path().size() == 1 && c.name().path().get(0).text().equals(v) -> e;
                    case Syntax.Quantified q -> {
                        Bound b = bound(q.decl().locals(), q.body());
                        yield new Syntax.Quantified(q.span(), q.quant(), q.disj(),
                                new Syntax.Decl(q.decl().span(), b.locals, go(q.decl().exp())), b.body);
                    }
                    case Syntax.Let l -> {
                        Bound b = bound(List.of(l.local()), l.body());
                        yield new Syntax.Let(l.span(), b.locals.get(0), go(l.value()), b.body);
                    }
                    default -> mapChildren(x, this::go);
                };
            }

            Bound bound(List<Syntax.Ident> ls, Syntax.Exp b) {
                if (ls.stream().anyMatch(l -> l.text().equals(v)) || !namesIn(b).contains(v)) {
                    return new Bound(ls, b);
                }
                Set<String> taken = new java.util.HashSet<>(namesOfE);
                taken.addAll(namesIn(b));
                ls.forEach(l -> taken.add(l.text()));
                List<Syntax.Ident> renamed = new ArrayList<>();
                for (Syntax.Ident l : ls) {
                    if (namesOfE.contains(l.text())) {
                        int k = 1;
                        while (taken.contains(l.text() + "_" + k)) {
                            k++;
                        }
                        renamed.add(new Syntax.Ident(l.span(), l.text() + "_" + k));
                    } else {
                        renamed.add(l);
                    }
                }
                Syntax.Exp b2 = b;
                for (int i = ls.size() - 1; i >= 0; i--) {
                    Syntax.Ident old = ls.get(i);
                    Syntax.Ident neu = renamed.get(i);
                    if (!old.equals(neu)) {
                        b2 = substName(old.text(), new Syntax.ClaferId(neu.span(),
                                new Syntax.Name(neu.span(), List.of(neu))), b2);
                    }
                }
                return new Bound(renamed, go(b2));
            }
        }.go(body);
    }

    private record Bound(List<Syntax.Ident> locals, Syntax.Exp body) {
    }

    private static Set<String> namesIn(Syntax.Exp x) {
        Set<String> names = new java.util.HashSet<>();
        collectNames(x, names);
        return names;
    }

    private static void collectNames(Syntax.Exp x, Set<String> names) {
        switch (x) {
            case Syntax.ClaferId c -> c.name().path().forEach(i -> names.add(i.text()));
            case Syntax.Quantified q -> q.decl().locals().forEach(i -> names.add(i.text()));
            case Syntax.Let l -> names.add(l.local().text());
            default -> {
            }
        }
        mapChildren(x, c -> {
            collectNames(c, names);
            return c;
        });
    }

    /**
     * The expression with the function applied to each expression in it.
     */
    private static Syntax.Exp mapChildren(Syntax.Exp x, UnaryOperator<Syntax.Exp> f) {
        return switch (x) {
            case Syntax.TransitionExp t -> new Syntax.TransitionExp(t.span(), f.apply(t.from()),
                    new Syntax.TransArrow(t.arrow().span(), t.arrow().sync(), t.arrow().guard().map(f)), f.apply(t.to()));
            case Syntax.Quantified q -> new Syntax.Quantified(q.span(), q.quant(), q.disj(),
                    new Syntax.Decl(q.decl().span(), q.decl().locals(), f.apply(q.decl().exp())), f.apply(q.body()));
            case Syntax.IfThenElse i -> new Syntax.IfThenElse(i.span(), f.apply(i.condition()), f.apply(i.then()),
                    f.apply(i.otherwise()));
            case Syntax.Let l -> new Syntax.Let(l.span(), l.local(), f.apply(l.value()), f.apply(l.body()));
            case Syntax.Pattern p -> new Syntax.Pattern(p.span(), p.kind(), p.exps().stream().map(f).toList(),
                    p.scope().map(s -> new Syntax.PatternScope(s.span(), s.kind(), s.exps().stream().map(f).toList())));
            case Syntax.Anchor a -> new Syntax.Anchor(a.span(), a.initially(), f.apply(a.exp()));
            case Syntax.Binary b -> new Syntax.Binary(b.span(), b.op(), f.apply(b.left()), f.apply(b.right()));
            case Syntax.Unary u -> new Syntax.Unary(u.span(), u.op(), f.apply(u.exp()));
            case Syntax.QuantExp q -> new Syntax.QuantExp(q.span(), q.quant(), f.apply(q.exp()));
            case Syntax.ClaferId c -> c;
            case Syntax.Literal l -> l;
        };
    }

    // ======================================================================
    // The name resolver (ResolverName.hs)
    // ======================================================================
    record Node(C clafer, List<C> path) {
    }

    private record Binding(List<String> names, List<C> path) {
    }

    private record Env(C context, List<Node> sub, List<Node> anc, List<Binding> bindings, List<C> resPath,
            Env parentEnv) {

        Env withPath(C context, List<C> resPath) {
            return new Env(context, sub, anc, bindings, resPath, parentEnv);
        }

        Env bind(List<String> names, List<C> path) {
            List<Binding> b = new ArrayList<>();
            b.add(new Binding(names, path));
            b.addAll(bindings);
            return new Env(context, sub, anc, b, resPath, parentEnv);
        }
    }

    private static final Env DefaultEnv = new Env(null, List.of(), List.of(), List.of(), List.of(), null);

    private enum How {
        Special, TypeSpecial, Binding, Subclafers, Reference, TargetChild, Ancestor, AbsClafer, TopClafer
    }

    private record Resolution(How how, String id, List<C> path) {
    }

    private record Nav(P pexp, List<C> path) {
    }

    /**
     * The environment of the elements of a clafer, or of the model.
     */
    private Env elementEnv(C c) {
        if (c == root) {
            return DefaultEnv;
        }
        Env cached = elementEnvs.get(c);
        if (cached != null) {
            return cached;
        }
        Env env = c.isTopLevel() ? DefaultEnv : elementEnv(find(c.parentUid));
        List<C> resPath = cons(c, env.resPath());
        List<Node> all = bfs(List.of(new Node(c, List.of(c))));
        List<Node> sub = all.subList(1, all.size());
        List<Node> anc = new ArrayList<>();
        List<C> rp = env.resPath();
        for (int i = 0; i < rp.size(); i++) {
            List<C> tail = rp.subList(i, rp.size());
            anc.addAll(bfs(List.of(new Node(tail.get(0), tail))));
        }
        Env result = new Env(c, List.copyOf(sub), List.copyOf(anc), List.of(), resPath, env);
        elementEnvs.put(c, result);
        return result;
    }

    private static <T> List<T> cons(T x, List<T> xs) {
        List<T> l = new ArrayList<>();
        l.add(x);
        l.addAll(xs);
        return List.copyOf(l);
    }

    private List<Node> bfs(List<Node> seeds) {
        List<Node> out = new ArrayList<>();
        List<Node> level = seeds;
        while (!level.isEmpty()) {
            List<Node> next = new ArrayList<>();
            for (Node n : level) {
                out.add(n);
                long repeats = n.path().stream().filter(x -> x == n.clafer()).count();
                if (repeats > 1) {
                    continue;
                }
                for (C child : allInhChildren(n.clafer())) {
                    next.add(new Node(child, cons(child, n.path())));
                }
            }
            level = next;
        }
        return out;
    }

    private List<C> allInhChildren(C c) {
        List<C> out = new ArrayList<>();
        for (C x : findHierarchy(this::getSuper, c)) {
            out.addAll(x.children);
        }
        return out;
    }

    private List<C> allChildren(C c) {
        List<C> out = new ArrayList<>();
        for (C x : findHierarchy(this::getSuperAndReference, c)) {
            out.addAll(x.children);
        }
        return out;
    }

    /**
     * The names of an expression, resolved in an environment.
     */
    private P resolvePExp(Env env, P pexp) {
        return switch (pexp.exp()) {
            case Q q -> {
                Env env2 = env;
                List<D> decls = new ArrayList<>();
                for (D d : q.decls()) {
                    Nav nav = resolveNav(env2, d.body(), true);
                    env2 = env2.bind(d.locals(), nav.path());
                    decls.add(new D(d.disj(), d.locals(), nav.pexp()));
                }
                yield pexp.withExp(new Q(q.quant(), decls, resolvePExp(env2, q.body())));
            }
            case Fun f when f.op().equals(".") -> resolveNav(env, pexp, true).pexp();
            case Fun f -> pexp.withExp(new Fun(f.op(), f.args().stream().map(a -> resolvePExp(env, a)).toList()));
            case Id i -> resolveNav(env, pexp, true).pexp();
            default -> pexp;
        };
    }

    private Nav resolveNav(Env env, P pexp0, boolean isFirst) {
        SourceSpan pos = pexp0.span();
        E x = pexp0.exp();
        if (x instanceof Fun f && f.op().equals(".") && f.args().size() == 2) {
            P pexp1 = f.args().get(0);
            P pexp2 = f.args().get(1);
            if (fromNoClafer(pexp1)) {
                return new Nav(pexp0.withExp(noClaferIExp()), List.of());
            }
            Nav n1 = resolveNav(env, pexp1, true);
            P pexp1r = n1.pexp();
            List<C> path1 = n1.path();
            Nav n2 = resolveNav(env.withPath(path1.isEmpty() ? null : path1.get(0), path1), pexp2, false);
            P pexp2r = n2.pexp();
            if (pexp2r.exp() instanceof Fun g && g.op().equals(".") && g.args().size() == 2) {
                P l = g.args().get(0);
                P r = g.args().get(1);
                if (isId(l, "dref") && endsWithDref(pexp1r)) {
                    return new Nav(pexp0.withExp(join(pexp1r, r)), n2.path());
                }
                if (isId(l, "dref")) {
                    P pexp0r = pexp0.withExp(join(pexp1r, l));
                    return new Nav(pexp2.withExp(join(pexp0r, r)), n2.path());
                }
                return new Nav(pexp0.withExp(join(pexp1r, pexp2r)), n2.path());
            }
            if (isId(pexp2r, "parent") && !path1.isEmpty() && path1.get(0).isRootChild() && isSingleInstance(pexp1r)) {
                return new Nav(pexp0.withExp(rootIExp()), List.of());
            }
            return new Nav(pexp0.withExp(join(pexp1r, pexp2r)), n2.path());
        }
        if (x instanceof Fun f && f.args().size() == 2 && (f.op().equals("**") || f.op().equals("--"))) {
            Nav l = resolveNav(env, f.args().get(0), true);
            Nav r = resolveNav(env, f.args().get(1), true);
            return new Nav(pexp0.withExp(new Fun(f.op(), List.of(l.pexp(), r.pexp()))),
                    f.op().equals("**") ? r.path() : l.path());
        }
        if (x instanceof Id i) {
            String id = i.id();
            if (id.equals(RootIdent)) {
                return new Nav(pexp0, List.of());
            }
            if (id.equals(NoClaferIdent)) {
                return new Nav(pexp0.withExp(noClaferIExp()), List.of());
            }
            if (id.equals(BaseClafer) && isFirst) {
                return new Nav(pexp0.withExp(id(BaseClafer, true, Bind.GLOBAL, BaseClafer)), List.of());
            }
            if (isFirst) {
                Resolution r = resolveName(pos, env, id);
                Nav n = mkPath(pos, env, r);
                return new Nav(pexp0.withExp(n.pexp().exp()), derefPath(id, n.path()));
            }
            Resolution r = env.resPath().isEmpty() ? resolveTopLevelName(pos, env, id) : resolveImmName(pos, env, id);
            Nav n = mkPathPrime(pos, i.mod(), r);
            return new Nav(pexp0.withExp(n.pexp().exp()), derefPath(id, n.path()));
        }
        throw new Abort(pos, "Cannot resolve nav of " + showExp(x));
    }

    private static boolean isSingleInstance(P p) {
        return p.exp() instanceof Id i && (i.id().equals("this") || i.bind() == Bind.LOCAL);
    }

    private static Id rootIExp() {
        return id(RootIdent, true, Bind.GLOBAL, RootIdent);
    }

    private static Id noClaferIExp() {
        return id(NoClaferIdent, true, Bind.GLOBAL, NoClaferIdent);
    }

    private static boolean fromNoClafer(P p) {
        if (p.exp() instanceof Fun f && f.op().equals(".") && f.args().size() == 2) {
            return fromNoClafer(f.args().get(0));
        }
        return isId(p, NoClaferIdent);
    }

    private static boolean endsWithDref(P p) {
        if (p.exp() instanceof Id i) {
            return i.id().equals("dref");
        }
        return p.exp() instanceof Fun f && f.op().equals(".") && f.args().size() == 2 && endsWithDref(f.args().get(1));
    }

    private List<C> derefPath(String id, List<C> path) {
        if (id.equals("dref") && !path.isEmpty()) {
            for (C h : findHierarchy(this::getSuper, path.get(0))) {
                List<String> r = getReference(h);
                if (!r.isEmpty()) {
                    if (r.size() == 1 && find(r.get(0)) != null) {
                        return cons(find(r.get(0)), path);
                    }
                    break;
                }
            }
        }
        return path;
    }

    private static Bind bindKind(C c) {
        return c == null ? Bind.NONE : Bind.GLOBAL;
    }

    private static String bound(C c) {
        return c == null ? null : c.uid;
    }

    private Nav mkPath(SourceSpan pos, Env env, Resolution r) {
        List<C> path = r.path();
        String id = r.id();
        E exp = switch (r.how()) {
            case Binding -> id(id, true, Bind.LOCAL, id);
            case Special -> switch (id) {
                case "this" -> id("this", true, bindKind(env.context()), bound(env.context()));
                case "parent" -> path.isEmpty() && env.context() != null && env.context().isRootChild() ? rootIExp()
                        : toNav(pos, env, List.of(new Tuple("parent", path.isEmpty() ? null : path.get(0))));
                case "dref" -> toNav(pos, env, List.of(new Tuple("dref", head(pos, path))));
                case "root" -> rootIExp();
                case "noclafer" -> noClaferIExp();
                case "children" -> toNav(pos, env, List.of(new Tuple(id, head(pos, path))));
                default -> throw new IllegalStateException(id);
            };
            case TypeSpecial -> id(id, true, Bind.GLOBAL, id);
            case Subclafers -> {
                List<Tuple> t = tuples(reverse(path));
                yield toNav(pos, env, t.subList(1, t.size()));
            }
            case Ancestor -> toNavPrime(pos, adjustAncestor(env.context(), tuples(reverse(env.resPath())),
                    tuples(reverse(path))));
            case Reference, AbsClafer, TopClafer -> toNavPrime(pos, tuples(reverse(path)));
            case TargetChild -> toNav(pos, env, List.of(new Tuple("dref", env.context()), new Tuple(id, path.get(0))));
        };
        return new Nav(pexp(pos, exp), path);
    }

    private static C head(SourceSpan pos, List<C> path) {
        if (path.isEmpty()) {
            throw new Abort(pos, "Prelude.head: empty list");
        }
        return path.get(0);
    }

    private record Tuple(String id, C clafer) {
    }

    private static List<Tuple> tuples(List<C> path) {
        return path.stream().map(c -> new Tuple(c.uid, c)).toList();
    }

    private static <T> List<T> reverse(List<T> xs) {
        List<T> r = new ArrayList<>(xs);
        java.util.Collections.reverse(r);
        return r;
    }

    private static E toNav(SourceSpan pos, Env env, List<Tuple> tuplePath) {
        E exp = id("this", true, bindKind(env.context()), bound(env.context()));
        for (Tuple t : tuplePath) {
            exp = join(pexp(pos, exp), pexp(pos, id(t.id(), false, bindKind(t.clafer()), bound(t.clafer()))));
        }
        return exp;
    }

    private static E toNavPrime(SourceSpan pos, List<Tuple> tuplePath) {
        List<E> ids = tuplePath.stream().map(t -> (E) id(t.id(), t.clafer() != null && t.clafer().isTopLevel(),
                bindKind(t.clafer()), bound(t.clafer()))).toList();
        E exp = ids.get(0);
        for (int i = 1; i < ids.size(); i++) {
            exp = join(pexp(pos, exp), pexp(pos, ids.get(i)));
        }
        return exp;
    }

    private static List<Tuple> adjustAncestor(C ctx, List<Tuple> cPath, List<Tuple> rPath) {
        int prefix = 0;
        while (prefix < cPath.size() && prefix < rPath.size() && cPath.get(prefix).id().equals(rPath.get(prefix).id())) {
            prefix++;
        }
        List<Tuple> out = new ArrayList<>();
        out.add(new Tuple("this", ctx));
        for (Tuple t : cPath.subList(prefix, cPath.size())) {
            out.add(new Tuple("parent", t.clafer()));
        }
        out.addAll(rPath.subList(prefix, rPath.size()));
        return out;
    }

    private Nav mkPathPrime(SourceSpan pos, String mod, Resolution r) {
        List<C> path = r.path();
        if (r.how() == How.Reference) {
            List<C> rev = reverse(path);
            List<Tuple> t = List.of(new Tuple("dref", rev.get(0)), new Tuple(r.id(), rev.get(1)));
            return new Nav(pexp(pos, toNavPrime(pos, t)), path);
        }
        C c = path.isEmpty() ? null : path.get(0);
        return new Nav(pexp(pos, new Id(mod, r.id(), c != null && c.isTopLevel(), bindKind(c), bound(c))), path);
    }

    private Resolution resolveTopLevelName(SourceSpan pos, Env env, String id) {
        List<C> found = topLevel.stream().filter(c -> c.ident.equals(id)).toList();
        if (found.size() == 1) {
            return new Resolution(How.TopClafer, found.get(0).uid, found);
        }
        throw resolveNone(pos, env, id);
    }

    private Resolution resolveName(SourceSpan pos, Env env, String id) {
        Resolution r = resolveSpecial(env, id);
        if (r == null) {
            r = resolveBind(env, id);
        }
        if (r == null && env.context() != null) {
            r = findFirst(id, env.sub()).map(u -> new Resolution(How.Subclafers, u.uid, u.path)).orElse(null);
        }
        if (r == null && env.context() != null) {
            r = findUnique(pos, id, env.anc()).map(u -> new Resolution(How.Ancestor, u.uid, u.path)).orElse(null);
        }
        if (r == null && env.context() != null && !env.context().refIds.isEmpty()) {
            List<C> targetChildren = new ArrayList<>(allChildren(env.context()));
            for (C inh : allInhChildren(env.context())) {
                targetChildren.remove(inh);
            }
            r = resolveChildren(pos, env, id, targetChildren, How.TargetChild);
        }
        if (r == null) {
            r = findUnique(pos, id, aClafers).map(u -> new Resolution(How.AbsClafer, u.uid, u.path)).orElse(null);
        }
        if (r == null) {
            r = findUnique(pos, id, cClafers).map(u -> new Resolution(How.TopClafer, u.uid, u.path)).orElse(null);
        }
        if (r == null) {
            throw resolveNone(pos, env, id);
        }
        return r;
    }

    private Resolution resolveImmName(SourceSpan pos, Env env, String id) {
        Resolution r = resolveSpecial(env, id);
        if (r == null) {
            r = resolveChildren(pos, env, id, allInhChildren(env.context()), How.Subclafers);
        }
        if (r == null) {
            r = resolveChildren(pos, env, id, allChildren(env.context()), How.Reference);
        }
        if (r == null) {
            throw resolveNone(pos, env, id);
        }
        return r;
    }

    private static Abort resolveNone(SourceSpan pos, Env env, String id) {
        return new Abort(pos, "Name resolver: '" + id + "' not found within paths:"
                + showPath(env.resPath().stream().map(c -> c.uid).toList())
                + "in context of '\"" + (env.context() == null ? "none" : env.context().uid) + "\"'");
    }

    private static Resolution resolveSpecial(Env env, String id) {
        if (id.equals("parent")) {
            List<C> rp = env.resPath();
            return new Resolution(How.Special, id, rp.isEmpty() ? List.of() : rp.subList(1, rp.size()));
        }
        if (SpecialNames.contains(id)) {
            return new Resolution(How.Special, id, env.resPath());
        }
        if (PrimitiveTypes.contains(id)) {
            return new Resolution(How.TypeSpecial, id, List.of());
        }
        return null;
    }

    private static Resolution resolveBind(Env env, String id) {
        for (Binding b : env.bindings()) {
            if (b.names().contains(id)) {
                return new Resolution(How.Binding, id, b.path());
            }
        }
        return null;
    }

    private Resolution resolveChildren(SourceSpan pos, Env env, String id, List<C> children, How how) {
        if (env.context() == null) {
            return null;
        }
        List<Node> nodes = children.stream().map(x -> new Node(x, List.of(x, env.context()))).toList();
        return findUnique(pos, id, nodes).map(u -> new Resolution(how, u.uid, u.path)).orElse(null);
    }

    private record Found(String uid, List<C> path) {
    }

    private static Optional<Found> findUnique(SourceSpan pos, String x, List<Node> xs) {
        List<Node> found = nub(xs).stream().filter(n -> n.clafer().ident.equals(x)).toList();
        if (found.isEmpty()) {
            return Optional.empty();
        }
        if (found.size() == 1) {
            return Optional.of(new Found(found.get(0).clafer().uid, found.get(0).path()));
        }
        List<List<String>> paths = found.stream().map(n -> n.path().stream().map(c -> c.uid).toList()).toList();
        boolean conflict = paths.get(0).subList(1, paths.get(0).size()).equals(paths.get(1).subList(1, paths.get(1).size()));
        StringBuilder msg = new StringBuilder("clafer \"" + x + "\" ");
        msg.append(conflict ? "cannot be defined because the name should be unique in the same namespace.\n"
                : "is not unique. ");
        msg.append("Available paths:\n");
        paths.forEach(p -> msg.append(showPath(p)));
        throw new Abort(pos, msg.toString());
    }

    private static Optional<Found> findFirst(String x, List<Node> xs) {
        return nub(xs).stream().filter(n -> n.clafer().ident.equals(x)).findFirst()
                .map(n -> new Found(n.clafer().uid, n.path()));
    }

    private static String showPath(List<String> xs) {
        return String.join(".", reverse(xs)) + "\n";
    }

    // ======================================================================
    // The type checker (ResolverType.hs)
    // ======================================================================
    private record Ctx(C curThis, Type curPath, List<Map.Entry<String, Type>> decls) {

        Ctx withPath(Type t) {
            return new Ctx(curThis, t, decls);
        }

        Ctx withDecls(List<Map.Entry<String, Type>> extra) {
            List<Map.Entry<String, Type>> d = new ArrayList<>(extra);
            d.addAll(decls);
            return new Ctx(curThis, curPath, d);
        }
    }

    /**
     * An alternative: a typed expression, or an error.
     */
    private record Alt(P ok, Abort err) {

        static Alt ok(P p) {
            return new Alt(p, null);
        }

        static Alt err(SourceSpan span, String message) {
            return new Alt(null, new Abort(span, message));
        }
    }

    private Type typeOfUid(Ctx cx, String uid) {
        for (Map.Entry<String, Type> d : cx.decls()) {
            if (d.getKey().equals(uid)) {
                return d.getValue();
            }
        }
        return new TClafer(List.of(uid));
    }

    private Type getTClafer(C c) {
        if (c.uid.equals(RootIdent)) {
            return RootT;
        }
        if (c.uid.equals(BaseClafer)) {
            return ClaferT;
        }
        return new TClafer(findHierarchy(this::getSuper, c).stream().map(x -> x.uid).toList());
    }

    private Type getTClaferByUid(String uid) {
        if (uid.equals(NoClaferIdent)) {
            return NoClaferT;
        }
        C c = find(uid);
        return c == null ? null : getTClafer(c);
    }

    private Type getDrefTMapByUid(String uid) {
        if (uid.equals(RootIdent) || uid.equals(BaseClafer) || uid.equals(NoClaferIdent)) {
            return null;
        }
        C c = find(uid);
        if (c == null) {
            return null;
        }
        Type ref = refType(c);
        return ref == null ? null : new TMap(getTClafer(c), ref);
    }

    private List<Type> getTMaps(Type t) {
        return switch (t) {
            case TClafer c -> c.hi().stream().map(this::getDrefTMapByUid).filter(x -> x != null).toList();
            case TMap m -> getTMaps(m.ta());
            case TUnion u -> u.un().stream().flatMap(x -> getTMaps(x).stream()).toList();
            default -> List.of();
        };
    }

    private List<Type> getTClafers(Type t) {
        return switch (t) {
            case TClafer c -> c.hi().stream().map(this::getTClaferByUid).filter(x -> x != null).toList();
            case TMap m -> getTClafers(m.ta());
            case TUnion u -> u.un().stream().flatMap(x -> getTClafers(x).stream()).toList();
            default -> List.of();
        };
    }

    private Type addHierarchy(Type t) {
        return switch (t) {
            case TClafer c when c.hi().size() == 1 -> {
                C x = find(c.hi().get(0));
                yield new TClafer(findHierarchy(this::getSuper, x).stream().map(y -> y.uid).toList());
            }
            case TMap m -> new TMap(addHierarchy(m.so()), addHierarchy(m.ta()));
            case TUnion u -> new TUnion(u.un().stream().map(this::addHierarchy).toList());
            default -> t;
        };
    }

    private boolean isChild(String child, String parent) {
        C c = find(child);
        if (c == null) {
            return false;
        }
        if (parent.equals(c.parentUid)) {
            return true;
        }
        List<String> h = hierarchyUids(parent);
        for (String s : h.subList(1, h.size())) {
            if (isChild(child, s)) {
                return true;
            }
        }
        return false;
    }

    private static String str(Type t) {
        List<String> u = unionType(t);
        return u.size() == 1 ? u.get(0) : "[" + String.join(",", u) + "]";
    }

    /**
     * {@code resolveTPExp}: the alternatives that type check, or the first
     * error.
     */
    private List<P> resolveTPExp(Ctx cx, P p) {
        List<Alt> alts = resolveTPExpPrime(cx, p);
        List<P> ok = alts.stream().filter(a -> a.ok() != null).map(Alt::ok).toList();
        if (!ok.isEmpty()) {
            return ok;
        }
        for (Alt a : alts) {
            if (a.err() != null) {
                throw a.err();
            }
        }
        throw new Abort(p.span(), "No results but no errors for " + showExp(p.exp()));
    }

    private List<Alt> liftError(Ctx cx, P p, java.util.function.Function<P, List<Alt>> k) {
        List<P> ps;
        try {
            ps = resolveTPExp(cx, p);
        } catch (Abort e) {
            return List.of(new Alt(null, e));
        }
        List<Alt> out = new ArrayList<>();
        for (P x : ps) {
            out.addAll(k.apply(x));
        }
        return out;
    }

    private List<Alt> resolveTPExpPrime(Ctx cx, P p) {
        if (p.exp() instanceof Id i) {
            return resolveId(cx, p, i);
        }
        if (p.exp() instanceof Fun f && f.op().equals(".") && f.args().size() == 2) {
            List<Alt> out = new ArrayList<>();
            for (P arg1 : resolveTPExp(cx, f.args().get(0))) {
                Ctx inner = cx.withPath(arg1.type());
                List<Alt> results = liftError(inner, f.args().get(1), arg2 -> {
                    Type t = switch (arg2.type()) {
                        case TClafer c -> c;
                        case TMap m -> m.ta();
                        default -> null;
                    };
                    if (t == null) {
                        return List.of();
                    }
                    return List.of(Alt.ok(new P(t, p.span(), join(arg1, arg2))));
                });
                for (Alt r : results) {
                    if (r.err() != null) {
                        out.add(r);
                    } else {
                        out.add(r);
                        out.addAll(addDref(cx, r.ok()));
                        out.add(addSome(r.ok()));
                    }
                }
            }
            return out;
        }
        return resolveTExp(cx, p);
    }

    private List<Alt> resolveId(Ctx cx, P p, Id i) {
        SourceSpan pos = p.span();
        switch (i.id()) {
            case "dref" -> {
                if (cx.curPath() == null) {
                    return List.of(Alt.err(pos, "Cannot deref at the start of a path"));
                }
                List<Type> ts = new ArrayList<>();
                for (Type t : getTClafers(cx.curPath())) {
                    ts.addAll(getTMaps(t));
                }
                if (ts.isEmpty()) {
                    return List.of(Alt.err(pos, "Cannot deref from type '" + str(cx.curPath()) + "'"));
                }
                return List.of(Alt.ok(p.withType(ts.get(0))));
            }
            case "parent" -> {
                if (cx.curPath() == null) {
                    return List.of(Alt.err(pos, "Cannot parent at the start of a path"));
                }
                List<String> parents = new ArrayList<>();
                for (String u : unionType(cx.curPath())) {
                    C c = find(u);
                    if (c != null) {
                        parents.add(c.parentUid);
                    }
                }
                Type t = fromUnionType(parents);
                if (t == null) {
                    return List.of(Alt.err(pos, "Cannot parent from root"));
                }
                return List.of(Alt.ok(p.withType(t)));
            }
            case "integer", "int" -> {
                return List.of(Alt.ok(p.withType(Prim.TInteger)));
            }
            case "string" -> {
                return List.of(Alt.ok(p.withType(Prim.TString)));
            }
            case "double" -> {
                return List.of(Alt.ok(p.withType(Prim.TDouble)));
            }
            case "real" -> {
                return List.of(Alt.ok(p.withType(Prim.TReal)));
            }
            case "decimal" -> {
                return List.of(Alt.ok(p.withType(Prim.TDecimal)));
            }
            case NoClaferIdent -> {
                return List.of(Alt.ok(p.withType(NoClaferT)));
            }
            case BaseClafer -> {
                return List.of(Alt.ok(p.withType(ClaferT)));
            }
            case "this" -> {
                P result = p.withType(typeOfUid(cx, cx.curThis().uid));
                List<Alt> out = new ArrayList<>();
                out.add(Alt.ok(result));
                out.addAll(addDref(cx, result));
                return out;
            }
            default -> {
                if (cx.curPath() != null) {
                    boolean child = false;
                    try {
                        for (String u : unionType(cx.curPath())) {
                            child |= isChild(i.id(), u);
                        }
                    } catch (Drop d) {
                        return List.of();
                    }
                    String parentId = str(cx.curPath());
                    if (!child && !parentId.equals(RootIdent)) {
                        return List.of(Alt.err(pos, "'" + i.id() + "' is not a child of type '" + parentId + "'"));
                    }
                }
                P result = p.withType(typeOfUid(cx, i.id()));
                List<Alt> out = new ArrayList<>();
                out.add(Alt.ok(result));
                if (i.top()) {
                    out.addAll(addDref(cx, result));
                    out.add(addSome(result));
                }
                return out;
            }
        }
    }

    private List<Alt> addDref(Ctx cx, P pexp) {
        Ctx inner = cx.withPath(pexp.type());
        List<Alt> derefs = resolveId(inner, pexp(pexp.span(), id("dref", false, Bind.NONE, null)),
                id("dref", false, Bind.NONE, null));
        List<Alt> out = new ArrayList<>();
        for (Alt d : derefs) {
            if (d.ok() == null) {
                continue;
            }
            P result = new P(d.ok().type(), pexp.span(), join(pexp, d.ok()));
            out.add(Alt.ok(result));
            out.addAll(addDref(cx, result));
        }
        return out;
    }

    private static Alt addSome(P pexp) {
        return Alt.ok(new P(Prim.TBoolean, pexp.span(), new Q(Ir.Quantifier.SOME, List.of(), pexp)));
    }

    private static Alt typed(P p, Type t, E e) {
        return Alt.ok(new P(t, p.span(), e));
    }

    private List<Alt> resolveTExp(Ctx cx, P p) {
        SourceSpan pos = p.span();
        E e = p.exp();
        switch (e) {
            case IntE x -> {
                return List.of(typed(p, Prim.TInteger, e));
            }
            case DoubleE x -> {
                return List.of(typed(p, Prim.TDouble, e));
            }
            case DecimalE x -> {
                return List.of(typed(p, Prim.TDecimal, e));
            }
            case StrE x -> {
                return List.of(typed(p, Prim.TString, e));
            }
            case Fun f when isRichTemporal(f.op()) -> {
                List<Alt> out = new ArrayList<>();
                for (List<P> args : product(cx, f.args())) {
                    if (!args.stream().allMatch(a -> isTBoolean(a.type()))) {
                        out.add(Alt.err(pos, "Function '" + f.op() + "' cannot be performed on "
                                + String.join(", ", args.stream().map(Resolver::showType).toList())));
                    } else {
                        out.add(typed(p, Prim.TBoolean, new Fun(f.op(), args)));
                    }
                }
                return out;
            }
            case Fun f when f.args().size() == 1 -> {
                return unary(cx, p, f);
            }
            case Fun f when f.op().equals("++") && f.args().size() == 2 -> {
                List<Alt> unions = new ArrayList<>();
                for (P a1 : resolveTPExp(cx, f.args().get(0))) {
                    for (P a2 : resolveTPExp(cx, f.args().get(1))) {
                        unions.add(typed(p, union(a1.type(), a2.type()), new Fun("++", List.of(a1, a2))));
                    }
                }
                List<Alt> sorted = new ArrayList<>(unions);
                sorted.sort(java.util.Comparator.comparing((Alt a) -> isTBoolean(a.ok().type()))
                        .thenComparingInt(a -> nub(unionType(a.ok().type()).stream().map(this::topmost).toList()).size()));
                return sorted;
            }
            case Fun f when f.args().size() == 2 -> {
                return binary(cx, p, f);
            }
            case Fun f when f.op().equals("ifthenelse") && f.args().size() == 3 -> {
                List<Alt> out = new ArrayList<>();
                for (List<P> args : product(cx, f.args())) {
                    P a1 = args.get(0);
                    P a2 = args.get(1);
                    P a3 = args.get(2);
                    if (!isTBoolean(a1.type())) {
                        out.add(Alt.err(pos, "The type of condition in 'if/then/else' must be 'TBoolean', insted it is "
                                + showType(a1)));
                        continue;
                    }
                    Optional<Type> it;
                    try {
                        it = ifThenElseType(a2.type(), a3.type());
                    } catch (Drop d) {
                        continue;
                    }
                    if (it.isEmpty()) {
                        out.add(Alt.err(pos, "Function 'if/then/else' cannot be performed on \nif\n" + showType(a1)
                                + "\nthen\n" + showType(a2) + "\nelse\n" + showType(a3)));
                        continue;
                    }
                    out.add(typed(p, it.get(), new Fun(f.op(), args)));
                }
                return out;
            }
            case Q q when q.decls().isEmpty() -> {
                return liftError(cx, q.body(), body -> isTBoolean(body.type())
                        ? List.of(Alt.err(pos, "The type of body of a quantified expression without local declarations"
                                + " must not be 'TBoolean'"))
                        : List.of(typed(p, Prim.TBoolean, new Q(q.quant(), List.of(), body))));
            }
            case Q q -> {
                List<Alt> out = new ArrayList<>();
                declAlternatives(cx, p, q, 0, new ArrayList<>(), out);
                return out;
            }
            default -> {
                return List.of();
            }
        }
    }

    private void declAlternatives(Ctx cx, P p, Q q, int i, List<D> done, List<Alt> out) {
        SourceSpan pos = p.span();
        if (i == q.decls().size()) {
            List<Map.Entry<String, Type>> extra = new ArrayList<>();
            for (D d : done) {
                for (String l : d.locals()) {
                    extra.add(Map.entry(l, d.body().type()));
                }
            }
            out.addAll(liftError(cx.withDecls(extra), q.body(), body -> isTBoolean(body.type())
                    ? List.of(typed(p, Prim.TBoolean, new Q(q.quant(), done, body)))
                    : List.of(Alt.err(pos, "The type of body of a quantified expression with local declarations must be"
                            + " 'TBoolean', instead it is\n" + showType(body)))));
            return;
        }
        D d = q.decls().get(i);
        for (P body : resolveTPExp(cx, d.body())) {
            if (isTBoolean(body.type())) {
                out.add(Alt.err(pos, "The type of declaration of a quantified expression must not be 'TBoolean'"));
                continue;
            }
            List<D> next = new ArrayList<>(done);
            next.add(new D(d.disj(), d.locals(), body));
            declAlternatives(cx, p, q, i + 1, next, out);
        }
    }

    private List<List<P>> product(Ctx cx, List<P> args) {
        List<List<P>> out = new ArrayList<>();
        out.add(List.of());
        for (P a : args) {
            List<List<P>> next = new ArrayList<>();
            List<P> alts = null;
            for (List<P> prefix : out) {
                if (alts == null) {
                    alts = resolveTPExp(cx, a);
                }
                for (P x : alts) {
                    List<P> l = new ArrayList<>(prefix);
                    l.add(x);
                    next.add(l);
                }
            }
            out = next;
        }
        return out;
    }

    private String topmost(String u) {
        C c = find(u);
        if (c == null) {
            return u;
        }
        List<C> h = findHierarchy(this::getSuper, c);
        return h.get(h.size() - 1).uid;
    }

    private List<Alt> unary(Ctx cx, P p, Fun f) {
        SourceSpan pos = p.span();
        String op = f.op();
        List<Alt> out = new ArrayList<>();
        for (P arg : resolveTPExp(cx, f.args().get(0))) {
            Type t = arg.type();
            String msg = "Function '" + op + "' cannot be performed on " + op + " '" + showType(arg) + "'";
            Type result;
            boolean ok;
            if (op.equals("!") || LtlUnOps.contains(op)) {
                ok = isTBoolean(t);
                result = Prim.TBoolean;
            } else if (op.equals("#")) {
                ok = true;
                result = Prim.TInteger;
            } else if (op.equals("sum")) {
                ok = is(t, Prim.TInteger) || is(t, Prim.TDecimal);
                result = is(t, Prim.TDecimal) ? Prim.TDecimal : Prim.TInteger;
            } else if (op.equals("product")) {
                ok = is(t, Prim.TInteger);
                result = Prim.TInteger;
            } else if (Set.of("-", "min", "max", "minimize", "maximize").contains(op)) {
                ok = numeric(t);
                result = t;
            } else {
                continue;
            }
            if (!ok) {
                out.add(Alt.err(pos, msg));
                continue;
            }
            out.add(typed(p, result, splitUnion(result, new Fun(op, List.of(arg)))));
        }
        return out;
    }

    private static E splitUnion(Type t, E e) {
        if (e instanceof Fun f && (f.op().equals("sum") || f.op().equals("product")) && f.args().size() == 1
                && f.args().get(0).exp() instanceof Fun u && u.op().equals("++") && u.args().size() == 2) {
            return new Fun(f.op().equals("sum") ? "+" : "*", List.of(part(t, f.op(), u.args().get(0)),
                    part(t, f.op(), u.args().get(1))));
        }
        return e;
    }

    private static P part(Type t, String op, P a) {
        if (a.exp() instanceof IntE || a.exp() instanceof DecimalE) {
            return a;
        }
        return new P(t, a.span(), splitUnion(t, new Fun(op, List.of(a))));
    }

    private static final Set<String> Comparisons = Set.of("<", ">", "<=", ">=");
    private static final Set<String> Arithmetic = Set.of("-", "*", "/", "%");

    private List<Alt> binary(Ctx cx, P p, Fun f) {
        SourceSpan pos = p.span();
        String op = f.op();
        List<Alt> out = new ArrayList<>();
        for (P a1 : resolveTPExp(cx, f.args().get(0))) {
            for (P a2 : resolveTPExp(cx, f.args().get(1))) {
                Type t1 = a1.type();
                Type t2 = a2.type();
                String msg = "Function '" + op + "' cannot be performed on\n" + showType(a1) + "\n" + op + "\n" + showType(a2);
                Alt err = Alt.err(pos, msg);
                Type result;
                try {
                    if (LogBinOps.contains(op) || LtlBinOps.contains(op)) {
                        result = isTBoolean(t1) && isTBoolean(t2) ? Prim.TBoolean : null;
                    } else if (Comparisons.contains(op)) {
                        result = numeric(t1) && numeric(t2) ? Prim.TBoolean : null;
                    } else if (op.equals("=") || op.equals("!=") || op.equals("in") || op.equals("not in")) {
                        result = testIntersect(t1, t2) != null ? Prim.TBoolean : null;
                    } else if (op.equals("--")) {
                        result = testIntersect(t1, t2) != null ? t1 : null;
                    } else if (op.equals("**") || op.equals("<:") || op.equals(":>")) {
                        result = testIntersect(t1, t2);
                    } else if (Arithmetic.contains(op)) {
                        result = allNumeric(t1) && allNumeric(t2) ? coerce(t1, t2) : null;
                    } else if (op.equals("+")) {
                        if (is(t1, Prim.TString) && is(t2, Prim.TString)) {
                            result = Prim.TString;
                        } else {
                            result = allNumeric(t1) && allNumeric(t2) ? coerce(t1, t2) : null;
                        }
                    } else {
                        continue;
                    }
                } catch (Drop d) {
                    continue;
                }
                out.add(result == null ? err : typed(p, result, new Fun(op, List.of(a1, a2))));
            }
        }
        List<Alt> sorted = new ArrayList<>(out);
        sorted.sort(java.util.Comparator.comparingInt((Alt a) -> a.ok() == null ? Integer.MAX_VALUE : viaClafer(a.ok().exp()))
                .thenComparingInt(a -> a.ok() == null ? Integer.MAX_VALUE : comparesReferences(a.ok().exp())));
        return sorted;
    }

    /**
     * The intersection of the types, or null if there is none or it is a
     * boolean.
     */
    private Type testIntersect(Type t1, Type t2) {
        Optional<Type> it = intersection(t1, t2);
        return it.isPresent() && !isTBoolean(it.get()) ? it.get() : null;
    }

    private static int viaClafer(E e) {
        if (e instanceof Fun f) {
            return (int) f.args().stream().filter(a -> a.type() != null && unionType(a.type()).contains(BaseClafer)).count();
        }
        return 0;
    }

    private int comparesReferences(E e) {
        if (e instanceof Fun f && f.args().size() == 2 && Set.of("=", "!=", "in", "not in", "**", "--").contains(f.op())
                && f.args().stream().allMatch(this::isReferenceNav)) {
            return 1;
        }
        return 0;
    }

    private boolean isReferenceNav(P p) {
        String last = lastName(p);
        if (last == null) {
            return false;
        }
        C c = find(last);
        return c != null && findHierarchy(this::getSuper, c).stream().anyMatch(x -> !getReference(x).isEmpty());
    }

    private static String lastName(P p) {
        if (p.exp() instanceof Fun f && f.op().equals(".") && f.args().size() == 2) {
            return lastName(f.args().get(1));
        }
        if (p.exp() instanceof Id i && !SpecialNames.contains(i.id()) && i.bind() != Bind.LOCAL) {
            return i.id();
        }
        return null;
    }

    /**
     * The type of the reference of a clafer, as the first phase of the type
     * checker gives it, or null.
     */
    private Type refType(C c) {
        if (c.refTyped) {
            return c.refType;
        }
        c.refTyped = true;
        Optional<Symbols.Reference> ref = c.symbol.reference();
        if (ref.isEmpty()) {
            return null;
        }
        P p = referenceExp(c);
        if (p == null) {
            return null;
        }
        Type t = p.type() == null ? null : addHierarchy(p.type());
        c.refType = t == null || isTBoolean(t) ? null : t;
        return c.refType;
    }

    private final Map<C, P> referenceExps = new IdentityHashMap<>();

    /**
     * The reference's target with its names resolved and its type, as the
     * first phase of the type checker leaves it; null after an error, which
     * is reported.
     */
    private P referenceExp(C c) {
        if (referenceExps.containsKey(c)) {
            return referenceExps.get(c);
        }
        referenceExps.put(c, null);
        Symbols.Reference ref = c.symbol.reference().orElseThrow();
        P result;
        try {
            P target;
            if (ref.primitive().isPresent() && !(ref.target() instanceof Syntax.ClaferId)
                    && !isSetExp(desugarExp(ref.target()).exp())) {
                // the type of a constant, -> 1 as -> integer = 1
                target = pexp(ref.span(), id(ref.primitive().get(), true, Bind.NONE, null));
            } else {
                target = desugarExp(ref.target());
            }
            Env env = new Env(c, List.of(), List.of(), List.of(), List.of(), null);
            P named = resolvePExp(env, target);
            result = resolveTPExp(new Ctx(c, null, List.of()), named).get(0);
        } catch (Abort a) {
            diagnostics.add(a.diagnostic);
            result = null;
        }
        referenceExps.put(c, result);
        return result;
    }

    // ======================================================================
    // The elements
    // ======================================================================
    /**
     * A constraint or an assertion of a clafer, or of the model: the
     * conjunction of its expressions, a formula.
     */
    Optional<Ir.Exp> constraint(List<Syntax.Exp> exps, ClaferSymbol clafer, boolean assertion) {
        return run(() -> {
            Syntax.Exp e = exps.get(0);
            for (int i = 1; i < exps.size(); i++) {
                e = new Syntax.Binary(exps.get(0).span(), Syntax.BinaryOp.And, e, exps.get(i));
            }
            C c = of(clafer);
            P named = resolvePExp(elementEnv(c), desugarPath(desugarExp(e)));
            P typed = resolveTPExp(new Ctx(c, null, List.of()), named).get(0);
            if (!isTBoolean(typed.type())) {
                throw new Abort(typed.span(), "A constraint requires an expression of type 'TBoolean' but got '"
                        + showType(typed) + "'");
            }
            return gen(constraintAfterRoot(root(c, typed, true)));
        });
    }

    /**
     * Keeps a constraint, an assertion or a transition as the compiler's IR
     * has it after {@code ResolverRoot}, for the scope analysis.
     */
    private P constraintAfterRoot(P rooted) {
        constraints.add(rooted);
        return rooted;
    }

    /**
     * An objective of a clafer, or of the model.
     */
    Optional<Ir.Exp> goal(Syntax.Goal g, ClaferSymbol clafer) {
        return run(() -> {
            boolean max = g.kind() == Syntax.GoalKind.Maximize || g.kind() == Syntax.GoalKind.MaxDeprecated;
            C c = of(clafer);
            P goal = desugarPath(pexp(g.span(), new Fun(max ? "maximize" : "minimize",
                    List.of(desugarExp(g.exps().get(0))))));
            P named = resolvePExp(elementEnv(c), goal);
            P typed = resolveTPExp(new Ctx(c, null, List.of()), named).get(0);
            P rooted = root(c, typed, false);
            return gen(((Fun) rooted.exp()).args().get(0));
        });
    }

    /**
     * The value of the initializer of a clafer: the equation {@code x = e},
     * in the context of the parent of {@code x}.
     */
    Optional<Ir.Exp> initializer(ClaferSymbol clafer, Syntax.Exp value, SourceSpan span) {
        return run(() -> {
            C c = of(clafer);
            P cId = pexp(clafer.nameSpan(), id(c.ident, false, Bind.NONE, null));
            P equation = pexp(span, new Fun("=", List.of(cId, desugarExp(value))));
            Env env = elementEnv(c).parentEnv();
            P named = resolvePExp(env, equation);
            C context = c.isTopLevel() ? root : find(c.parentUid);
            P typed = resolveTPExp(new Ctx(context, null, List.of()), named).get(0);
            if (!isTBoolean(typed.type())) {
                throw new Abort(typed.span(), "An initializer requires a value of the type of its reference, but got '"
                        + showType(typed) + "'");
            }
            P initial = ((Fun) typed.exp()).args().get(1);
            if (c.isAbstract && clafer.initializer().equals(Optional.of(Ir.InitKind.EACH)) && !atMostOne(initial)) {
                initialValueErrors.add(new Diagnostic(initial.span(), "The initializer ':=' of the abstract clafer "
                        + c.ident + " gives each instance of " + c.ident + " its initial value, which must have at"
                        + " most one element, but that could not be established for '" + written(initial)
                        + "' from the declarations (literals, clafers with at most one instance, and the operators"
                        + " that keep one element). To initialize all the instances of " + c.ident
                        + " together, write ':= together': abstract " + c.ident + " -> ... := together ..."));
            }
            P rooted = root(context == root ? null : context, typed, true);
            return gen(((Fun) rooted.exp()).args().get(1));
        });
    }

    // ------------------------------------------------------------------
    // The values with at most one element (InitializerChecks.atMostOne)
    // ------------------------------------------------------------------
    /**
     * Whether the value has at most one element, established from the
     * declarations and from the operators that keep one element, without the
     * constraints ({@code InitializerChecks.atMostOne}).
     */
    private boolean atMostOne(P p) {
        return switch (p.exp()) {
            case IntE i -> true;
            case DecimalE d -> true;
            case DoubleE d -> true;
            case StrE s -> true;
            case Id i when i.id().equals("this") || i.id().equals("parent") -> true;
            case Id i when i.bind() == Bind.GLOBAL -> {
                C c = i.bound() == null ? null : find(i.bound());
                yield c != null && c.isTopLevel() && single(c);
            }
            case Fun f when f.args().size() == 1 -> switch (f.op()) {
                case "#", "sum", "product", "max", "min" -> true;
                case "-" -> atMostOne(f.args().get(0));
                default -> false;
            };
            case Fun f when f.args().size() == 2 -> {
                P a = f.args().get(0);
                P b = f.args().get(1);
                yield switch (f.op()) {
                    case "." -> atMostOne(a) && step(b);
                    case "++" -> atMostOne(a) && atMostOne(b) && written(a).equals(written(b));
                    case "**" -> atMostOne(a) || atMostOne(b);
                    case "--" -> atMostOne(a);
                    case "+", "-", "*", "/", "%" -> atMostOne(a) && atMostOne(b);
                    default -> false;
                };
            }
            case Fun f when f.args().size() == 3 && f.op().equals("ifthenelse") ->
                atMostOne(f.args().get(1)) && atMostOne(f.args().get(2));
            default -> false;
        };
    }

    /**
     * A navigation step from at most one instance to at most one instance.
     */
    private boolean step(P p) {
        if (p.exp() instanceof Id i) {
            if (i.id().equals("dref") || i.id().equals("parent")) {
                return true;
            }
            if (i.bind() == Bind.GLOBAL) {
                C c = i.bound() == null ? null : find(i.bound());
                return c != null && single(c);
            }
        }
        return false;
    }

    /**
     * At most one instance under each parent.
     */
    private static boolean single(C c) {
        return !c.isAbstract && (c.card.high() == 0 || c.card.high() == 1);
    }

    /**
     * An expression as the compiler writes it in a message: as
     * {@code printTree} prints it, without the spaces around {@code .} and
     * {@code ..} ({@code InitializerChecks.written}).
     */
    static String written(P p) {
        return SyntaxPrinter.print(sugar(p)).replace(" . ", ".").replace(" .. ", "..");
    }

    /**
     * An expression in the syntax ({@code Desugarer.sugarExp}).
     */
    private static Syntax.Exp sugar(P p) {
        SourceSpan s = p.span();
        return switch (p.exp()) {
            case Id i -> {
                List<Syntax.Ident> path = new ArrayList<>();
                if (!i.mod().isEmpty()) {
                    path.add(new Syntax.Ident(s, i.mod()));
                }
                path.add(new Syntax.Ident(s, i.id()));
                yield new Syntax.ClaferId(s, new Syntax.Name(s, path));
            }
            case IntE i -> new Syntax.Literal(s, Syntax.LiteralKind.Int, Long.toString(i.value()));
            case DoubleE d -> new Syntax.Literal(s, Syntax.LiteralKind.Double, Double.toString(d.value()));
            case DecimalE d -> new Syntax.Literal(s, Syntax.LiteralKind.Real, d.text());
            case StrE t -> new Syntax.Literal(s, Syntax.LiteralKind.Str, t.text());
            case Fun f when f.op().equals("ifthenelse") && f.args().size() == 3 ->
                new Syntax.IfThenElse(s, sugar(f.args().get(0)), sugar(f.args().get(1)), sugar(f.args().get(2)));
            case Fun f when f.args().size() == 1 -> {
                Syntax.Exp a = sugar(f.args().get(0));
                yield switch (f.op()) {
                    case "initially" -> new Syntax.Anchor(s, true, a);
                    case "finally" -> new Syntax.Anchor(s, false, a);
                    default -> new Syntax.Unary(s, sugarUnOp(f.op()), a);
                };
            }
            case Fun f when f.args().size() == 2 ->
                new Syntax.Binary(s, sugarBinOp(f.op()), sugar(f.args().get(0)), sugar(f.args().get(1)));
            case Q q when q.decls().isEmpty() -> new Syntax.QuantExp(s, sugarQuant(q.quant()), sugar(q.body()));
            case Q q when q.decls().size() == 1 -> {
                D d = q.decls().get(0);
                List<Syntax.Ident> locals = d.locals().stream().map(l -> new Syntax.Ident(s, l)).toList();
                yield new Syntax.Quantified(s, sugarQuant(q.quant()), d.disj(),
                        new Syntax.Decl(s, locals, sugar(d.body())), sugar(q.body()));
            }
            default -> throw new IllegalArgumentException("Desugarer.sugarExp': invalid argument: " + p.exp());
        };
    }

    private static Syntax.UnaryOp sugarUnOp(String op) {
        return switch (op) {
            case "!" -> Syntax.UnaryOp.Neg;
            case "#" -> Syntax.UnaryOp.Card;
            case "-" -> Syntax.UnaryOp.Minus;
            case "max" -> Syntax.UnaryOp.GMax;
            case "min" -> Syntax.UnaryOp.GMin;
            case "sum" -> Syntax.UnaryOp.Sum;
            case "product" -> Syntax.UnaryOp.Product;
            case "F" -> Syntax.UnaryOp.LtlF;
            case "G" -> Syntax.UnaryOp.LtlG;
            case "X" -> Syntax.UnaryOp.LtlX;
            case "previous" -> Syntax.UnaryOp.Previous;
            case "once" -> Syntax.UnaryOp.Once;
            case "historically" -> Syntax.UnaryOp.Historically;
            default -> throw new IllegalArgumentException(op + "is not an op");
        };
    }

    private static Syntax.BinaryOp sugarBinOp(String op) {
        for (Syntax.BinaryOp b : Syntax.BinaryOp.values()) {
            if (b.syntax().equals(op)) {
                return b;
            }
        }
        throw new IllegalArgumentException(op + "is not an op");
    }

    private static Syntax.Quant sugarQuant(Ir.Quantifier q) {
        return switch (q) {
            case ALL -> Syntax.Quant.All;
            case SOME -> Syntax.Quant.Some;
            case NO -> Syntax.Quant.No;
            case LONE -> Syntax.Quant.Lone;
            case ONE -> Syntax.Quant.One;
        };
    }

    /**
     * The transition of a clafer, a constraint of the clafer whose source is
     * its instance: {@code this --> e}.
     */
    Optional<Ir.Exp> transition(ClaferSymbol clafer, Syntax.ClaferTransition t) {
        return run(() -> {
            C c = of(clafer);
            SourceSpan name = clafer.nameSpan();
            SourceSpan span = new SourceSpan(name.line(), name.column(), t.span().endLine(), t.span().endColumn());
            P exp = desugarExp(new Syntax.TransitionExp(span, thisAt(span), t.arrow(), t.exp()));
            P named = resolvePExp(elementEnv(c), exp);
            P typed = resolveTPExp(new Ctx(c, null, List.of()), named).get(0);
            if (!isTBoolean(typed.type())) {
                throw new Abort(typed.span(), "A constraint requires an expression of type 'TBoolean' but got '"
                        + showType(typed) + "'");
            }
            return gen(constraintAfterRoot(root(c, typed, true)));
        });
    }

    /**
     * The constraints of a reference to a set expression
     * ({@code genSetExpTarget}): for primitives, a formula of
     * {@code this.dref = v}, none for a primitive type, or {@code no this};
     * for clafers, {@code this.dref in e}.
     */
    List<Ir.Exp> referenceConstraints(ClaferSymbol clafer) {
        C c = of(clafer);
        P ref = referenceExp(c);
        if (ref == null) {
            return List.of();
        }
        try {
            SourceSpan span = ref.span();
            Ir.Exp dref = new Ir.Dref(span, new Ir.This(span));
            if (getRefIds(ref).stream().anyMatch(PrimitiveTypes::contains)) {
                Member m = member(ref, dref);
                if (m.formula != null) {
                    return List.of(m.formula);
                }
                return m.truth ? List.of() : List.of(new Ir.QuantifiedSet(span, Ir.Quantifier.NO, new Ir.This(span)));
            }
            return List.of(new Ir.Binary(span, Ir.BinaryOp.IN, List.of(dref, gen(ref))));
        } catch (Abort a) {
            diagnostics.add(a.diagnostic);
            return List.of();
        }
    }

    private static List<String> getRefIds(P p) {
        return switch (p.exp()) {
            case Id i -> List.of(i.id());
            case Fun f when f.args().size() == 2 && f.op().equals(".") -> getRefIds(f.args().get(1));
            case Fun f when f.args().size() == 2 && Set.of("++", "--", "**").contains(f.op()) -> {
                List<String> l = new ArrayList<>(getRefIds(f.args().get(0)));
                l.addAll(getRefIds(f.args().get(1)));
                yield l;
            }
            case IntE i -> List.of("integer");
            case DoubleE d -> List.of("double");
            case DecimalE d -> List.of("decimal");
            case StrE s -> List.of("string");
            case Q q when q.quant() == Ir.Quantifier.SOME -> List.of();
            default -> throw new Abort(p.span(), "[Bug] Commmon.getRefIds called on unexpected argument");
        };
    }

    private record Member(Ir.Exp formula, boolean truth) {
    }

    private Member member(P e, Ir.Exp dref) {
        SourceSpan span = e.span();
        if (e.exp() instanceof Fun f && f.args().size() == 2 && Set.of("++", "**", "--").contains(f.op())) {
            Member a = member(f.args().get(0), dref);
            Member b = member(f.args().get(1), dref);
            return switch (f.op()) {
                case "++" -> or(span, a, b);
                case "**" -> and(span, a, b);
                default -> and(span, a, not(span, b));
            };
        }
        if (e.exp() instanceof Id i && PrimitiveTypes.contains(i.id())) {
            return new Member(null, true);
        }
        return new Member(new Ir.Binary(span, Ir.BinaryOp.EQ, List.of(dref, gen(e))), false);
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

    private Optional<Ir.Exp> run(java.util.function.Supplier<Ir.Exp> f) {
        try {
            return Optional.of(f.get());
        } catch (Abort a) {
            diagnostics.add(a.diagnostic);
            return Optional.empty();
        }
    }

    // ======================================================================
    // The root (ResolverRoot.hs)
    // ======================================================================
    /**
     * The root's rewrites of an expression of a clafer, or of the model if
     * the clafer is the root; none in a temporal model.
     */
    private P root(C context, P p, boolean isConstraint) {
        if (dynamic) {
            return p;
        }
        return new RootRewriter(context == root ? null : context).rewrite(p, isConstraint);
    }

    private static boolean isDynamic(Symbols.Result symbols, Syntax.Model model) {
        for (ClaferSymbol s : symbols.all()) {
            if (s.isInitial() || s.isFinal()) {
                return true;
            }
        }
        boolean[] found = {false};
        forEachExp(model, e -> {
            switch (e) {
                case Syntax.TransitionExp t -> found[0] = true;
                case Syntax.Pattern p -> found[0] = true;
                case Syntax.Anchor a -> found[0] = true;
                case Syntax.Binary b -> found[0] |= Set.of("U", "W", "since", "triggered").contains(binOp(b.op()));
                case Syntax.Unary u -> found[0] |= Set.of("X", "G", "F", "previous", "once", "historically")
                        .contains(unOp(u.op()));
                default -> {
                }
            }
        });
        for (Syntax.Declaration d : model.declarations()) {
            if (d instanceof Syntax.Element el) {
                found[0] |= hasTransition(el);
            }
        }
        return found[0];
    }

    private static boolean hasTransition(Syntax.Element e) {
        if (e instanceof Syntax.Clafer c) {
            if (c.transition().isPresent()) {
                return true;
            }
            return c.elements().map(es -> es.elements().stream().anyMatch(Resolver::hasTransition)).orElse(false);
        }
        if (e instanceof Syntax.ClaferUse u) {
            return u.elements().map(es -> es.elements().stream().anyMatch(Resolver::hasTransition)).orElse(false);
        }
        return false;
    }

    /**
     * Applies the function to every expression of the model.
     */
    static void forEachExp(Syntax.Model model, java.util.function.Consumer<Syntax.Exp> f) {
        for (Syntax.Declaration d : model.declarations()) {
            if (d instanceof Syntax.Element e) {
                forEachExp(e, f);
            }
        }
    }

    private static void forEachExp(Syntax.Element e, java.util.function.Consumer<Syntax.Exp> f) {
        switch (e) {
            case Syntax.Clafer c -> {
                c.superClafer().ifPresent(s -> walk(s.exp(), f));
                c.reference().ifPresent(r -> walk(r.exp(), f));
                c.init().ifPresent(i -> walk(i.exp(), f));
                c.transition().ifPresent(t -> walk(t.exp(), f));
                c.elements().ifPresent(es -> es.elements().forEach(x -> forEachExp(x, f)));
            }
            case Syntax.ClaferUse u -> u.elements().ifPresent(es -> es.elements().forEach(x -> forEachExp(x, f)));
            case Syntax.Constraint c -> c.exps().forEach(x -> walk(x, f));
            case Syntax.Assertion a -> a.exps().forEach(x -> walk(x, f));
            case Syntax.Goal g -> g.exps().forEach(x -> walk(x, f));
        }
    }

    private static void walk(Syntax.Exp x, java.util.function.Consumer<Syntax.Exp> f) {
        f.accept(x);
        mapChildren(x, c -> {
            walk(c, f);
            return c;
        });
    }

    private static long maxFractionalDigits(Syntax.Model model) {
        long[] max = {0};
        forEachExp(model, e -> {
            if (e instanceof Syntax.Literal l && l.kind() == Syntax.LiteralKind.Real) {
                int dot = l.text().indexOf('.');
                max[0] = Math.max(max[0], dot < 0 ? 0 : l.text().length() - dot - 1);
            }
        });
        return max[0];
    }

    /**
     * Whether the parent of {@code this} is the root, in one part of a split
     * constraint.
     */
    private record ThisParent(boolean unsplit, C nested) {

        static final ThisParent Unsplit = new ThisParent(true, null);
        static final ThisParent RootParent = new ThisParent(false, null);
    }

    /**
     * The parent of the instances of a top-level clafer: the root for a
     * concrete one, and for an abstract one, an instance when its concrete
     * subclafers are nested, the root when they are at the top level, or
     * both ({@code ParentKind}).
     */
    private record ParentKind(boolean real, boolean root, List<C> tops, List<C> nested) {
    }

    private ParentKind parentKind(String uid) {
        List<C> subs = new ArrayList<>();
        uidMap.values().stream().sorted(java.util.Comparator.comparing((C c) -> c.uid)).forEach(c -> {
            if (!c.isAbstract && !c.uid.equals(RootIdent)) {
                List<C> h = findHierarchy(this::getSuper, c);
                if (h.subList(1, h.size()).stream().anyMatch(x -> x.uid.equals(uid))) {
                    subs.add(c);
                }
            }
        });
        List<C> tops = subs.stream().filter(C::isRootChild).toList();
        List<C> nested = subs.stream().filter(c -> !c.isRootChild()).toList();
        if (tops.isEmpty()) {
            return new ParentKind(true, false, tops, nested);
        }
        if (nested.isEmpty()) {
            return new ParentKind(false, true, tops, nested);
        }
        return new ParentKind(false, false, tops, nested);
    }

    private static boolean isMixed(ParentKind k) {
        return k != null && !k.real() && !k.root();
    }

    private static boolean isRootKind(ParentKind k) {
        return k != null && k.root();
    }

    private final class RootRewriter {

        private final C ctx;

        RootRewriter(C ctx) {
            this.ctx = ctx;
        }

        P rewrite(P pexp, boolean isConstraint) {
            if (isConstraint && ctx != null) {
                ParentKind kind = parentKind(ctx.uid);
                if (isMixed(kind) && takesParentOfThis(pexp)) {
                    SourceSpan s = pexp.span();
                    List<P> parts = new ArrayList<>();
                    parts.add(implies(s, kind.tops(), rootPExp(ThisParent.RootParent, pexp)));
                    for (C n : kind.nested()) {
                        parts.add(implies(s, List.of(n), rootPExp(new ThisParent(false, n), pexp)));
                    }
                    P acc = parts.get(0);
                    for (int i = 1; i < parts.size(); i++) {
                        acc = new P(Prim.TBoolean, s, new Fun("&&", List.of(acc, parts.get(i))));
                    }
                    return pexp.withExp(acc.exp());
                }
            }
            return rootPExp(ThisParent.Unsplit, pexp);
        }

        private P implies(SourceSpan s, List<C> cs, P e) {
            P in = new P(Prim.TBoolean, s, new Fun("in", List.of(thisOf(s, ctx), unionOf(s, cs))));
            return new P(Prim.TBoolean, s, new Fun("=>", List.of(in, e)));
        }

        private P rootPExp(ThisParent split, P pexp) {
            SourceSpan pos = pexp.span();
            E exp = pexp.exp();
            if (exp instanceof Fun f && f.op().equals(".") && f.args().size() == 2 && isId(f.args().get(1), "parent")) {
                P r = f.args().get(1);
                P l = rootPExp(split, f.args().get(0));
                ParentKind k = parentKindOf(l);
                if (isRootKind(k) && (isSingle(l) || alwaysExists(l))) {
                    return toRoot(pexp);
                }
                if (isMixed(k) && isId(l, "this") && !split.unsplit() && split.nested() == null) {
                    return toRoot(pexp);
                }
                if (isMixed(k) && isId(l, "this") && split.nested() != null) {
                    P inter = l.withExp(new Fun("**", List.of(l, unionOf(pos, List.of(split.nested())))));
                    return pexp.withExp(join(inter, r));
                }
                if (isRootKind(k)) {
                    String n = claferNames(l);
                    throw new Abort(pos, "The parent of " + n + " is the root when an instance exists, and nothing"
                            + " otherwise: it can be navigated from, quantified or counted, e.g., `" + n
                            + ".parent.a`, `some " + n + ".parent` or `#" + n + ".parent`");
                }
                if (isMixed(k)) {
                    throw new Abort(pos, "The parent of an instance of " + claferNames(l) + " is the root for its"
                            + " top-level subclafers (" + String.join(" ", k.tops().stream().map(c -> c.ident).toList())
                            + ") and an instance for the others: take the parent of `this` in a constraint of "
                            + claferNames(l));
                }
                return pexp.withExp(join(l, r));
            }
            if (exp instanceof Fun f && f.op().equals(".") && f.args().size() == 2
                    && f.args().get(0).exp() instanceof Fun g && g.op().equals(".") && g.args().size() == 2
                    && isId(g.args().get(1), "parent") && isRootKind(parentKindOf(g.args().get(0)))
                    && !(isSingle(g.args().get(0)) || alwaysExists(g.args().get(0)))) {
                P l = rootPExp(split, g.args().get(0));
                P r = rootPExp(split, f.args().get(1));
                P nav = pexp.withExp(join(toRoot(l), r));
                P noClafer = new P(NoClaferT, pos, noClaferIExp());
                return pexp.withExp(new Fun("ifthenelse", List.of(exists(l), nav, noClafer)));
            }
            if (exp instanceof Fun f && f.op().equals(".") && f.args().size() == 2) {
                P l = rootPExp(split, f.args().get(0));
                P r = rootPExp(split, f.args().get(1));
                if (isId(r, RootIdent)) {
                    throw new Abort(pos, "The root is not a child of any clafer");
                }
                return pexp.withExp(join(l, r));
            }
            if (exp instanceof Fun f && f.op().equals("#") && f.args().size() == 1
                    && f.args().get(0).exp() instanceof Fun g && g.op().equals(".") && g.args().size() == 2
                    && isId(g.args().get(1), "parent") && isRootKind(parentKindOf(g.args().get(0)))
                    && !(isSingle(g.args().get(0)) || alwaysExists(g.args().get(0)))) {
                P l = rootPExp(split, g.args().get(0));
                return new P(Prim.TInteger, pos, new Fun("ifthenelse", List.of(exists(l), integer(pos, 1),
                        integer(pos, 0))));
            }
            if (exp instanceof Fun f && f.op().equals("#") && f.args().size() == 1) {
                P e = rootPExp(split, f.args().get(0));
                return isId(e, RootIdent) ? new P(Prim.TInteger, pos, new IntE(1)) : pexp.withExp(new Fun("#", List.of(e)));
            }
            if (exp instanceof Fun f) {
                List<P> es = f.args().stream().map(a -> rootPExp(split, a)).toList();
                if (es.stream().anyMatch(a -> isId(a, RootIdent))) {
                    throw new Abort(pos, "The root has no instance: `root` can be navigated from, e.g., `root.a`,"
                            + " counted (`#root` is 1) and quantified (`some root` holds), but not used in '"
                            + f.op() + "'");
                }
                if (Set.of("=", "!=", "<", ">", "<=", ">=").contains(f.op()) && es.size() == 2
                        && es.get(0).exp() instanceof IntE m && es.get(1).exp() instanceof IntE n
                        && (becameInt(f.args().get(0), es.get(0)) || becameInt(f.args().get(1), es.get(1)))) {
                    boolean b = switch (f.op()) {
                        case "=" -> m.value() == n.value();
                        case "!=" -> m.value() != n.value();
                        case "<" -> m.value() < n.value();
                        case ">" -> m.value() > n.value();
                        case "<=" -> m.value() <= n.value();
                        default -> m.value() >= n.value();
                    };
                    return new P(Prim.TBoolean, pos, constant(pos, b));
                }
                return pexp.withExp(new Fun(f.op(), es));
            }
            if (exp instanceof Q q && q.decls().isEmpty() && q.body().exp() instanceof Fun g && g.op().equals(".")
                    && g.args().size() == 2 && isId(g.args().get(1), "parent")
                    && isRootKind(parentKindOf(g.args().get(0)))
                    && !(isSingle(g.args().get(0)) || alwaysExists(g.args().get(0)))) {
                P l = rootPExp(split, g.args().get(0));
                return switch (q.quant()) {
                    case LONE -> new P(Prim.TBoolean, pos, constant(pos, true));
                    case NO -> pexp.withExp(new Q(Ir.Quantifier.NO, List.of(), l));
                    default -> pexp.withExp(new Q(Ir.Quantifier.SOME, List.of(), l));
                };
            }
            if (exp instanceof Q q && q.decls().isEmpty()) {
                P e = rootPExp(split, q.body());
                return isId(e, RootIdent) ? new P(Prim.TBoolean, pos, constant(pos, q.quant() != Ir.Quantifier.NO))
                        : pexp.withExp(new Q(q.quant(), List.of(), e));
            }
            if (exp instanceof Q q) {
                List<D> ds = new ArrayList<>();
                for (D d : q.decls()) {
                    P body = rootPExp(split, d.body());
                    if (isId(body, RootIdent)) {
                        throw new Abort(pos, "A variable cannot range over the root, which has no instance");
                    }
                    ds.add(new D(d.disj(), d.locals(), body));
                }
                return pexp.withExp(new Q(q.quant(), ds, rootPExp(split, q.body())));
            }
            if (isId(pexp, BaseClafer)) {
                List<C> tops = topClafers();
                if (tops.isEmpty()) {
                    return new P(NoClaferT, pos, noClaferIExp());
                }
                P acc = instancesOf(pos, tops.get(0));
                for (int i = 1; i < tops.size(); i++) {
                    P r = instancesOf(pos, tops.get(i));
                    acc = new P(acc.type() == null || r.type() == null ? null : union(acc.type(), r.type()), pos,
                            new Fun("++", List.of(acc, r)));
                }
                return new P(acc.type(), pos, acc.exp());
            }
            return pexp;
        }

        private P toRoot(P p) {
            return new P(RootT, p.span(), rootIExp());
        }

        private P exists(P l) {
            return new P(Prim.TBoolean, l.span(), new Q(Ir.Quantifier.SOME, List.of(), l));
        }
    }

    private static P integer(SourceSpan span, long n) {
        return new P(Prim.TInteger, span, new IntE(n));
    }

    /**
     * True as {@code 0 < 1}, false as {@code 1 < 0}.
     */
    private static E constant(SourceSpan span, boolean b) {
        return new Fun("<", b ? List.of(integer(span, 0), integer(span, 1)) : List.of(integer(span, 1), integer(span, 0)));
    }

    private static boolean becameInt(P before, P after) {
        return !(before.exp() instanceof IntE) && after.exp() instanceof IntE;
    }

    private static boolean isSingle(P p) {
        return p.exp() instanceof Id i && (i.bind() == Bind.LOCAL || i.id().equals("this"));
    }

    private boolean alwaysExists(P p) {
        if (p.exp() instanceof Id i && i.bind() == Bind.GLOBAL) {
            C c = find(i.id());
            return c != null && c.isRootChild() && c.card.low() >= 1;
        }
        return false;
    }

    private ParentKind parentKindOf(P p) {
        if (p.exp() instanceof Fun f && f.op().equals("**") && f.args().size() == 2) {
            return parentKindOf(f.args().get(1));
        }
        if (p.type() instanceof TClafer t && !t.hi().isEmpty()) {
            List<C> cs = t.hi().stream().map(this::find).toList();
            if (cs.stream().allMatch(c -> c != null && c.isRootChild())) {
                return new ParentKind(false, true, List.of(), List.of());
            }
            if (t.hi().size() == 1 && cs.get(0) != null && cs.get(0).isTopLevel() && cs.get(0).isAbstract) {
                return parentKind(cs.get(0).uid);
            }
        }
        return null;
    }

    private String claferNames(P p) {
        if (p.type() instanceof TClafer t) {
            return String.join(" ", t.hi().stream().map(u -> {
                C c = find(u);
                return c == null ? u : c.ident;
            }).toList());
        }
        return "the expression";
    }

    private static boolean takesParentOfThis(P p) {
        return switch (p.exp()) {
            case Fun f when f.op().equals(".") && f.args().size() == 2 && isId(f.args().get(1), "parent")
                    && isId(f.args().get(0), "this") -> true;
            case Fun f -> f.args().stream().anyMatch(Resolver::takesParentOfThis);
            case Q q -> q.decls().stream().anyMatch(d -> takesParentOfThis(d.body())) || takesParentOfThis(q.body());
            default -> false;
        };
    }

    private static P thisOf(SourceSpan s, C c) {
        return new P(new TClafer(List.of(c.uid)), s, id("this", true, Bind.GLOBAL, c.uid));
    }

    private P unionOf(SourceSpan s, List<C> cs) {
        P acc = instancesOf(s, cs.get(0));
        for (int i = 1; i < cs.size(); i++) {
            acc = new P(new TClafer(List.of()), s, new Fun("++", List.of(acc, instancesOf(s, cs.get(i)))));
        }
        return acc;
    }

    /**
     * All the instances of a clafer, a nested one by its path from the top
     * level, e.g., {@code g.f2}.
     */
    private P instancesOf(SourceSpan s, C c) {
        List<C> ancestors = new ArrayList<>();
        C x = c;
        while (true) {
            ancestors.add(x);
            C p = find(x.parentUid);
            if (p != null && !x.isTopLevel() && !p.uid.equals(RootIdent)) {
                x = p;
            } else {
                break;
            }
        }
        java.util.Collections.reverse(ancestors);
        P acc = null;
        for (C a : ancestors) {
            P id = new P(new TClafer(List.of(a.uid)), s, id(a.uid, a.isTopLevel(), Bind.GLOBAL, a.uid));
            acc = acc == null ? id : new P(id.type(), s, join(acc, id));
        }
        return acc;
    }

    /**
     * The clafers with no super clafer, in the order of the model.
     */
    private List<C> topClafers() {
        List<C> out = new ArrayList<>();
        for (C c : topLevel) {
            preOrder(c, out);
        }
        return out;
    }

    private void preOrder(C c, List<C> out) {
        if (c.superUid == null) {
            out.add(c);
        }
        for (C child : c.children) {
            preOrder(child, out);
        }
    }

    // ======================================================================
    // The expression of the explicit model (ChocoModel.hs)
    // ======================================================================
    private static P rewrite(P p1) {
        if (p1.type() != null && p1.exp() instanceof Fun f && f.op().equals(".") && f.args().size() == 2
                && f.args().get(1).exp() instanceof Fun g && g.op().equals(".")) {
            P p2 = f.args().get(0);
            P p3 = f.args().get(1);
            P r = rewrite(p3);
            if (r.exp() instanceof Fun h && h.op().equals(".") && h.args().size() == 2) {
                P p4 = h.args().get(0);
                P p5 = h.args().get(1);
                return p1.withExp(join(new P(p4.type(), p3.span(), join(p2, p4)), p5));
            }
            return p1;
        }
        if (p1.exp() instanceof Fun f && f.op().equals("-") && f.args().size() == 1
                && f.args().get(0).exp() instanceof IntE i) {
            return p1.withExp(new IntE(-i.value()));
        }
        return p1;
    }

    Ir.Exp gen(P p) {
        P r = rewrite(p);
        return genExp(r.span(), r.exp());
    }

    private Ir.Exp genExp(SourceSpan span, E e) {
        return switch (e) {
            case Q q when q.decls().isEmpty() -> new Ir.QuantifiedSet(span, q.quant(), gen(q.body()));
            case Q q -> new Ir.Quantified(span, q.quant(), q.decls().stream().map(d -> new Ir.Decl(d.body().span(),
                    d.disj(), d.locals().stream().map(l -> new Ir.Name(d.body().span(), l)).toList(), gen(d.body())))
                    .toList(), gen(q.body()));
            case Fun f when f.op().equals(".") && f.args().size() == 2 -> {
                P l = f.args().get(0);
                P r = f.args().get(1);
                if (isId(l, RootIdent)) {
                    yield gen(r);
                }
                if (isId(r, "dref")) {
                    yield new Ir.Dref(span, gen(l));
                }
                if (isId(r, "parent")) {
                    yield new Ir.ParentOf(span, gen(l));
                }
                if (r.exp() instanceof Id i) {
                    yield new Ir.Join(span, gen(l), new Ir.NameExp(r.span(), i.id()));
                }
                throw new Abort(span, "Did not rewrite all joins to left joins.");
            }
            case Fun f when f.op().equals("-") && f.args().size() == 1 -> new Ir.Unary(span, Ir.UnaryOp.MINUS,
                    gen(f.args().get(0)));
            case Fun f when (f.op().equals("sum") || f.op().equals("product")) && f.args().size() == 1 -> {
                P arg = rewrite(f.args().get(0));
                Ir.UnaryOp op = f.op().equals("sum") ? Ir.UnaryOp.SUM : Ir.UnaryOp.PRODUCT;
                if (arg.exp() instanceof Fun a && a.args().size() == 2 && isId(a.args().get(1), "dref")) {
                    yield new Ir.Unary(span, op, gen(a.args().get(0)));
                }
                if (op == Ir.UnaryOp.PRODUCT) {
                    throw new Abort(span, "Choco: Unexpected product argument.");
                }
                yield new Ir.Unary(span, op, gen(f.args().get(0)));
            }
            case Fun f when f.op().equals("+") && f.args().get(0).type() == Prim.TString -> new Ir.Call(span, "concat",
                    f.args().stream().map(this::gen).toList());
            case Fun f -> fun(span, f);
            case Id i when i.id().equals(NoClaferIdent) -> new Ir.NoClafer(span);
            case Id i when i.id().equals("this") -> new Ir.This(span);
            case Id i -> new Ir.NameExp(span, i.id());
            case IntE i -> new Ir.IntLit(span, i.value());
            case StrE s -> new Ir.StringLit(span, s.text());
            case DoubleE d -> new Ir.DoubleLit(span, showEFloat(d.value()));
            case DecimalE d -> new Ir.DecimalLit(span, genDecimal(d.text()));
        };
    }

    private String genDecimal(String literal) {
        int dot = literal.indexOf('.');
        long digits = dot < 0 ? 0 : literal.length() - dot - 1;
        return literal + "0".repeat((int) Math.max(0, decimalScale - digits));
    }

    /**
     * A double as Haskell's {@code showEFloat Nothing} writes it, e.g.,
     * {@code 1.5e3}.
     */
    static String showEFloat(double v) {
        if (v == 0) {
            return (1 / v < 0 ? "-" : "") + "0.0e0";
        }
        BigDecimal d = new BigDecimal(Double.toString(Math.abs(v))).stripTrailingZeros();
        String digits = d.unscaledValue().toString();
        int exponent = digits.length() - d.scale() - 1;
        String mantissa = digits.substring(0, 1) + "." + (digits.length() > 1 ? digits.substring(1) : "0");
        return (v < 0 ? "-" : "") + mantissa + "e" + exponent;
    }

    private Ir.Exp fun(SourceSpan span, Fun f) {
        String op = f.op();
        List<Ir.Exp> args = f.args().stream().map(a -> isId(a, NoClaferIdent) ? (Ir.Exp) new Ir.NoClafer(a.span())
                : gen(a)).toList();
        Ir.BinaryOp bin = switch (op) {
            case "<=>" -> Ir.BinaryOp.IFF;
            case "=>" -> Ir.BinaryOp.IMPLIES;
            case "||" -> Ir.BinaryOp.OR;
            case "xor" -> Ir.BinaryOp.XOR;
            case "&&" -> Ir.BinaryOp.AND;
            case "<" -> Ir.BinaryOp.LT;
            case ">" -> Ir.BinaryOp.GT;
            case "=" -> Ir.BinaryOp.EQ;
            case "<=" -> Ir.BinaryOp.LE;
            case ">=" -> Ir.BinaryOp.GE;
            case "!=" -> Ir.BinaryOp.NE;
            case "in" -> Ir.BinaryOp.IN;
            case "not in" -> Ir.BinaryOp.NOT_IN;
            case "+" -> Ir.BinaryOp.ADD;
            case "-" -> Ir.BinaryOp.SUB;
            case "*" -> Ir.BinaryOp.MUL;
            case "/" -> Ir.BinaryOp.DIV;
            case "%" -> Ir.BinaryOp.MOD;
            case "++" -> Ir.BinaryOp.UNION;
            case "--" -> Ir.BinaryOp.DIFF;
            case "**" -> Ir.BinaryOp.INTER;
            case "<:" -> Ir.BinaryOp.DOMAIN;
            case ":>" -> Ir.BinaryOp.RANGE;
            default -> null;
        };
        if (bin != null && args.size() == 2) {
            return new Ir.Binary(span, bin, args);
        }
        switch (op) {
            case "!":
                return new Ir.Unary(span, Ir.UnaryOp.NOT, args.get(0));
            case "#":
                return new Ir.Unary(span, Ir.UnaryOp.CARD, args.get(0));
            case "min":
                return new Ir.Unary(span, Ir.UnaryOp.MIN, args.get(0));
            case "max":
                return new Ir.Unary(span, Ir.UnaryOp.MAX, args.get(0));
            case "ifthenelse":
                return new Ir.IfThenElse(span, args.get(0), args.get(1), args.get(2));
            case "initially", "finally":
                return new Ir.Anchor(span, op.equals("initially"), args.get(0));
            case "-->":
                return new Ir.Transition(span, false, args.get(0), Optional.empty(), args.get(1));
            case "-[]->":
                return new Ir.Transition(span, false, args.get(0), Optional.of(args.get(1)), args.get(2));
            case "-->>":
                return new Ir.Transition(span, true, args.get(0), Optional.empty(), args.get(1));
            case "-[]->>":
                return new Ir.Transition(span, true, args.get(0), Optional.of(args.get(1)), args.get(2));
            default:
                break;
        }
        for (Ir.TemporalOp t : Ir.TemporalOp.values()) {
            if (t.keyword().equals(op) && t.arity() == args.size()) {
                return new Ir.Temporal(span, t, args);
            }
        }
        if (PatternOps.contains(op)) {
            int space = op.indexOf(' ');
            String kind = space < 0 ? op : op.substring(0, space);
            String scope = space < 0 ? "" : op.substring(space + 1);
            Ir.PatternKind k = Ir.PatternKind.valueOf(kind.toUpperCase());
            Ir.PatternScope s = switch (scope) {
                case "" -> Ir.PatternScope.GLOBALLY;
                case "before" -> Ir.PatternScope.BEFORE;
                case "after" -> Ir.PatternScope.AFTER;
                case "between" -> Ir.PatternScope.BETWEEN;
                default -> Ir.PatternScope.AFTER_UNTIL;
            };
            return new Ir.Pattern(span, k, s, args);
        }
        throw new Abort(span, "Choco: Unknown op: " + op);
    }
}
