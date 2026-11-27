package org.clafer.frontend;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import org.clafer.frontend.Resolver.C;
import org.clafer.frontend.Resolver.Fun;
import org.clafer.frontend.Resolver.Id;
import org.clafer.frontend.Resolver.IntE;
import org.clafer.frontend.Resolver.P;
import org.clafer.frontend.Resolver.Q;
import org.clafer.frontend.Symbols.Interval;

/**
 * The scopes of the explicit model's scope file, as the compiler computes
 * them: {@code getScopeStrategy ss (zeroUnusedAbstracts iModule)} on the rich
 * IR, a port of {@code Optimizer.zeroUnusedAbstracts},
 * {@code SimpleScopeAnalyzer.simpleScopeAnalysis} and
 * {@code ScopeAnalysis.withEmptyAbstracts}, function by function. The IR is
 * the resolver's mirror: its clafers and the constraints after
 * {@code ResolverRoot}.
 */
final class ScopeAnalysis {

    /**
     * An error of the compiler's analysis, which ends it, with the
     * compiler's message.
     */
    static final class Failure extends RuntimeException {

        Failure(String message) {
            super(message, null, false, false);
        }
    }

    /**
     * A clafer of the IR as the analysis reads it.
     */
    private static final class Cl {

        final String uid;
        final boolean isAbstract;
        long lb;
        long ub;
        final long gcardLb;
        final String superUid;
        final boolean hasReference;
        final boolean isSet;
        final List<String> refIds;
        final List<Cl> children = new ArrayList<>();

        Cl(String uid, boolean isAbstract, Interval card, long gcardLb, String superUid, boolean hasReference,
                boolean isSet, List<String> refIds) {
            this.uid = uid;
            this.isAbstract = isAbstract;
            this.lb = card.low();
            this.ub = card.high();
            this.gcardLb = gcardLb;
            this.superUid = superUid;
            this.hasReference = hasReference;
            this.isSet = isSet;
            this.refIds = refIds;
        }
    }

    private final Map<String, Cl> uidClaferMap = new HashMap<>();
    // the clafers of the module, in pre-order (universeOn biplate)
    private final List<Cl> clafers = new ArrayList<>();
    // the top-level declarations
    private final List<Cl> decls = new ArrayList<>();
    private final List<P> constraints;

    /**
     * @param resolver the resolver, after it resolved the constraints
     * @param elements the top-level elements of the explicit IR, whose
     * clafers are in the order of the compiler's IR
     */
    private ScopeAnalysis(Resolver resolver, List<Ir.Element> elements) {
        for (String prim : List.of("root", "clafer", "integer", "int", "string", "decimal", "double", "real",
                "boolean")) {
            C c = resolver.find(prim);
            uidClaferMap.put(prim, new Cl(prim, false, Interval.One, 0, c.superUid, false, false, List.of()));
        }
        for (Ir.Element e : elements) {
            switch (e) {
                // an enumeration is an abstract clafer followed by its literals
                case Ir.EnumDecl en -> {
                    decls.add(add(resolver, resolver.clafer(en.name().text()), List.of()));
                    for (Ir.Name literal : en.literals()) {
                        decls.add(add(resolver, resolver.clafer(literal.text()), List.of()));
                    }
                }
                case Ir.ClaferDecl c -> decls.add(add(resolver, c));
                default -> {
                }
            }
        }
        this.constraints = resolver.constraints;
    }

    private Cl add(Resolver resolver, Ir.ClaferDecl d) {
        return add(resolver, resolver.clafer(d.name().text()), d.elements());
    }

    private Cl add(Resolver resolver, C c, List<Ir.Element> elements) {
        Symbols.ClaferSymbol s = c.symbol;
        Optional<Symbols.Reference> ref = s.reference();
        Cl cl = new Cl(c.uid, c.isAbstract, c.card, s.groupCard().interval().low(), c.superUid, ref.isPresent(),
                ref.map(Symbols.Reference::isSet).orElse(false), c.refIds);
        uidClaferMap.putIfAbsent(c.uid, cl);
        clafers.add(cl);
        for (Ir.Element e : elements) {
            if (e instanceof Ir.ClaferDecl child) {
                cl.children.add(add(resolver, child));
            }
        }
        return cl;
    }

    /**
     * The scopes of the clafers, by UID, as {@code getScopeStrategy} gives
     * them: the scopes other than 1, and 0 for the empty abstract clafers.
     *
     * @param simple the strategy {@code simple} (the default), or
     * {@code none}
     */
    static Map<String, Long> scopes(Resolver resolver, List<Ir.Element> elements, boolean simple) {
        ScopeAnalysis a = new ScopeAnalysis(resolver, elements);
        a.zeroUnusedAbstracts();
        Map<String, Long> scopes = new TreeMap<>();
        if (simple) {
            a.simpleScopeAnalysis().forEach(scopes::put);
        }
        for (String u : a.emptyAbstracts()) {
            scopes.put(u, 0L);
        }
        return scopes;
    }

    private Cl findIClafer(String uid) {
        return uidClaferMap.get(uid);
    }

    private static List<String> getSuper(Cl c) {
        return c.superUid == null ? List.of() : List.of(c.superUid);
    }

    /**
     * {@code findHierarchy getSuper}: the clafer and its supers, ending at a
     * clafer already on the path.
     */
    private List<Cl> findHierarchy(Cl c) {
        List<Cl> out = new ArrayList<>();
        List<String> path = new ArrayList<>();
        Cl x = c;
        while (true) {
            out.add(x);
            if (path.contains(x.uid) || x.superUid == null) {
                return out;
            }
            path.add(x.uid);
            Cl s = findIClafer(x.superUid);
            if (s == null) {
                return out;
            }
            x = s;
        }
    }

    private Optional<Cl> directSuper(Cl c) {
        List<Cl> h = findHierarchy(c);
        return h.size() < 2 ? Optional.empty() : Optional.of(h.get(1));
    }

    // ======================================================================
    // Optimizer.zeroUnusedAbstracts, ScopeAnalysis.withEmptyAbstracts
    // ======================================================================
    /**
     * The top-level abstract clafers that no concrete top-level clafer
     * reaches through the supers of itself and its descendants get no
     * instances: the card 0..0.
     */
    private void zeroUnusedAbstracts() {
        List<Cl> maybeUsed = new ArrayList<>(decls);
        List<String> used = decls.stream().filter(c -> !c.isAbstract).map(c -> c.uid).toList();
        while (!used.isEmpty() && !maybeUsed.isEmpty()) {
            List<Cl> usedNow = new ArrayList<>();
            List<Cl> rest = new ArrayList<>();
            for (Cl c : maybeUsed) {
                (used.contains(c.uid) ? usedNow : rest).add(c);
            }
            maybeUsed = rest;
            LinkedHashSet<String> extended = new LinkedHashSet<>();
            for (Cl c : usedNow) {
                getExtended(c, extended);
            }
            used = new ArrayList<>(extended);
        }
        if (used.isEmpty()) {
            for (Cl c : maybeUsed) {
                c.lb = 0;
                c.ub = 0;
            }
        }
    }

    private static void getExtended(Cl c, Set<String> out) {
        out.addAll(getSuper(c));
        for (Cl child : c.children) {
            getExtended(child, out);
        }
    }

    /**
     * The abstract clafers that no concrete clafer inherits from, directly or
     * through other abstract clafers, and the clafers nested in them.
     */
    private Set<String> emptyAbstracts() {
        Set<String> inherited = new HashSet<>();
        for (Cl c : clafers) {
            if (!c.isAbstract) {
                List<Cl> h = findHierarchy(c);
                for (Cl s : h.subList(1, h.size())) {
                    inherited.add(s.uid);
                }
            }
        }
        Set<String> empty = new HashSet<>();
        for (Cl c : clafers) {
            if (c.isAbstract && !inherited.contains(c.uid)) {
                universe(c, empty);
            }
        }
        return empty;
    }

    private static void universe(Cl c, Set<String> out) {
        out.add(c.uid);
        for (Cl child : c.children) {
            universe(child, out);
        }
    }

    // ======================================================================
    // SimpleScopeAnalyzer.simpleScopeAnalysis
    // ======================================================================
    private Map<String, Long> upperCardsMap;
    private Map<String, Long> constraintAnalysis;
    private Map<String, List<String>> subclaferMap;
    private Map<String, String> parentMap;

    private long upperCards(String u) {
        Long ub = upperCardsMap.get(u);
        if (ub == null) {
            throw new Failure("No upper cardinality for clafer named \"" + u + "\".");
        }
        return ub;
    }

    private Map<String, Long> simpleScopeAnalysis() {
        upperCardsMap = new HashMap<>();
        for (Cl c : clafers) {
            upperCardsMap.put(c.uid, c.ub);
        }
        Map<String, Long> analysis = new HashMap<>();
        for (Cl d : decls) {
            analyzeSupers(analysis, d);
        }
        for (Cl d : decls) {
            analyzeRefs(analysis, d);
        }
        constraintAnalysis = analyzeConstraints();
        analyzeHierarchy();
        for (List<String> component : analyzeDependencies()) {
            analyzeComponent(analysis, component);
        }
        Map<String, Long> result = new TreeMap<>();
        withSetReferences(analysis).forEach((a, b) -> {
            if (b != 1) {
                result.put(a, b);
            }
        });
        return result;
    }

    /**
     * The targets of a set reference differ under each parent: when the
     * multiplicities bound its target, a set reference has at most as many
     * instances under a parent as its target has in all (#125).
     */
    private Map<String, Long> withSetReferences(Map<String, Long> analysis) {
        Map<String, Long> a = new HashMap<>(analysis);
        for (Cl c : clafers) {
            if (c.hasReference && c.isSet && c.refIds.size() == 1 && !c.isAbstract) {
                Cl t = findIClafer(c.refIds.get(0));
                if (t != null && bounded(t) && analysis.containsKey(t.uid)) {
                    long cap = parentScopeOf(analysis, c) * analysis.get(t.uid);
                    a.computeIfPresent(c.uid, (k, v) -> Math.min(cap, v));
                }
            }
        }
        return a;
    }

    /**
     * Whether the multiplicities bound the number of instances of the
     * clafer.
     */
    private boolean bounded(Cl c) {
        String parent = parentMap.get(c.uid);
        boolean boundedParent = (c.isAbstract || c.ub >= 0)
                && (parent == null || boundedUid(parent))
                && (!c.isAbstract || subclaferMap.getOrDefault(c.uid, List.of()).stream().allMatch(this::boundedUid));
        if (boundedParent) {
            return true;
        }
        for (Cl s : findHierarchy(c)) {
            if (s.isAbstract && s.ub > 0) {
                String sp = parentMap.get(s.uid);
                if (sp == null || boundedUid(sp)) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean boundedUid(String uid) {
        Cl c = findIClafer(uid);
        return c != null && bounded(c);
    }

    private long parentScopeOf(Map<String, Long> analysis, Cl c) {
        String p = parentMap.get(c.uid);
        return p == null ? 1 : analysis.getOrDefault(p, 1L);
    }

    private long lowerOrUpperFixedCard(Map<String, Long> analysis, Cl clafer) {
        long cardLb = clafer.lb;
        long cardUb = clafer.ub;
        long oneForStar = cardLb == 0 && cardUb == -1 ? 1 : 0;
        long targetScopeForStar = 0;
        if (clafer.hasReference && cardUb == -1) {
            if (clafer.refIds.size() == 1) {
                Cl t = findIClafer(clafer.refIds.get(0));
                targetScopeForStar = analysis.getOrDefault(t == null ? "unknown" : t.uid, 1L);
            }
        }
        long lowFromConstraints = constraintAnalysis.getOrDefault(clafer.uid, 0L);
        long lowFromGroupCard = 0;
        String parent = parentMap.get(clafer.uid);
        Cl p = parent == null ? null : findIClafer(parent);
        if (p != null) {
            long groupLb = p.gcardLb;
            lowFromGroupCard = cardUb == -1 ? groupLb : Math.min(groupLb, cardUb);
        }
        long low = 0;
        for (Cl s : findHierarchy(clafer)) {
            if (s.isAbstract) {
                low = Math.max(low, s.ub > 0 ? s.ub : s.lb);
            }
        }
        long lowFromAbstractSupers = cardUb == -1 ? low : Math.min(low, cardUb);
        return Arrays.stream(new long[]{cardLb, cardUb, lowFromConstraints, oneForStar, targetScopeForStar,
            lowFromGroupCard, lowFromAbstractSupers}).max().getAsLong();
    }

    /**
     * The number of instances that the upper bound of the abstract clafer
     * allows in all.
     */
    private Optional<Long> abstractCap(Map<String, Long> analysis, Cl c) {
        if (c.isAbstract && c.ub > 0) {
            String parent = parentMap.get(c.uid);
            if (parent == null) {
                return Optional.of(c.ub);
            }
            Long ps = analysis.get(parent);
            return ps == null ? Optional.empty() : Optional.of(c.ub * ps);
        }
        return Optional.empty();
    }

    private Optional<Long> abstractSupersBound(Map<String, Long> analysis, Cl c) {
        List<Cl> h = findHierarchy(c);
        Optional<Long> min = Optional.empty();
        for (Cl s : h.subList(1, h.size())) {
            Optional<Long> b = abstractCap(analysis, s);
            if (b.isPresent()) {
                min = Optional.of(min.isEmpty() ? b.get() : Math.min(min.get(), b.get()));
            }
        }
        return min;
    }

    private void analyzeComponent(Map<String, Long> analysis, List<String> component) {
        if (component.size() == 1) {
            analyze(analysis, findClafer(component.get(0)));
            return;
        }
        // assume that each of the scopes in the component is 1 while solving
        for (String u : component) {
            analysis.put(u, 1L);
        }
        // foldr: from the last clafer of the component
        for (int i = component.size() - 1; i >= 0; i--) {
            analyze(analysis, findClafer(component.get(i)));
        }
    }

    private Cl findClafer(String uid) {
        Cl c = findIClafer(uid);
        if (c == null) {
            throw new Failure("Maybe.fromJust: Nothing");
        }
        return c;
    }

    private void analyze(Map<String, Long> analysis, Cl clafer) {
        long scope;
        Optional<Long> bound;
        if (clafer.isAbstract) {
            scope = 0;
            for (String sub : subclaferMap.getOrDefault(clafer.uid, List.of())) {
                scope += findOrError(analysis, sub, " subclafer scope not found");
            }
            bound = abstractCap(analysis, clafer);
        } else {
            String parent = parentMap.get(clafer.uid);
            long parentScope = parent == null ? 1 : findOrError(analysis, parent, " parent scope not found");
            scope = parentScope * lowerOrUpperFixedCard(analysis, clafer);
            bound = abstractSupersBound(analysis, clafer);
        }
        analysis.merge(clafer.uid, scope, Math::max);
        if (bound.isPresent()) {
            analysis.computeIfPresent(clafer.uid, (k, v) -> Math.min(bound.get(), v));
        }
    }

    private static long findOrError(Map<String, Long> m, String key, String message) {
        Long v = m.get(key);
        if (v == null) {
            throw new Failure(key + message);
        }
        return v;
    }

    private void analyzeSupers(Map<String, Long> analysis, Cl clafer) {
        // a clafer without instances adds none to its super, and neither do its children
        if (clafer.ub == 0) {
            return;
        }
        long lowerOrFixedUpperBound = Math.max(1, Math.max(clafer.lb, clafer.ub));
        if (!clafer.hasReference) {
            directSuper(clafer).ifPresent(s -> analysis.merge(s.uid, lowerOrFixedUpperBound, Long::sum));
        }
        for (Cl child : clafer.children) {
            analyzeSupers(analysis, child);
        }
    }

    private void analyzeRefs(Map<String, Long> analysis, Cl clafer) {
        long lowerOrFixedUpperBound = Math.max(1, Math.max(clafer.lb, clafer.ub));
        if (clafer.hasReference) {
            directSuper(clafer).ifPresent(s -> analysis.merge(s.uid, lowerOrFixedUpperBound, Math::max));
        }
        for (Cl child : clafer.children) {
            analyzeRefs(analysis, child);
        }
    }

    private Map<String, Long> analyzeConstraints() {
        Map<String, Long> analysis = new HashMap<>();
        for (P c : constraints) {
            if (c.exp() instanceof Q q
                    && (q.quant() == Ir.Quantifier.ONE || q.quant() == Ir.Quantifier.SOME)) {
                analyzeConstraint(q, analysis);
            }
        }
        for (P c : constraints) {
            for (Map.Entry<List<String>, Long> count : countsOf(c)) {
                analyzeCount(count.getKey(), count.getValue(), analysis);
            }
        }
        return analysis;
    }

    /**
     * The constraints that count the instances of a path from below, e.g.,
     * {@code [ #Box.item = 10 ]}, also in a conjunction.
     */
    private List<Map.Entry<List<String>, Long>> countsOf(P p) {
        if (p.exp() instanceof Fun f && f.args().size() == 2) {
            P a = f.args().get(0);
            P b = f.args().get(1);
            String op = f.op();
            if (op.equals("&&")) {
                List<Map.Entry<List<String>, Long>> out = new ArrayList<>(countsOf(a));
                out.addAll(countsOf(b));
                return out;
            }
            if ((op.equals("=") || op.equals(">=")) && countOf(a) != null && intOf(b) != null) {
                return List.of(Map.entry(countOf(a), intOf(b)));
            }
            if (op.equals(">") && countOf(a) != null && intOf(b) != null) {
                return List.of(Map.entry(countOf(a), intOf(b) + 1));
            }
            if ((op.equals("=") || op.equals("<=")) && intOf(a) != null && countOf(b) != null) {
                return List.of(Map.entry(countOf(b), intOf(a)));
            }
            if (op.equals("<") && intOf(a) != null && countOf(b) != null) {
                return List.of(Map.entry(countOf(b), intOf(a) + 1));
            }
        }
        return List.of();
    }

    private List<String> countOf(P p) {
        if (p.exp() instanceof Fun f && f.op().equals("#") && f.args().size() == 1) {
            List<String> path = dropThisAndParent(unfoldJoins(f.args().get(0)));
            if (!path.isEmpty() && path.stream().allMatch(upperCardsMap::containsKey)) {
                return path;
            }
        }
        return null;
    }

    private static Long intOf(P p) {
        return p.exp() instanceof IntE i ? i.value() : null;
    }

    private void analyzeCount(List<String> path, long n, Map<String, Long> analysis) {
        if (n <= 0) {
            return;
        }
        insertAll(analysis, path, assign(path, n));
    }

    private void analyzeConstraint(Q q, Map<String, Long> analysis) {
        if (q.decls().isEmpty()) {
            for (String u : dropThisAndParent(unfoldJoins(q.body()))) {
                analysis.merge(u, 1L, Math::max);
            }
            return;
        }
        for (Resolver.D d : q.decls()) {
            List<String> path = dropThisAndParent(unfoldJoins(d.body()));
            long minScope = d.disj() ? d.locals().size() : 1;
            insertAll(analysis, path, assign(path, minScope));
        }
    }

    private static void insertAll(Map<String, Long> analysis, List<String> path, List<Long> scores) {
        for (int i = 0; i < path.size(); i++) {
            analysis.merge(path.get(i), scores.get(i), Math::max);
        }
    }

    /**
     * A greedy assignment of the score from the last clafer of the path up.
     */
    private List<Long> assign(List<String> path, long score) {
        long[] out = new long[path.size() + 1];
        out[path.size()] = 1;
        long product = 1;
        for (int i = path.size() - 1; i >= 0; i--) {
            if (product == 0) {
                throw new Failure("Ratio has zero denominator");
            }
            long desire = Math.floorDiv(score + product - 1, product);
            long max = upperCards(path.get(i));
            out[i] = max == -1 ? desire : Math.min(desire, max);
            product *= out[i];
        }
        List<Long> scores = new ArrayList<>();
        for (long x : out) {
            scores.add(x);
        }
        return scores;
    }

    private static List<String> dropThisAndParent(List<String> path) {
        int i = 0;
        while (i < path.size() && path.get(i).equals("this")) {
            i++;
        }
        while (i < path.size() && path.get(i).equals("parent")) {
            i++;
        }
        return path.subList(i, path.size());
    }

    /**
     * The names of a tree of only joins, or none.
     */
    private static List<String> unfoldJoins(P p) {
        List<String> out = new ArrayList<>();
        if (p.exp() instanceof Fun f && f.op().equals(".")) {
            for (P a : f.args()) {
                out.addAll(unfoldJoins(a));
            }
        } else if (p.exp() instanceof Id i) {
            out.add(i.id());
        }
        return out;
    }

    private void analyzeHierarchy() {
        subclaferMap = new HashMap<>();
        parentMap = new HashMap<>();
        for (Cl c : clafers) {
            directSuper(c).ifPresent(s -> subclaferMap.computeIfAbsent(s.uid, k -> new ArrayList<>()).add(0, c.uid));
            for (Cl child : c.children) {
                parentMap.put(child.uid, c.uid);
            }
        }
    }

    // ======================================================================
    // analyzeDependencies: Data.Graph.stronglyConnComp
    // ======================================================================
    private List<List<String>> analyzeDependencies() {
        // Map.fromListWith (++) [(a, [b])]: the dependencies of a key, the last first
        TreeMap<String, List<String>> graph = new TreeMap<>();
        for (Cl c : clafers) {
            for (String[] d : dependency(c)) {
                graph.computeIfAbsent(d[0], k -> new ArrayList<>()).add(0, d[1]);
            }
        }
        List<String> keys = new ArrayList<>(graph.keySet());
        List<List<String>> deps = new ArrayList<>(graph.values());
        List<List<Integer>> components = stronglyConnComp(keys, deps);
        List<List<String>> out = new ArrayList<>();
        for (List<Integer> component : components) {
            out.add(component.stream().map(keys::get).toList());
        }
        return out;
    }

    private List<String[]> dependency(Cl clafer) {
        List<String[]> out = new ArrayList<>();
        out.add(new String[]{clafer.uid, clafer.uid});
        if (clafer.superUid != null) {
            directSuper(clafer).ifPresent(s -> out.add(new String[]{s.uid, clafer.uid}));
        }
        for (Cl child : clafer.children) {
            out.add(new String[]{child.uid, clafer.uid});
        }
        if (clafer.ub == -1 && clafer.refIds.size() == 1) {
            Cl target = findIClafer(clafer.refIds.get(0));
            if (target != null) {
                out.add(new String[]{clafer.uid, target.uid});
            }
        }
        return out;
    }

    /**
     * The strongly connected components of the graph whose vertices are the
     * sorted keys, each with the edges to its dependencies that are keys, in
     * the order of {@code Data.Graph.stronglyConnComp}: the components in
     * reverse topological order, each component's vertices in the pre-order
     * of its tree.
     *
     * @param keys the sorted keys
     * @param deps the dependencies of each key
     * @return the vertices of each component
     */
    static List<List<Integer>> stronglyConnComp(List<String> keys, List<List<String>> deps) {
        int n = keys.size();
        Map<String, Integer> vertex = new HashMap<>();
        for (int i = 0; i < n; i++) {
            vertex.put(keys.get(i), i);
        }
        List<List<Integer>> g = new ArrayList<>();
        for (List<String> ds : deps) {
            List<Integer> adj = new ArrayList<>();
            for (String d : ds) {
                Integer w = vertex.get(d);
                if (w != null) {
                    adj.add(w);
                }
            }
            g.add(adj);
        }
        // transposeG: buildG with the reversed edges, accumArray (flip (:))
        List<List<Integer>> t = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            t.add(new ArrayList<>());
        }
        for (int v = 0; v < n; v++) {
            for (int w : g.get(v)) {
                t.get(w).add(0, v);
            }
        }
        List<Integer> all = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            all.add(i);
        }
        List<Integer> post = new ArrayList<>();
        for (Tree tree : dfs(t, all)) {
            postorder(tree, post);
        }
        List<Integer> order = new ArrayList<>(post);
        java.util.Collections.reverse(order);
        List<List<Integer>> out = new ArrayList<>();
        for (Tree tree : dfs(g, order)) {
            List<Integer> vs = new ArrayList<>();
            preorder(tree, vs);
            out.add(vs);
        }
        return out;
    }

    private record Tree(int v, List<Tree> children) {
    }

    /**
     * The depth-first forest from the vertices, in order, each vertex once.
     */
    private static List<Tree> dfs(List<List<Integer>> g, List<Integer> vs) {
        boolean[] seen = new boolean[g.size()];
        List<Tree> forest = new ArrayList<>();
        for (int v : vs) {
            if (seen[v]) {
                continue;
            }
            seen[v] = true;
            Tree root = new Tree(v, new ArrayList<>());
            forest.add(root);
            // the trees and the next edge to follow of each
            Deque<Object[]> stack = new ArrayDeque<>();
            stack.push(new Object[]{root, 0});
            while (!stack.isEmpty()) {
                Object[] top = stack.peek();
                Tree tree = (Tree) top[0];
                int i = (Integer) top[1];
                List<Integer> adj = g.get(tree.v());
                if (i == adj.size()) {
                    stack.pop();
                    continue;
                }
                top[1] = i + 1;
                int w = adj.get(i);
                if (!seen[w]) {
                    seen[w] = true;
                    Tree child = new Tree(w, new ArrayList<>());
                    tree.children().add(child);
                    stack.push(new Object[]{child, 0});
                }
            }
        }
        return forest;
    }

    private static void postorder(Tree t, List<Integer> out) {
        Deque<Object[]> stack = new ArrayDeque<>();
        stack.push(new Object[]{t, 0});
        while (!stack.isEmpty()) {
            Object[] top = stack.peek();
            Tree tree = (Tree) top[0];
            int i = (Integer) top[1];
            if (i == tree.children().size()) {
                out.add(tree.v());
                stack.pop();
            } else {
                top[1] = i + 1;
                stack.push(new Object[]{tree.children().get(i), 0});
            }
        }
    }

    private static void preorder(Tree t, List<Integer> out) {
        Deque<Tree> stack = new ArrayDeque<>();
        stack.push(t);
        while (!stack.isEmpty()) {
            Tree tree = stack.pop();
            out.add(tree.v());
            for (int i = tree.children().size() - 1; i >= 0; i--) {
                stack.push(tree.children().get(i));
            }
        }
    }
}
