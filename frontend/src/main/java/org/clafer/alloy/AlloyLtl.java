package org.clafer.alloy;

import static org.clafer.alloy.Concat.cat;
import static org.clafer.alloy.Concat.cconcat;
import static org.clafer.alloy.Concat.cintercalate;
import static org.clafer.alloy.Concat.cs;
import static org.clafer.alloy.Concat.cunlines;
import static org.clafer.alloy.Concat.filterNull;
import static org.clafer.alloy.Concat.flatten;
import static org.clafer.alloy.Concat.plus;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.clafer.alloy.Concat.ExactCard;
import org.clafer.alloy.Concat.IrPExp;
import org.clafer.alloy.Concat.IrTrace;
import org.clafer.alloy.Concat.LowerCard;
import org.clafer.alloy.Concat.NoTrace;
import org.clafer.alloy.Concat.UpperCard;
import org.clafer.alloy.Mirror.IClafer;
import org.clafer.alloy.Mirror.IClaferId;
import org.clafer.alloy.Mirror.IConstraint;
import org.clafer.alloy.Mirror.IDecl;
import org.clafer.alloy.Mirror.IDeclPExp;
import org.clafer.alloy.Mirror.IElement;
import org.clafer.alloy.Mirror.IExp;
import org.clafer.alloy.Mirror.IFunExp;
import org.clafer.alloy.Mirror.IInt;
import org.clafer.alloy.Mirror.IType;
import org.clafer.alloy.Mirror.Interval;
import org.clafer.alloy.Mirror.PExp;
import org.clafer.alloy.Mirror.TClafer;
import org.clafer.alloy.Mirror.TMap;
import org.clafer.alloy.Mirror.TPrim;
import org.clafer.frontend.Ir;

/**
 * The Alloy 6 code of a temporal model, with Alloy's own temporal logic: a
 * port of the compiler's {@code Generator/AlloyLtl.hs}, function by function,
 * with the compiler's names, so that a difference from the compiler is a
 * difference between two functions of the same name. The semantics, and why
 * each part of the encoding is as it is, are documented there.
 * <p>
 * It reads the {@link Mirror} of the core that {@link AlloyInput} builds: the
 * clafers and typed expressions in the compiler's shapes.
 */
final class AlloyLtl {

    static final String rootIdent = "root";
    static final String thisIdent = "this";
    static final String parentIdent = "parent";

    /**
     * The generated code, and the map from its positions to the parts of the
     * model.
     */
    record Result(String code, List<Map.Entry<Span, IrTrace>> mapping) {
    }

    /**
     * A region of the Alloy code, 1-based lines and columns.
     */
    record Span(long startLine, long startColumn, long endLine, long endColumn) {
    }

    // ----------------------------------------------------------------------
    // GenEnv
    // ----------------------------------------------------------------------
    private final boolean validate;
    private final Map<String, IClafer> uidIClaferMap;
    private final List<IClafer> allClafers;
    private final String forScopes;
    private final Set<String> statics;
    private final Set<String> staticRefs;
    private final Set<String> enumLiterals;

    private AlloyLtl(boolean validate, Map<String, IClafer> uidIClaferMap, List<IClafer> allClafers,
            String forScopes, Set<String> statics, Set<String> staticRefs, Set<String> enumLiterals) {
        this.validate = validate;
        this.uidIClaferMap = uidIClaferMap;
        this.allClafers = allClafers;
        this.forScopes = forScopes;
        this.statics = statics;
        this.staticRefs = staticRefs;
        this.enumLiterals = enumLiterals;
    }

    /**
     * {@code genAlloyLtlModule}.
     *
     * @param statics the clafers that are final with all their ancestors in
     * the rich IR
     * @param staticRefs those of them whose own references are final
     * @param literals the literals of the enumerations of the rich IR
     * @param validate {@code -v}: no {@code run} command
     * @param root the root clafer, with the top-level declarations
     * @param scopes the scopes, in order
     * @param tl the trace length
     */
    static Result genAlloyLtlModule(Set<String> statics, Set<String> staticRefs, Set<String> literals,
            boolean validate, IClafer root, List<Map.Entry<String, Long>> scopes, int tl) {
        List<IClafer> nested = nestedClafers(root);
        List<IClafer> clafers = nested.subList(1, nested.size());
        Map<String, IClafer> map = new HashMap<>();
        for (IClafer c : nested) {
            map.put(c.uid, c);
        }
        List<String> parts = new ArrayList<>();
        for (Map.Entry<String, Long> s : scopes) {
            parts.add(s.getValue() + " " + s.getKey());
        }
        parts.add(tl + " steps");
        String forScopes = "for 1 but " + String.join(", ", parts);
        AlloyLtl genEnv = new AlloyLtl(validate, map, clafers, forScopes, statics, staticRefs, literals);
        List<Concat> cs = new ArrayList<>();
        for (IClafer c : nested) {
            cs.add(genEnv.genClafer(c));
        }
        Concat output = plus(genEnv.header(), cconcat(cs));
        List<Map.Entry<Span, IrTrace>> mapping = mapLineCol(output).stream()
                .filter(e -> e.getValue() != NoTrace.NoTrace).toList();
        return new Result(flatten(output), mapping);
    }

    static List<IClafer> nestedClafers(IClafer c) {
        List<IClafer> out = new ArrayList<>();
        out.add(c);
        for (IClafer s : c.subclafers()) {
            out.addAll(nestedClafers(s));
        }
        return out;
    }

    private Concat header() {
        return cs("open util/integer\n" + "pred show {}\n" + (validate ? "" : "run show " + forScopes) + "\n" + "\n");
    }

    // ----------------------------------------------------------------------
    // the structure of the model
    // ----------------------------------------------------------------------
    private static boolean isRoot(IClafer c) {
        return c.uid.equals(rootIdent);
    }

    private IClafer findIClafer(String uid) {
        return uidIClaferMap.get(uid);
    }

    private Optional<IClafer> parentOf(IClafer c) {
        if (isRoot(c)) {
            return Optional.empty();
        }
        if (c.isTopLevel()) {
            return Optional.ofNullable(findIClafer(rootIdent));
        }
        return Optional.ofNullable(findIClafer(c.parentUid));
    }

    private static boolean hasContainment(IClafer c) {
        return !isRoot(c) && !(c.isAbstract && c.isTopLevel());
    }

    private static List<IClafer> containedClafers(IClafer c) {
        return c.subclafers().stream().filter(AlloyLtl::hasContainment).toList();
    }

    private boolean isStatic(IClafer c) {
        return isRoot(c) || statics.contains(c.uid);
    }

    private boolean isEnumLiteral(IClafer c) {
        return enumLiterals.contains(c.uid);
    }

    private static List<String> getSuper(IClafer c) {
        return c.superUid == null ? List.of() : List.of(c.superUid);
    }

    /**
     * {@code findHierarchy getSuper}.
     */
    private List<IClafer> findHierarchy(IClafer c) {
        return go(new ArrayList<>(), c);
    }

    private List<IClafer> go(List<String> path, IClafer c) {
        if (path.contains(c.uid)) {
            return List.of(c);
        }
        List<String> supers = getSuper(c);
        if (supers.isEmpty()) {
            return List.of(c);
        }
        List<IClafer> out = new ArrayList<>();
        out.add(c);
        List<String> path2 = new ArrayList<>();
        path2.add(c.uid);
        path2.addAll(path);
        for (String s : supers) {
            IClafer sc = findIClafer(s);
            if (sc != null) {
                out.addAll(go(path2, sc));
            }
        }
        return out;
    }

    /**
     * The nearest super clafer that declares a reference.
     */
    private Optional<IClafer> inheritedReference(IClafer c) {
        List<IClafer> h = findHierarchy(c);
        for (IClafer s : h.subList(1, h.size())) {
            if (s.reference != null) {
                return Optional.of(s);
            }
        }
        return Optional.empty();
    }

    private List<IClafer> subsOf(IClafer c) {
        List<IClafer> out = new ArrayList<>();
        for (IClafer x : allClafers) {
            if (getSuper(x).equals(List.of(c.uid))) {
                out.add(x);
            }
        }
        return out;
    }

    private List<IClafer> concreteSubs(IClafer c) {
        if (c.isAbstract) {
            List<IClafer> out = new ArrayList<>();
            for (IClafer s : subsOf(c)) {
                out.addAll(concreteSubs(s));
            }
            return out;
        }
        return List.of(c);
    }

    static String aliveName(String uid) {
        return uid.equals(rootIdent) ? rootIdent : "alive_" + uid;
    }

    static String genRelName(String name) {
        return "r_" + name;
    }

    static String genRefName(String name) {
        return name + "_ref";
    }

    static String genParentName(String name) {
        return "p_" + name;
    }

    private String parentRel(String uid) {
        IClafer c = findIClafer(uid);
        if (c == null) {
            throw new IllegalStateException("AlloyLtl.parentRel: clafer not found: " + uid);
        }
        List<String> rels = concreteSubs(c).stream().map(this::rel).toList();
        return switch (rels.size()) {
            case 0 -> "(none->none)";
            case 1 -> rels.get(0);
            default -> "(" + String.join(" + ", rels) + ")";
        };
    }

    private String rel(IClafer c) {
        if (isEnumLiteral(c)) {
            return "(" + c.uid + "->" + rootIdent + ")";
        }
        return "(@" + genParentName(c.uid) + " :> " + parentOf(c).map(p -> aliveName(p.uid)).orElse(rootIdent) + ")";
    }

    private static Concat mkFact(Concat x) {
        if (x instanceof Concat.CString s && s.text().isEmpty()) {
            return x;
        }
        return cconcat(cs("fact "), mkSet(x), cs("\n"));
    }

    private static Concat mkSet(Concat xs) {
        return cconcat(cs("{ "), xs, cs(" }"));
    }

    private static Concat showSet(Concat delim, List<Concat> xs) {
        List<Concat> xs2 = filterNull(xs);
        if (xs2.isEmpty()) {
            return cs("{}");
        }
        return mkSet(cintercalate(delim, xs2));
    }

    private static Concat optShowSet(List<Concat> xs) {
        if (xs.isEmpty()) {
            return cs("");
        }
        return plus(cs("\n"), showSet(cs("\n  "), xs));
    }

    private Concat genClafer(IClafer c) {
        List<Concat> parts = new ArrayList<>();
        parts.add(claferDecl(isEnumLiteral(c), c, plus(showSet(cs("\n, "), genRelations(c)),
                optShowSet(filterNull(genConstraints(c))))));
        parts.add(isRoot(c) ? cs("") : aliveFun(c));
        parts.add(mkFact(cintercalate(cs("\n  "), filterNull(genStructure(c)))));
        parts.add(cunlines(genFacts(c)));
        parts.add(cconcat(getAssertions(c).stream().map(p -> mkAssert(c, p)).toList()));
        return plus(cunlines(filterNull(parts)), cs("\n"));
    }

    private Concat aliveFun(IClafer c) {
        return cs("fun " + aliveName(c.uid) + " : set " + c.uid + " { " + aliveBody(c) + " }");
    }

    private String aliveBody(IClafer c) {
        if (isEnumLiteral(c)) {
            return c.uid;
        }
        if (hasContainment(c)) {
            return sigOf(parentOf(c).orElseThrow()) + ".@" + genRelName(c.uid);
        }
        List<String> alive = subsOf(c).stream().map(s -> aliveName(s.uid)).toList();
        return alive.isEmpty() ? "none" : String.join(" + ", alive);
    }

    private static String sigOf(IClafer p) {
        return isRoot(p) ? rootIdent : p.uid;
    }

    private static Concat claferDecl(boolean one, IClafer c, Concat rest) {
        return cconcat(
                cs(isRoot(c) || one ? "one " : ""),
                cs(c.isAbstract ? "abstract " : ""),
                cs("sig "),
                cat(NoTrace.NoTrace, List.of(cs(c.uid), genExtends(c), cs("\n"), rest)));
    }

    private static Concat genExtends(IClafer c) {
        if (c.superUid != null) {
            return plus(cs(" "), cat(NoTrace.NoTrace, List.of(cs("extends " + c.superUid))));
        }
        return cs("");
    }

    /**
     * The parent, reference and containment fields of the clafer.
     */
    private List<Concat> genRelations(IClafer c) {
        List<Concat> out = new ArrayList<>();
        if (hasContainment(c) && !c.isAbstract && !isEnumLiteral(c)) {
            out.add(cs(genParentName(c.uid) + " : lone " + parentOf(c).map(AlloyLtl::sigOf).orElse(rootIdent)));
        }
        if (c.reference != null) {
            out.add(cs(var(staticRefs.contains(c.uid)) + genRefName(c.uid) + " : set " + refType(c)));
        }
        for (IClafer c2 : containedClafers(c)) {
            out.add(cat(NoTrace.NoTrace, List.of(cs(var(isStatic(c2)) + genRelName(c2.uid) + " : set " + c2.uid))));
        }
        return out;
    }

    private static String var(boolean isStatic) {
        return isStatic ? "" : "var ";
    }

    static boolean isPrimitive(String name) {
        return switch (name) {
            case "string", "int", "integer", "decimal", "double", "real" -> true;
            default -> false;
        };
    }

    private String refType(IClafer c) {
        if (c.reference == null) {
            return "";
        }
        if (c.reference.ref() != null) {
            return targetType(c, c.reference.ref());
        }
        String sid = c.reference.target();
        return isPrimitive(sid) ? "Int" : sid;
    }

    /**
     * The type of a field for the target of a reference: a union of targets
     * is the union of their signatures, since a field declaration cannot name
     * another signature's field.
     */
    private String targetType(IClafer c, PExp pexp) {
        PExp target = getTarget(pexp);
        if (target.exp() instanceof IClaferId id) {
            return isPrimitive(id.sident()) ? "Int" : id.sident();
        }
        if (target.exp() instanceof IFunExp f && f.op().equals("++")) {
            return f.exps().stream().map(p -> targetType(c, p)).collect(Collectors.joining(" + "));
        }
        return flatten(genPExp(List.of(c.uid), target));
    }

    private static PExp getTarget(PExp x) {
        if (x.exp() instanceof IFunExp f && f.exps().size() >= 2 && f.op().equals(iJoin)) {
            return f.exps().get(1);
        }
        return x;
    }

    /**
     * The facts about the instances of the clafer: a fixed parent, which
     * exists; for a nested abstract clafer, its instances are those of its
     * subclafers.
     */
    private List<Concat> genStructure(IClafer c) {
        if (!hasContainment(c) || isEnumLiteral(c)) {
            return List.of();
        }
        String rel = genRelName(c.uid);
        if (c.isAbstract) {
            List<String> rels = subsOf(c).stream().map(s -> "@" + genRelName(s.uid)).toList();
            return List.of(cs("always @" + rel + " = "
                    + (rels.isEmpty() ? "none->none" : "(" + String.join(" + ", rels) + ")")));
        }
        IClafer parent = parentOf(c).orElseThrow();
        return List.of(
                cs("always ~@" + rel + " in @" + genParentName(c.uid)),
                isRoot(parent) ? cs("") : cs("always @" + rel + ".univ in " + aliveName(parent.uid)));
    }

    /**
     * The constraints in the signature fact of the clafer, which hold for
     * every existing instance.
     */
    private List<Concat> genConstraints(IClafer c) {
        String uid = c.uid;
        String alive = aliveName(uid);
        Function<Concat, Concat> always = x -> isRoot(c)
                ? cconcat(cs("always ("), x, cs(")"))
                : cconcat(cs("always (this in "), cs(alive), cs(" implies ("), x, cs("))"));
        List<Concat> refConsts = new ArrayList<>();
        if (c.reference != null) {
            refConsts.add(cs("one this.@" + genRefName(uid)));
            String target = refType(c);
            if (!target.equals("Int") && findIClafer(target) != null) {
                refConsts.add(cs("this.@" + genRefName(uid) + " in " + aliveName(target)));
            }
            inheritedReference(c).ifPresent(s -> refConsts.add(
                    cs("this.@" + genRefName(uid) + " in this.@" + genRefName(s.uid))));
        }
        List<Concat> absentConsts = new ArrayList<>();
        if (!isRoot(c) && c.reference != null) {
            absentConsts.add(cs("always (this not in " + alive + " implies no this.@" + genRefName(uid) + ")"));
        }
        List<Concat> all = new ArrayList<>(refConsts);
        all.add(genGroupConst(c));
        for (IClafer c2 : containedClafers(c)) {
            all.add(genParentSubrelationConstraint(c2));
            all.addAll(genSetUniquenessConstraint(c2));
        }
        List<Concat> out = new ArrayList<>(filterNull(all).stream().map(always).toList());
        out.addAll(absentConsts);
        return out;
    }

    private record Fact(IrTrace trace, Concat x) {
    }

    /**
     * The constraints of the clafer and the cardinalities of its children,
     * each a fact of its own.
     */
    private List<Concat> genFacts(IClafer c) {
        List<Fact> facts = new ArrayList<>();
        for (IClafer c2 : containedClafers(c)) {
            facts.addAll(childCards(c2));
        }
        for (IElement e : c.elements) {
            if (e instanceof IConstraint k && k.isHard() && !finalityOfLiteral(c, k.origin())) {
                facts.add(new Fact(new IrPExp(k.pexp().pid()), genPExp(List.of(c.uid), k.pexp())));
            }
        }
        return facts.stream().map(f -> cconcat(cs("fact { "), cat(f.trace(), List.of(forEveryInstance(c, f.x()))),
                cs(" }"))).toList();
    }

    private boolean finalityOfLiteral(IClafer c, Optional<Ir.Origin> o) {
        return isEnumLiteral(c) && o.map(x -> List.of("final", "finalref", "finaltarget").contains(x.construct()))
                .orElse(false);
    }

    private static List<Fact> childCards(IClafer c) {
        String child = c.uid;
        String rel = "this.@" + genRelName(child);
        long n = c.card.low();
        long m = c.card.high();
        if (n == 1 && m == 1) {
            return List.of(new Fact(new ExactCard(child, false), cs("one " + rel)));
        }
        if (n == 0 && m == 1) {
            return List.of(new Fact(new ExactCard(child, false), cs("lone " + rel)));
        }
        if (n == 1 && m == -1) {
            return List.of(new Fact(new ExactCard(child, false), cs("some " + rel)));
        }
        List<Fact> out = new ArrayList<>();
        if (n > 0) {
            out.add(new Fact(new LowerCard(child, false), cs(n + " <= #(" + rel + ")")));
        }
        if (m != -1) {
            out.add(new Fact(new UpperCard(child, false), cs("#(" + rel + ") <= " + m)));
        }
        return out;
    }

    /**
     * The constraint for every instance of the clafer, in every snapshot in
     * which it exists.
     */
    private static Concat forEveryInstance(IClafer c, Concat x) {
        if (isRoot(c)) {
            return bindTo(rootIdent, cconcat(cs("always ("), x, cs(")")));
        }
        Concat always = cconcat(cs("always (" + selfIdent + " in " + aliveName(c.uid) + " implies ("), x, cs("))"));
        return cconcat(cs("all " + selfIdent + " : " + c.uid + " | "), bindSelf(always));
    }

    static final String selfIdent = "self_";

    static Concat bindSelf(Concat c) {
        return bindTo(selfIdent, c);
    }

    /**
     * The constraint with {@code this} replaced by the given name, chunk by
     * chunk, as the compiler replaces it.
     */
    static Concat bindTo(String name, Concat c) {
        return switch (c) {
            case Concat.CString s -> cs(bind(name, s.text()));
            case Concat.Cat k -> cat(k.srcPos(), k.nodes().stream().map(n -> bindTo(name, n)).toList());
        };
    }

    private static String bind(String name, String str) {
        StringBuilder out = new StringBuilder();
        Character prev = null;
        int i = 0;
        while (i < str.length()) {
            if (str.startsWith("this", i) && (prev == null || !identChar(prev))
                    && !(i + 4 < str.length() && identChar(str.charAt(i + 4)))) {
                out.append(name);
                prev = '_';
                i += 4;
            } else {
                char x = str.charAt(i);
                out.append(x);
                prev = x;
                i++;
            }
        }
        return out.toString();
    }

    private static boolean identChar(char x) {
        return Character.isLetterOrDigit(x) || x == '_' || x == '\'';
    }

    private static List<PExp> getAssertions(IClafer c) {
        List<PExp> out = new ArrayList<>();
        for (IElement e : c.elements) {
            if (e instanceof IConstraint k && !k.isHard()) {
                out.add(k.pexp());
            }
        }
        return out;
    }

    private Concat mkAssert(IClafer c, PExp pexp) {
        Concat name = cs(assertionName(pexp));
        return cconcat(cs("assert "), name, cs(" { "), forEveryInstance(c, genPExp(List.of(c.uid), pexp)),
                cs(" }\n"), cs("check "), name, cs(" "), cs(forScopes), cs("\n\n"));
    }

    static String assertionName(PExp pexp) {
        return "assertAt_" + pexp.inPos().line() + "_" + pexp.inPos().column();
    }

    /**
     * {@code setReferenceKey}: the clafer that declares the set reference
     * whose targets are distinct under one parent.
     */
    private Optional<IClafer> setReferenceKey(IClafer c) {
        IClafer declared;
        if (c.reference != null) {
            declared = c;
        } else if (c.isAbstract) {
            declared = null;
        } else {
            List<IClafer> h = findHierarchy(c);
            declared = h.subList(1, h.size()).stream().filter(s -> s.reference != null).findFirst().orElse(null);
        }
        if (declared == null || c.card == null) {
            return Optional.empty();
        }
        long lb = c.card.low();
        long ub = c.card.high();
        if ((lb > 1 || ub > 1 || ub == -1) && declared.reference.isSet() && !(c.isAbstract && c.isTopLevel())) {
            return Optional.of(declared);
        }
        return Optional.empty();
    }

    private List<Concat> genSetUniquenessConstraint(IClafer c) {
        return setReferenceKey(c).map(s -> List.of(cs("all disj x, y : this.@" + genRelName(c.uid) + " | (x.@"
                + genRefName(s.uid) + ") != (y.@" + genRefName(s.uid) + ")"))).orElse(List.of());
    }

    private Concat genParentSubrelationConstraint(IClafer headClafer) {
        if (headClafer.superUid == null) {
            return cs("");
        }
        IClafer parentClafer = findIClafer(headClafer.parentUid);
        IClafer superClafer = findIClafer(headClafer.superUid);
        if (parentClafer == null || superClafer == null) {
            return cs("");
        }
        IClafer superClafersParent = findIClafer(superClafer.parentUid);
        if (superClafersParent == null) {
            return cs("");
        }
        IClafer parentsSuperClafer = parentClafer.superUid == null ? null : findIClafer(parentClafer.superUid);
        boolean proper = (superClafer.isTopLevel() && superClafer.isAbstract)
                || headClafer.parentUid.equals(superClafer.parentUid)
                || (parentsSuperClafer == null
                        ? parentClafer.uid.equals(superClafersParent.uid)
                        : findUIDinSupers(superClafer.parentUid, parentsSuperClafer) != null);
        if (proper && !superClafer.isTopLevel()) {
            return cs("this.@" + genRelName(headClafer.uid) + " in this.@" + genRelName(superClafer.uid));
        }
        return cs("");
    }

    private IClafer findUIDinSupers(String uidToFind, IClafer current) {
        IClafer c = current;
        for (int guard = 0; c != null && guard < 10000; guard++) {
            if (uidToFind.equals(c.uid)) {
                return c;
            }
            if (c.superUid == null) {
                return null;
            }
            c = findIClafer(c.superUid);
        }
        return null;
    }

    private Concat genGroupConst(IClafer clafer) {
        if (clafer.isAbstract) {
            return cs("");
        }
        List<String> rels = new ArrayList<>();
        for (IClafer s : findHierarchy(clafer)) {
            for (IClafer sub : s.subclafers()) {
                rels.add("this.@" + genRelName(sub.uid));
            }
        }
        String children = String.join(" + ", rels);
        if (children.isEmpty()) {
            return cs("");
        }
        Concat card = mkCard(clafer.uid, true, "children", clafer.gcard);
        if (flatten(card).isEmpty()) {
            return cs("");
        }
        return cconcat(cs("let children = "), brArg(cs(children)), cs(" | "), card);
    }

    private static Concat mkCard(String constraintName, boolean group, String element, Interval crd) {
        Concat interval = genInterval(constraintName, group, element, crd);
        String crd2 = flatten(interval);
        if (crd2.equals("set") || crd2.isEmpty()) {
            return cs("");
        }
        if (crd2.equals("one") || crd2.equals("lone") || crd2.equals("some")) {
            return cs(crd2 + " " + element);
        }
        return interval;
    }

    private static Concat genInterval(String constraintName, boolean group, String element, Interval x) {
        long n = x.low();
        long exinteger = x.high();
        if (n == 1 && exinteger == 1) {
            return cat(new ExactCard(constraintName, group), List.of(cs("one")));
        }
        if (n == 0 && exinteger == 1) {
            return cat(new ExactCard(constraintName, group), List.of(cs("lone")));
        }
        if (n == 1 && exinteger == -1) {
            return cat(new ExactCard(constraintName, group), List.of(cs("some")));
        }
        if (n == 0 && exinteger == -1) {
            return cs("set");
        }
        String element2 = "(" + element + ")";
        Concat s1 = n == 0 ? null
                : cat(new LowerCard(constraintName, group), List.of(cs(n + " <= #" + element2)));
        Concat s2 = genExInteger(element2, x, exinteger)
                .map(s -> (Concat) cat(new UpperCard(constraintName, group), List.of(cs(s)))).orElse(null);
        if (s1 != null && s2 != null) {
            return cconcat(s1, cs(" and "), s2);
        }
        if (s1 != null) {
            return s1;
        }
        if (s2 != null) {
            return s2;
        }
        throw new IllegalStateException("AlloyLtl.genInterval: undefined");
    }

    private static Optional<String> genExInteger(String element, Interval yz, long x) {
        if (yz.low() == 0 && yz.high() == 0) {
            return Optional.of("#" + element + " = 0");
        }
        if (x == -1) {
            return Optional.empty();
        }
        return Optional.of("#" + element + " <= " + x);
    }

    // ----------------------------------------------------------------------
    // expressions; the head of the path is the UID of the context clafer
    // ----------------------------------------------------------------------
    private Concat genPExp(List<String> resPath, PExp x) {
        return genPExp2(resPath, adjustPExp(resPath, pushNegations(true, x)));
    }

    private static final String iNot = "!";
    private static final String iAnd = "&&";
    private static final String iOr = "||";
    private static final String iImpl = "=>";
    private static final String iIff = "<=>";
    private static final String iXor = "xor";
    private static final String iX = "X";
    private static final String iF = "F";
    private static final String iG = "G";
    private static final String iU = "U";
    private static final String iW = "W";
    private static final String iPrevious = "previous";
    private static final String iWeakPrevious = "weakprevious";
    private static final String iOnce = "once";
    private static final String iHistorically = "historically";
    private static final String iSince = "since";
    private static final String iTriggered = "triggered";
    private static final String iInitially = "initially";
    private static final String iFinally = "finally";
    private static final String iIfThenElse = "ifthenelse";
    private static final String iJoin = ".";
    private static final String iMin = "-";
    private static final String iMul = "*";
    private static final String iSumSet = "sum";
    private static final String iProdSet = "product";

    private static final List<String> ltlOps = List.of(iW, iU, iX, iG, iF, iInitially, iFinally, iPrevious, iOnce,
            iHistorically, iSince, iTriggered);
    private static final List<String> unOps = List.of(iNot, "#", iMin, "max", "min", "maximize", "minimize",
            iSumSet, iProdSet, iX, iF, iG, iInitially, iFinally, iPrevious, iOnce, iHistorically, iWeakPrevious);
    private static final List<String> logBinOps = List.of(iIff, iImpl, iOr, iXor, iAnd, iU, iW, iSince, iTriggered);
    private static final List<String> relBinOps = List.of("<", ">", "=", "<=", ">=", "!=", "in", "not in");
    private static final List<String> arithBinOps = List.of("+", "-", "*", "/", "%");

    /**
     * Pushes the negations of the formulas with temporal operators to the
     * formulas without them, with the dualities of LTL.
     */
    static PExp pushNegations(boolean pos, PExp x) {
        if (!hasTemporal(x)) {
            return pos ? x : neg(x);
        }
        IType t = x.type();
        Function<PExp, PExp> push = a -> pushNegations(pos, a);
        if (x.exp() instanceof IFunExp f) {
            String op = f.op();
            List<PExp> es = f.exps();
            if (es.size() == 1) {
                PExp a = es.get(0);
                if (op.equals(iNot)) {
                    return pushNegations(!pos, a);
                }
                if (op.equals(iF)) {
                    return mk(x, pos ? iF : iG, push.apply(a));
                }
                if (op.equals(iG)) {
                    return mk(x, pos ? iG : iF, push.apply(a));
                }
                if (List.of(iX, iInitially, iFinally).contains(op)) {
                    return mk(x, op, push.apply(a));
                }
                if (op.equals(iPrevious)) {
                    return mk(x, pos ? iPrevious : iWeakPrevious, push.apply(a));
                }
                if (op.equals(iWeakPrevious)) {
                    return mk(x, pos ? iWeakPrevious : iPrevious, push.apply(a));
                }
                if (op.equals(iOnce)) {
                    return mk(x, pos ? iOnce : iHistorically, push.apply(a));
                }
                if (op.equals(iHistorically)) {
                    return mk(x, pos ? iHistorically : iOnce, push.apply(a));
                }
            } else if (es.size() == 2) {
                PExp a = es.get(0);
                PExp b = es.get(1);
                if (op.equals(iAnd)) {
                    return mk(x, pos ? iAnd : iOr, push.apply(a), push.apply(b));
                }
                if (op.equals(iOr)) {
                    return mk(x, pos ? iOr : iAnd, push.apply(a), push.apply(b));
                }
                if (op.equals(iImpl) && pos && !hasTemporal(a)) {
                    return mk(x, iImpl, a, push.apply(b));
                }
                if (op.equals(iImpl)) {
                    return mk(x, pos ? iOr : iAnd, pushNegations(!pos, a), push.apply(b));
                }
                if (op.equals(iIff)) {
                    return pushNegations(pos, bool(x, iAnd, bool(x, iImpl, a, b), bool(x, iImpl, b, a)));
                }
                if (op.equals(iXor)) {
                    return pushNegations(pos, bool(x, iOr, bool(x, iAnd, a, bool(x, iNot, b)),
                            bool(x, iAnd, bool(x, iNot, a), b)));
                }
                if ((op.equals(iU) || op.equals(iW)) && pos) {
                    return mk(x, op, push.apply(a), push.apply(b));
                }
                if (op.equals(iU) || op.equals(iW)) {
                    return mk(x, op.equals(iU) ? iW : iU, push.apply(b), mk(x, iAnd, push.apply(a), push.apply(b)));
                }
                if (op.equals(iSince)) {
                    return mk(x, pos ? iSince : iTriggered, push.apply(a), push.apply(b));
                }
                if (op.equals(iTriggered)) {
                    return mk(x, pos ? iTriggered : iSince, push.apply(a), push.apply(b));
                }
            } else if (es.size() == 3) {
                PExp c = es.get(0);
                PExp a = es.get(1);
                PExp b = es.get(2);
                if (op.equals(iIfThenElse) && t == TPrim.TBoolean && !hasTemporal(c)) {
                    return mk(x, op, c, push.apply(a), push.apply(b));
                }
                if (op.equals(iIfThenElse) && t == TPrim.TBoolean) {
                    return pushNegations(pos, bool(x, iAnd, bool(x, iImpl, c, a), bool(x, iImpl, bool(x, iNot, c), b)));
                }
            }
        } else if (x.exp() instanceof IDeclPExp d && !d.oDecls().isEmpty()) {
            switch (d.quant()) {
                case ALL:
                    return x.withExp(new IDeclPExp(pos ? Ir.Quantifier.ALL : Ir.Quantifier.SOME, d.oDecls(),
                            push.apply(d.bpexp())));
                case SOME:
                    return x.withExp(new IDeclPExp(pos ? Ir.Quantifier.SOME : Ir.Quantifier.ALL, d.oDecls(),
                            push.apply(d.bpexp())));
                case NO:
                    return x.withExp(new IDeclPExp(pos ? Ir.Quantifier.ALL : Ir.Quantifier.SOME, d.oDecls(),
                            pushNegations(!pos, d.bpexp())));
                default:
                    break;
            }
        }
        return pos ? x : neg(x);
    }

    private static PExp mk(PExp x, String op, PExp... args) {
        return x.withExp(new IFunExp(op, List.of(args)));
    }

    private static PExp bool(PExp x, String op, PExp... args) {
        return new PExp(TPrim.TBoolean, x.pid(), x.inPos(), new IFunExp(op, List.of(args)));
    }

    private static PExp neg(PExp a) {
        return bool(a, iNot, a);
    }

    static boolean hasTemporal(PExp p) {
        return switch (p.exp()) {
            case IFunExp f -> f.op().equals(iFinally) || f.op().equals(iWeakPrevious) || ltlOps.contains(f.op())
                    || f.exps().stream().anyMatch(AlloyLtl::hasTemporal);
            case IDeclPExp d -> hasTemporal(d.bpexp());
            default -> false;
        };
    }

    private Concat genPExp2(List<String> resPath, PExp p) {
        IType iType = p.type();
        IExp exp = p.exp();
        switch (exp) {
            case IDeclPExp d -> {
                List<Concat> decls = d.oDecls().stream().map(x -> genDecl(resPath, x)).toList();
                return cat(new IrPExp(p.pid()), List.of(cs(genQuant(d.quant())), cs(" "),
                        cintercalate(cs(", "), decls), cs(d.oDecls().isEmpty() ? "" : " | "),
                        genPExp2(resPath, d.bpexp())));
            }
            case IClaferId id -> {
                String sid = id.sident();
                switch (sid) {
                    case "integer", "int", "string" -> {
                        return cs("Int");
                    }
                    case "noclafer" -> {
                        return cs("none");
                    }
                    case "dref" -> {
                        if (iType instanceof TMap m && m.so() instanceof TClafer c && !c.hi().isEmpty()) {
                            return cs("@" + genRefName(c.hi().get(0)));
                        }
                        throw new IllegalStateException("[bug] AlloyLtl.genPExp'.getTClaferUID: unknown type: " + iType);
                    }
                    default -> {
                    }
                }
                if (id.local()) {
                    return cs(sid);
                }
                if (!sid.isEmpty() && "~@(".indexOf(sid.charAt(0)) >= 0) {
                    return cs(sid);
                }
                String sid2;
                if (sid.equals(thisIdent)) {
                    sid2 = thisAlive(resPath);
                } else if (sid.equals(rootIdent)) {
                    sid2 = rootIdent;
                } else if (id.isTop()) {
                    sid2 = aliveName(sid);
                } else {
                    sid2 = "@" + genRelName(sid);
                }
                if (iType instanceof TPrim t && t != TPrim.TBoolean) {
                    return cs(sid2 + ".@" + genRefName(sid.equals(thisIdent) ? resPath.get(0) : sid));
                }
                return cs(sid2);
            }
            case IFunExp f -> {
                return genIFunExp(p.pid(), resPath, transformExp(f));
            }
            case IInt i -> {
                return cs(Long.toString(i.value()));
            }
        }
    }

    /**
     * The context instance, as long as it exists.
     */
    private static String thisAlive(List<String> resPath) {
        if (!resPath.isEmpty() && !resPath.get(0).equals(rootIdent)) {
            return "(this & " + aliveName(resPath.get(0)) + ")";
        }
        return rootIdent;
    }

    private Optional<String> parentExists(List<String> resPath) {
        if (!resPath.isEmpty()) {
            String ctx = resPath.get(0);
            IClafer c = findIClafer(ctx);
            if (c != null && concreteSubs(c).stream().anyMatch(s -> parentOf(s).map(x -> !isRoot(x)).orElse(false))) {
                return Optional.of("some this." + parentRel(ctx));
            }
        }
        return Optional.empty();
    }

    private static String appearsNow(List<String> resPath) {
        if (!resPath.isEmpty() && !resPath.get(0).equals(rootIdent)) {
            return "not before this in " + aliveName(resPath.get(0));
        }
        return "not before some " + rootIdent;
    }

    private static IFunExp transformExp(IFunExp x) {
        String op = x.op();
        List<PExp> exps = x.exps();
        if (exps.size() == 1 && op.equals(iMin)) {
            PExp e1 = exps.get(0);
            return new IFunExp(iMul, List.of(new PExp(e1.type(), "", null, new IInt(-1)), e1));
        }
        if (exps.size() >= 2) {
            PExp e1 = exps.get(0);
            PExp e2 = exps.get(1);
            if (op.equals(iXor)) {
                return new IFunExp(iNot, List.of(new PExp(TPrim.TBoolean, "", null, new IFunExp(iIff, exps))));
            }
            if (op.equals(iW)) {
                return new IFunExp(iOr, List.of(new PExp(TPrim.TBoolean, "", null, new IFunExp(iG, List.of(e1))),
                        new PExp(TPrim.TBoolean, "", null, new IFunExp(iU, exps))));
            }
            if (op.equals(iJoin) && e1.exp() instanceof IClaferId a && e2.exp() instanceof IClaferId b
                    && a.sident().equals(thisIdent) && b.sident().startsWith("~")) {
                return new IFunExp(op, List.of(e1.withType(new TClafer(List.of())), e2));
            }
        }
        return x;
    }

    private Concat genIFunExp(Object pid, List<String> resPath, IFunExp f) {
        String op = f.op();
        List<PExp> exps = f.exps();
        IrTrace trace = new IrPExp(pid);
        if (op.equals("min") && exps.size() == 1) {
            return cat(trace, List.of(cs("min["), genPExp2(resPath, exps.get(0)), cs("]")));
        }
        if (op.equals("max") && exps.size() == 1) {
            return cat(trace, List.of(cs("max["), genPExp2(resPath, exps.get(0)), cs("]")));
        }
        if (exps.size() == 2 && exps.get(0).exp() instanceof IClaferId id) {
            if (op.equals(iJoin) && id.sident().equals(thisIdent)) {
                return cat(trace, List.of(cs("this."), optBrArg(resPath, exps.get(1))));
            }
            if (op.equals(iJoin) && id.sident().equals(rootIdent)) {
                return genPExp2(resPath, exps.get(1));
            }
        }
        if (exps.size() == 1) {
            PExp e1 = exps.get(0);
            Optional<String> pe = parentExists(resPath);
            if (op.equals(iInitially)) {
                return cat(trace, List.of(cs("(" + appearsNow(resPath) + ") implies "), brArg(genPExp2(resPath, e1))));
            }
            if (op.equals(iFinally)) {
                return cat(trace, List.of(cs("(some " + thisAlive(resPath) + " and after no " + thisAlive(resPath)
                        + ") implies "), brArg(genPExp2(resPath, e1))));
            }
            if (pe.isPresent()) {
                String p = pe.get();
                Concat e = brArg(genPExp2(resPath, e1));
                switch (op) {
                    case iX:
                        return cat(trace, List.of(cs("after (not " + p + " or "), e, cs(")")));
                    case iF:
                        return cat(trace, List.of(cs("eventually (not " + p + " or "), e, cs(")")));
                    case iG:
                        return cat(trace, List.of(cs("(not " + p + ") releases (not " + p + " or "), e, cs(")")));
                    case iPrevious:
                        return cat(trace, List.of(cs("before (" + p + " and "), e, cs(")")));
                    case iWeakPrevious:
                        return cat(trace, List.of(cs("not before (" + p + " and not "), e, cs(")")));
                    case iOnce:
                        return cat(trace, List.of(cs(p + " since (" + p + " and "), e, cs(")")));
                    case iHistorically:
                        return cat(trace, List.of(cs("not (" + p + " since (" + p + " and not "), e, cs("))")));
                    default:
                        break;
                }
            } else if (op.equals(iWeakPrevious)) {
                return cat(trace, List.of(cs("not before not "), brArg(genPExp2(resPath, e1))));
            }
        }
        if (exps.size() == 2) {
            Optional<String> pe = parentExists(resPath);
            if (pe.isPresent()) {
                String p = pe.get();
                switch (op) {
                    case iU:
                        return cat(trace, List.of(cs("(" + p + " and "), brArg(genPExp2(resPath, exps.get(0))),
                                cs(") until (not " + p + " or "), brArg(genPExp2(resPath, exps.get(1))), cs(")")));
                    case iSince:
                        return cat(trace, List.of(cs("(" + p + " and "), brArg(genPExp2(resPath, exps.get(0))),
                                cs(") since (" + p + " and "), brArg(genPExp2(resPath, exps.get(1))), cs(")")));
                    case iTriggered:
                        return cat(trace, List.of(cs("not ((" + p + " and not "),
                                brArg(genPExp2(resPath, exps.get(0))), cs(") since (" + p + " and not "),
                                brArg(genPExp2(resPath, exps.get(1))), cs("))")));
                    default:
                        break;
                }
            }
        }
        if (op.equals(iSumSet)) {
            return genIFunExp(pid, resPath, new IFunExp("sum'", List.of(removeright(exps.get(0)), getRight(exps.get(0)))));
        }
        List<Concat> exps2 = exps.stream().map(e -> optBrArg(resPath, e)).toList();
        List<Concat> ops;
        List<Concat> parts;
        if (op.equals("sum'")) {
            ops = genOp(iSumSet).stream().map(Concat::cs).toList();
            parts = interleave(ops, exps2);
        } else {
            ops = genOp(op).stream().map(Concat::cs).toList();
            if (arithBinOps.contains(op) && exps.size() == 2) {
                parts = interleave(exps2, ops);
            } else {
                List<Concat> rx = new ArrayList<>(exps2);
                Collections.reverse(rx);
                List<Concat> ry = new ArrayList<>(ops);
                Collections.reverse(ry);
                parts = new ArrayList<>(interleave(rx, ry));
                Collections.reverse(parts);
            }
        }
        return cat(trace, parts);
    }

    private Concat optBrArg(List<String> resPath, PExp x) {
        if (x.exp() instanceof IClaferId || x.exp() instanceof IInt) {
            return genPExp2(resPath, x);
        }
        return brArg(genPExp2(resPath, x));
    }

    private static List<Concat> interleave(List<Concat> xs, List<Concat> ys) {
        List<Concat> out = new ArrayList<>();
        List<Concat> a = xs;
        List<Concat> b = ys;
        while (true) {
            if (a.isEmpty()) {
                out.addAll(b);
                return out;
            }
            if (b.isEmpty()) {
                out.addAll(a);
                return out;
            }
            out.add(a.get(0));
            List<Concat> rest = a.subList(1, a.size());
            a = b;
            b = rest;
        }
    }

    private static Concat brArg(Concat arg) {
        return cconcat(cs("("), arg, cs(")"));
    }

    private static List<String> genOp(String op) {
        switch (op) {
            case iX:
                return List.of("after ");
            case iF:
                return List.of("eventually ");
            case iG:
                return List.of("always ");
            case iU:
                return List.of(" until ");
            case iPrevious:
                return List.of("before ");
            case iOnce:
                return List.of("once ");
            case iHistorically:
                return List.of("historically ");
            case iSince:
                return List.of(" since ");
            case iTriggered:
                return List.of(" triggered ");
            case "+":
                return List.of(".plus[", "]");
            case "-":
                return List.of(".minus[", "]");
            case iSumSet:
                return List.of("sum temp : ", " | temp.");
            case iProdSet:
                return List.of("prod temp : ", " | temp.");
            default:
                break;
        }
        if (unOps.contains(op)) {
            return List.of(op);
        }
        switch (op) {
            case "*":
                return List.of(".mul[", "]");
            case "/":
                return List.of(".div[", "]");
            case "%":
                return List.of(".rem[", "]");
            default:
                break;
        }
        if (logBinOps.contains(op) || relBinOps.contains(op) || arithBinOps.contains(op)) {
            return List.of(" " + op + " ");
        }
        return switch (op) {
            case "++" -> List.of(" + ");
            case "--" -> List.of(" - ");
            case "**" -> List.of(" & ");
            case "<:" -> List.of(" <: ");
            case ":>" -> List.of(" :> ");
            case iJoin -> List.of(".");
            case iIfThenElse -> List.of(" => ", " else ");
            default -> throw new IllegalStateException("[bug] AlloyLtl.genOp: Unmatched operator: " + op);
        };
    }

    /**
     * Replaces the navigation to the parent with the relation to the parent.
     */
    private PExp adjustPExp(List<String> resPath, PExp p) {
        return p.withExp(adjustIExp(resPath, p.exp()));
    }

    private IExp adjustIExp(List<String> resPath, IExp x) {
        return switch (x) {
            case IDeclPExp d -> new IDeclPExp(d.quant(), d.oDecls(), adjustPExp(resPath, d.bpexp()));
            case IFunExp f -> f.op().equals(iJoin)
                    ? adjustNav(resPath, f).exp()
                    : new IFunExp(f.op(), f.exps().stream().map(e -> adjustPExp(resPath, e)).toList());
            case IClaferId id -> adjustNav(resPath, id).exp();
            default -> x;
        };
    }

    private record Nav(IExp exp, List<String> path) {
    }

    private Nav adjustNav(List<String> resPath, IExp x) {
        if (x instanceof IFunExp f && f.exps().size() >= 2) {
            if (!f.op().equals(iJoin)) {
                return new Nav(x, resPath);
            }
            PExp pexp0 = f.exps().get(0);
            PExp pexp = f.exps().get(1);
            Nav n0 = adjustNav(resPath, pexp0.exp());
            List<String> typePath = n0.path();
            if (pexp0.type() instanceof TClafer tc && !tc.hi().isEmpty() && findIClafer(tc.hi().get(0)) != null) {
                List<String> tp = new ArrayList<>();
                tp.add(tc.hi().get(0));
                tp.addAll(n0.path().subList(Math.min(1, n0.path().size()), n0.path().size()));
                typePath = tp;
            }
            Nav n = adjustNav(typePath, pexp.exp());
            return new Nav(new IFunExp(iJoin, List.of(pexp0.withExp(n0.exp()), pexp.withExp(n.exp()))), n.path());
        }
        if (x instanceof IFunExp) {
            return new Nav(x, resPath);
        }
        if (x instanceof IClaferId id) {
            if (id.sident().equals(parentIdent)) {
                List<String> parentPath;
                IClafer c = findIClafer(resPath.get(0));
                Optional<IClafer> p = c == null ? Optional.empty() : parentOf(c);
                if (p.isPresent()) {
                    parentPath = new ArrayList<>();
                    parentPath.add(p.get().uid);
                    parentPath.addAll(resPath.subList(1, resPath.size()));
                } else {
                    parentPath = resPath.subList(1, resPath.size());
                }
                return new Nav(id.withSident(parentRel(resPath.get(0))), parentPath);
            }
            return new Nav(x, resPath);
        }
        throw new IllegalStateException("AlloyLtl.adjustNav: expects a IFunExp or IClaferId");
    }

    private static String genQuant(Ir.Quantifier q) {
        return switch (q) {
            case NO -> "no";
            case LONE -> "lone";
            case ONE -> "one";
            case SOME -> "some";
            case ALL -> "all";
        };
    }

    private Concat genDecl(List<String> resPath, IDecl x) {
        return cconcat(cs(x.isDisj() ? "disj" : ""), cs(" "), cs(String.join(", ", x.decls())), cs(" : "),
                genPExp(resPath, x.body()));
    }

    private static PExp removeright(PExp p) {
        if (p.exp() instanceof IFunExp f && f.exps().size() >= 2) {
            PExp x = f.exps().get(0);
            PExp r = f.exps().get(1);
            if (r.exp() instanceof IClaferId || r.exp() instanceof IInt) {
                return x;
            }
            List<PExp> es = new ArrayList<>(f.exps());
            es.set(1, removeright(r));
            return p.withExp(new IFunExp(f.op(), es));
        }
        throw new IllegalStateException("[bug] AlloyLtl.removeright: expects a PExp with a IFunExp inside but was given: " + p);
    }

    private static PExp getRight(PExp p) {
        if (p.exp() instanceof IFunExp f && f.exps().size() >= 2) {
            return getRight(f.exps().get(1));
        }
        return p;
    }

    // ----------------------------------------------------------------------
    // mapping line/columns between Clafer and Alloy code
    // ----------------------------------------------------------------------
    private static List<Map.Entry<Span, IrTrace>> mapLineCol(Concat code) {
        long[] lineCol = {1, 1};
        List<Map.Entry<Span, IrTrace>> mapping = new ArrayList<>();
        mapLineCol2(code, lineCol, mapping);
        return mapping;
    }

    private static void mapLineCol2(Concat c, long[] lineCol, List<Map.Entry<Span, IrTrace>> mapping) {
        switch (c) {
            case Concat.CString s -> addCode(lineCol, s.text());
            case Concat.Cat k -> {
                long startLine = lineCol[0];
                long startCol = lineCol[1];
                for (Concat n : k.nodes()) {
                    mapLineCol2(n, lineCol, mapping);
                }
                String flat = flatten(c);
                long raiseStart = countLeading("([", flat);
                long deductEnd = -countLeading(")]", new StringBuilder(flat).reverse().toString());
                // the compiler conses each mapping onto the front
                mapping.add(0, Map.entry(new Span(startLine, startCol + raiseStart, lineCol[0], lineCol[1] + deductEnd),
                        k.srcPos()));
            }
        }
    }

    private static long countLeading(String chars, String xs) {
        int n = 0;
        while (n < xs.length() && chars.indexOf(xs.charAt(n)) >= 0) {
            n++;
        }
        return n;
    }

    private static void addCode(long[] lineCol, String str) {
        long newLines = str.chars().filter(ch -> ch == '\n').count();
        int last = str.lastIndexOf('\n');
        long newCol = last < 0 ? str.length() : str.length() - last - 1;
        lineCol[1] = (newLines > 0 ? 1 : lineCol[1]) + newCol;
        lineCol[0] += newLines;
    }
}
