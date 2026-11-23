package org.clafer.ast;

import java.math.BigDecimal;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.clafer.assertion.Assertion;
import org.clafer.frontend.Core;
import org.clafer.frontend.Diagnostic;
import org.clafer.frontend.ExplicitParser;
import org.clafer.frontend.ToCore;
import org.clafer.frontend.Ir;
import org.clafer.frontend.Ir.BinaryOp;
import org.clafer.frontend.Ir.ClaferDecl;
import org.clafer.frontend.Ir.Element;
import org.clafer.frontend.Ir.Exp;
import org.clafer.frontend.Ir.Node;
import org.clafer.frontend.SourceSpan;
import org.clafer.objective.Objective;

/**
 * Builds chocosolver's {@link AstModel} from a static explicit model of the
 * {@link Ir explicit IR}: the core explicit model that {@code clafer -m clafer
 * --core} prints, a static rich one, which it lowers to the core with
 * {@link ToCore}, or what
 * {@link AstPrinter} prints. It resolves the names: the clafers, which may be
 * used before they are declared, and the variables of the quantifiers.
 * <p>
 * The clafers are created in the order of the source, so that the instances
 * are numbered as the compiler numbers them, then given their super clafers,
 * and then their references and group cards, after their super clafers'.
 * What the printer writes because it applies, this sets only where it is not
 * inherited: a group card equal to the super clafer's is the super clafer's.
 * A written reference is the clafer's own.
 * <p>
 * A nested abstract clafer's multiplicity counts the instances of its
 * subclafers in each parent, so it becomes a constraint of the parent on
 * {@code #this.A}, as the compiler gives it to chocosolver. A temporal
 * model has a diagnostic on each line with a temporal construct, since
 * chocosolver solves only static models. {@code real} and {@code double} have
 * no solver.
 * <p>
 * Clafers, constraints, objectives and assertions get the spans of their
 * source, so chocosolver's errors name their positions. Errors do not stop
 * the build: it reports a diagnostic and goes on with the next declaration or
 * constraint.
 */
public final class ToAst {

    /**
     * A model built from an explicit model.
     *
     * @param model the model
     * @param objectives the objectives
     * @param assertions the assertions
     * @param diagnostics the errors, empty if the model is complete
     */
    public record Result(AstModel model, Objective[] objectives, Assertion[] assertions,
            List<Diagnostic> diagnostics) {
    }

    /**
     * An error at a node, unwinding to the constraint that contains it.
     */
    private static final class BuildError extends RuntimeException {

        private final Diagnostic diagnostic;

        BuildError(SourceSpan span, String message) {
            super(message, null, false, false);
            this.diagnostic = new Diagnostic(span, message);
        }
    }

    private static final Map<BinaryOp, AstArithm.Op> ArithmOps = Map.of(
            BinaryOp.ADD, AstArithm.Op.Add, BinaryOp.SUB, AstArithm.Op.Sub, BinaryOp.MUL, AstArithm.Op.Mul,
            BinaryOp.DIV, AstArithm.Op.Div, BinaryOp.DECIMAL_MUL, AstArithm.Op.DecimalMul,
            BinaryOp.DECIMAL_DIV, AstArithm.Op.DecimalDiv);
    private static final Map<BinaryOp, AstBoolArithm.Op> BoolOps = Map.of(
            BinaryOp.AND, AstBoolArithm.Op.And, BinaryOp.OR, AstBoolArithm.Op.Or,
            BinaryOp.IMPLIES, AstBoolArithm.Op.Implies, BinaryOp.IFF, AstBoolArithm.Op.IfOnlyIf,
            BinaryOp.XOR, AstBoolArithm.Op.Xor);
    private static final Map<BinaryOp, AstCompare.Op> CompareOps = Map.of(
            BinaryOp.LT, AstCompare.Op.LessThan, BinaryOp.LE, AstCompare.Op.LessThanEqual,
            BinaryOp.GT, AstCompare.Op.GreaterThan, BinaryOp.GE, AstCompare.Op.GreaterThanEqual);
    private static final Set<String> Unsolved = Set.of("real", "double");

    private final List<Diagnostic> diagnostics = new ArrayList<>();
    private final AstModel model = Asts.newModel();
    private final Map<String, AstClafer> clafers = new LinkedHashMap<>();
    // In the order of declaration, which is the order in which sub clafers
    // extend their super clafer, as in the compiler's output.
    private final Map<AstClafer, ClaferDecl> declOf = new LinkedHashMap<>();
    private final Deque<Map<String, AstLocal>> locals = new ArrayDeque<>();
    private final List<Objective> objectives = new ArrayList<>();
    private final List<Assertion> assertions = new ArrayList<>();

    private ToAst() {
        clafers.put("int", Asts.IntType);
        clafers.put("integer", Asts.IntType);
        clafers.put("string", Asts.StringType);
        clafers.put("decimal", Asts.DecimalType);
        clafers.put("bool", Asts.BoolType);
    }

    /**
     * Parses an explicit model and builds its {@link AstModel}.
     *
     * @param source the text
     * @return the model, its objectives and assertions, and the errors of the
     * parser and the build, by position
     */
    public static Result parse(String source) {
        ExplicitParser.Result parsed = ExplicitParser.parse(source);
        return toAst(parsed.model(), parsed.diagnostics());
    }

    /**
     * Builds the {@link AstModel} of an explicit model.
     *
     * @param tree the explicit model
     * @return the model, its objectives and assertions, and the errors
     */
    public static Result toAst(Ir.Model tree) {
        return toAst(tree, List.of());
    }

    private static Result toAst(Ir.Model tree, List<Diagnostic> parseErrors) {
        ToAst builder = new ToAst();
        List<Diagnostic> refused = temporal(tree);
        if (refused.isEmpty()) {
            // the rich constructs of a static model: its enumerations,
            // initializers and top-level abstract multiplicities
            tree = ToCore.toCore(tree);
            // := in a model with a trace length gives an initial value
            refused = temporal(tree);
        }
        if (!refused.isEmpty()) {
            // the other errors of a temporal model follow from its temporal constructs
            builder.diagnostics.addAll(refused);
        } else {
            // what could be read, so that its errors are reported too
            builder.diagnostics.addAll(parseErrors);
            builder.build(tree);
        }
        List<Diagnostic> diagnostics = new ArrayList<>(builder.diagnostics);
        diagnostics.sort((a, b) -> a.span().line() != b.span().line()
                ? Integer.compare(a.span().line(), b.span().line())
                : Integer.compare(a.span().column(), b.span().column()));
        return new Result(builder.model,
                builder.objectives.toArray(new Objective[0]),
                builder.assertions.toArray(new Assertion[0]),
                Collections.unmodifiableList(diagnostics));
    }

    /**
     * The temporal constructs of a model, the first on each line.
     */
    private static List<Diagnostic> temporal(Ir.Model tree) {
        List<Diagnostic> temporal = new ArrayList<>();
        collectTemporal(tree, temporal);
        temporal.sort((a, b) -> a.span().line() != b.span().line()
                ? Integer.compare(a.span().line(), b.span().line())
                : Integer.compare(a.span().column(), b.span().column()));
        List<Diagnostic> first = new ArrayList<>();
        int line = -1;
        for (Diagnostic d : temporal) {
            if (d.span().line() != line) {
                first.add(d);
                line = d.span().line();
            }
        }
        return first;
    }

    private static void collectTemporal(Node node, List<Diagnostic> temporal) {
        String what = switch (node) {
            case Ir.Modifier m -> m.kind().keyword() + " is a temporal modifier";
            case Ir.Temporal t -> t.op().keyword() + " is a temporal operator";
            case Ir.Anchor a -> (a.initially() ? "initially" : "finally") + " is a temporal operator";
            case Ir.Pattern p -> p.kind().keyword() + " is a behavioral pattern";
            case Ir.Transition t -> "a transition is temporal";
            default -> null;
        };
        if (what != null) {
            temporal.add(new Diagnostic(node.span(), "Chocosolver does not solve temporal models: " + what
                    + "; Alloy solves them (claferIG)"));
        }
        for (Node child : Core.children(node)) {
            collectTemporal(child, temporal);
        }
    }

    // ------------------------------------------------------------------
    // The clafers
    // ------------------------------------------------------------------
    private void build(Ir.Model tree) {
        // The clafers, so that every name can be found.
        for (Element element : tree.elements()) {
            if (element instanceof ClaferDecl decl) {
                declare(decl, null);
            }
        }
        // Supers, then references and group cards, which depend on the
        // super clafers' ones.
        for (Map.Entry<AstClafer, ClaferDecl> entry : declOf.entrySet()) {
            setSuper(entry.getKey(), entry.getValue());
        }
        Set<AstClafer> done = new HashSet<>();
        for (AstClafer clafer : new ArrayList<>(declOf.keySet())) {
            setInherited(clafer, done, new HashSet<>());
        }
        // The expressions.
        for (Element element : tree.elements()) {
            switch (element) {
                case Ir.Constraint c -> addConstraint(model.getAbstractRoot(), c);
                case Ir.RootBlock r -> r.constraints().forEach(c -> addConstraint(model.getRoot(), c));
                default -> {
                }
            }
        }
        for (Map.Entry<AstClafer, ClaferDecl> entry : declOf.entrySet()) {
            for (Element element : entry.getValue().elements()) {
                if (element instanceof Ir.Constraint c) {
                    addConstraint(entry.getKey(), c);
                }
            }
        }
        for (Ir.Objective objective : tree.objectives()) {
            AstExpr expr = convertTop(objective.exp());
            if (expr instanceof AstSetExpr set) {
                objectives.add(objective.maximize()
                        ? Objective.maximize(set, objective.span())
                        : Objective.minimize(set, objective.span()));
            } else if (expr != null) {
                diagnostics.add(new Diagnostic(objective.span(), "An objective must be a number, not a constraint"));
            }
        }
        for (Ir.Assertion assertion : tree.assertions()) {
            AstExpr expr = convertTop(assertion.exp());
            if (expr instanceof AstBoolExpr bool) {
                assertions.add(new Assertion(bool, assertion.span()));
            } else if (expr != null) {
                diagnostics.add(new Diagnostic(assertion.span(), "An assertion must be a constraint"));
            }
        }
    }

    private void declare(ClaferDecl decl, AstClafer parent) {
        String name = decl.name().text();
        if (clafers.containsKey(name)) {
            diagnostics.add(new Diagnostic(decl.name().span(), "A clafer named " + name + " is already declared"));
            return;
        }
        AstClafer clafer;
        if (decl.isAbstract()) {
            clafer = parent == null ? model.addAbstract(name) : parent.addAbstractChild(name);
            Ir.Card card = decl.card();
            if (!card.isAny()) {
                if (parent == null) {
                    diagnostics.add(new Diagnostic(card.span(),
                            "The multiplicity of the top-level abstract clafer " + name + " is rich"));
                } else {
                    // the number of the instances of its subclafers in each
                    // parent, a constraint of the parent
                    AstSetExpr own = Asts.card(Asts.join(Asts.$this(), clafer));
                    if (card.low() > 0) {
                        parent.addConstraint(Asts.greaterThanEqual(own, Asts.constant(card.low())), card.span());
                    }
                    if (!card.isUnbounded()) {
                        parent.addConstraint(Asts.lessThanEqual(own, Asts.constant(card.high())), card.span());
                    }
                }
            }
        } else {
            AstConcreteClafer concrete = parent == null ? model.addChild(name) : parent.addChild(name);
            concrete.withCard(card(decl.card()));
            clafer = concrete;
        }
        clafer.at(decl.name().span());
        clafers.put(name, clafer);
        declOf.put(clafer, decl);
        for (Element element : decl.elements()) {
            if (element instanceof ClaferDecl child) {
                declare(child, clafer);
            }
        }
    }

    private static Card card(Ir.Card card) {
        return card.isUnbounded() ? new Card(card.low()) : new Card(card.low(), card.high());
    }

    private void setSuper(AstClafer clafer, ClaferDecl decl) {
        if (decl.superName().isEmpty()) {
            return;
        }
        Ir.Name superName = decl.superName().get();
        AstClafer sup = clafers.get(superName.text());
        if (sup == null) {
            diagnostics.add(new Diagnostic(superName.span(), "No clafer named " + superName.text()));
        } else if (!(sup instanceof AstAbstractClafer abstractSuper) || sup.isPrimitive()) {
            diagnostics.add(new Diagnostic(superName.span(), superName.text() + " is not an abstract clafer"));
        } else {
            clafer.extending(abstractSuper);
        }
    }

    /**
     * Sets the reference of a clafer, if it has one written, and its group
     * card where it differs from what it inherits, after its super clafer's.
     */
    private void setInherited(AstClafer clafer, Set<AstClafer> done, Set<AstClafer> visiting) {
        if (done.contains(clafer) || !declOf.containsKey(clafer)) {
            return;
        }
        if (!visiting.add(clafer)) {
            diagnostics.add(new Diagnostic(declOf.get(clafer).name().span(), clafer.getName() + " extends itself"));
            return;
        }
        AstClafer sup = clafer.hasSuperClafer() ? clafer.getSuperClafer() : null;
        if (sup != null) {
            setInherited(sup, done, visiting);
        }
        ClaferDecl decl = declOf.get(clafer);
        // A written reference is the clafer's own, as in Clafer: on a sub
        // clafer it redefines the inherited one, even with the same target.
        if (decl.ref().isPresent()) {
            Ir.RefDecl ref = decl.ref().get();
            Ir.Name targetName = ref.target();
            AstClafer target = clafers.get(targetName.text());
            if (target == null) {
                diagnostics.add(new Diagnostic(targetName.span(), Unsolved.contains(targetName.text())
                        ? "Chocosolver does not solve " + targetName.text() + ", nor does any backend"
                        : "No clafer named " + targetName.text()));
            } else if (ref.isSet()) {
                clafer.refToUnique(target);
            } else {
                clafer.refTo(target);
            }
        }
        Card inheritedGroupCard = sup == null ? null : AstUtil.getInheritedGroupCard(sup);
        Card groupCard = card(decl.groupCard());
        boolean asInherited = inheritedGroupCard == null
                ? groupCard.getLow() == 0 && !groupCard.hasHigh()
                : inheritedGroupCard.equals(groupCard);
        if (!asInherited) {
            clafer.withGroupCard(groupCard);
        }
        visiting.remove(clafer);
        done.add(clafer);
    }

    private void addConstraint(AstClafer context, Ir.Constraint constraint) {
        AstExpr expr = convertTop(constraint.exp());
        if (expr instanceof AstBoolExpr bool) {
            context.addConstraint(bool, constraint.span());
        } else if (expr != null) {
            diagnostics.add(new Diagnostic(constraint.span(), "A constraint must be true or false, not a set or a number"));
        }
    }

    // ------------------------------------------------------------------
    // Expressions
    // ------------------------------------------------------------------
    /**
     * Converts an expression, or reports why not and returns null.
     */
    private AstExpr convertTop(Exp exp) {
        try {
            return convert(exp, false);
        } catch (BuildError e) {
            diagnostics.add(e.diagnostic);
            return null;
        } finally {
            locals.clear();
        }
    }

    private static BuildError error(Node at, String message) {
        return new BuildError(at.span(), message);
    }

    /**
     * @param relation whether names are child relations, as on the right of
     * a navigation
     */
    private AstExpr convert(Exp exp, boolean relation) {
        return switch (exp) {
            case Ir.This e -> new AstThis();
            case Ir.NameExp e -> name(e, relation);
            case Ir.IntLit e -> {
                if (e.value() < Integer.MIN_VALUE || e.value() > Integer.MAX_VALUE) {
                    throw error(e, "The number " + e.value() + " is too large");
                }
                yield Asts.constant((int) e.value());
            }
            case Ir.DecimalLit e -> new AstDecimalConstant(new BigDecimal(e.text()));
            case Ir.DoubleLit e -> throw error(e, "Chocosolver does not solve double, nor does any backend");
            // As from Clafer's JavaScript: the literal, with its quotes.
            case Ir.StringLit e -> new AstStringConstant(e.text());
            case Ir.NoClafer e -> noClafer(null);
            case Ir.Join e -> {
                AstSetExpr left = set(e, convert(e.left(), relation));
                if (e.right() instanceof Ir.NameExp n) {
                    yield new AstJoin(left, new AstChildRelation(clafer(n.span(), n.name())));
                }
                yield new AstJoin(left, set(e, convert(e.right(), true)));
            }
            case Ir.Dref e -> new AstJoinRef(set(e, convert(e.left(), relation)));
            case Ir.ParentOf e -> new AstJoinParent(set(e, convert(e.left(), relation)));
            case Ir.Unary e -> unary(e, relation);
            case Ir.Binary e -> binary(e, relation);
            case Ir.IfThenElse e -> {
                AstBoolExpr antecedent = bool(e, convert(e.condition(), relation));
                AstExpr consequent = convert(e.then(), relation);
                AstExpr alternative = convert(e.otherwise(), relation);
                if (consequent instanceof AstBoolExpr c && alternative instanceof AstBoolExpr a) {
                    yield new AstIfThenElse(antecedent, c, a);
                }
                AstSetExpr c = set(e, consequent);
                AstSetExpr a = set(e, alternative);
                yield new AstTernary(antecedent, typedBeside(c, a), typedBeside(a, c));
            }
            case Ir.Quantified e -> quantified(e);
            case Ir.QuantifiedSet e -> {
                AstSetExpr set = set(e, convert(e.set(), false));
                yield switch (e.quantifier()) {
                    case SOME -> Asts.some(set);
                    case NO -> Asts.none(set);
                    case LONE -> Asts.lone(set);
                    case ONE -> Asts.one(set);
                    case ALL -> throw error(e, "Chocosolver has no 'all' without variables");
                };
            }
            case Ir.Call e -> {
                AstSetExpr first = set(e, convert(e.args().get(0), false));
                AstSetExpr second = set(e, convert(e.args().get(1), false));
                yield switch (e.function()) {
                    case "concat" -> new AstConcat(first, second);
                    case "prefix" -> new AstPrefix(first, second);
                    default -> new AstSuffix(first, second);
                };
            }
            case Ir.RelationOf e -> {
                AstClafer source = clafer(e.clafer().span(), e.clafer().text());
                if (e.isDref()) {
                    if (!source.hasRef()) {
                        throw error(e.clafer(), e.clafer().text() + " has no reference of its own");
                    }
                    yield new AstRefRelation(source.getRef());
                }
                if (!(source instanceof AstConcreteClafer concrete)) {
                    throw error(e.clafer(), e.clafer().text() + " is not a concrete clafer");
                }
                yield new AstParentRelation(concrete);
            }
            case Ir.Cast e -> {
                AstSetExpr base = set(e, convert(e.base(), false));
                ProductType type = type(e.type());
                yield e.up() ? new AstUpcast(base, type) : new AstDowncast(base, type);
            }
            case Ir.Connected e -> new AstConnected(set(e, convert(e.nodes(), false)),
                    set(e, convert(e.relation(), false)), e.directed());
            case Ir.ConstantSet e -> {
                ProductType type = type(e.type());
                if (type.arity() != e.rows().size()) {
                    throw error(e, "The constant has " + e.rows().size() + " columns but its type " + type
                            + " has " + type.arity());
                }
                int[][] rows = e.rows().stream().map(r -> r.stream().mapToInt(Integer::intValue).toArray())
                        .toArray(int[][]::new);
                yield new AstConstant(type, rows);
            }
            case Ir.Temporal e -> throw error(e, "Chocosolver does not solve temporal models");
            case Ir.Anchor e -> throw error(e, "Chocosolver does not solve temporal models");
            case Ir.Pattern e -> throw error(e, "Chocosolver does not solve temporal models");
            case Ir.Transition e -> throw error(e, "Chocosolver does not solve temporal models");
        };
    }

    private AstExpr name(Ir.NameExp e, boolean relation) {
        for (Map<String, AstLocal> scope : locals) {
            AstLocal local = scope.get(e.name());
            if (local != null) {
                return local;
            }
        }
        AstClafer clafer = clafer(e.span(), e.name());
        return relation ? new AstChildRelation(clafer) : new AstGlobal(clafer);
    }

    private AstExpr unary(Ir.Unary e, boolean relation) {
        AstExpr arg = convert(e.arg(), relation);
        return switch (e.op()) {
            case NOT -> new AstNot(bool(e, arg));
            case MINUS -> new AstMinus(set(e, arg));
            case CARD -> new AstCard(set(e, arg));
            case SUM -> new AstSum(set(e, arg));
            case PRODUCT -> new AstProduct(set(e, arg));
            case MAX -> new AstMax(set(e, arg));
            case MIN -> new AstMin(set(e, arg));
            case LENGTH -> new AstLength(set(e, arg));
            case INVERSE -> new AstInverse(set(e, arg));
            case CLOSURE -> new AstTransitiveClosure(set(e, arg), false);
            case REFLEXIVE_CLOSURE -> new AstTransitiveClosure(set(e, arg), true);
        };
    }

    private AstExpr binary(Ir.Binary e, boolean relation) {
        List<AstExpr> operands = e.operands().stream().map(o -> convert(o, relation)).toList();
        BinaryOp op = e.op();
        if (ArithmOps.containsKey(op)) {
            return new AstArithm(ArithmOps.get(op), operands.stream().map(o -> set(e, o)).toArray(AstSetExpr[]::new));
        }
        if (BoolOps.containsKey(op)) {
            return new AstBoolArithm(BoolOps.get(op), operands.stream().map(o -> bool(e, o)).toArray(AstBoolExpr[]::new));
        }
        if (operands.size() > 2) {
            throw error(e, "Parenthesize: '" + op.syntax() + "' takes two operands");
        }
        AstSetExpr left0 = set(e, operands.get(0));
        AstSetExpr right0 = set(e, operands.get(1));
        AstSetExpr left = typedBeside(left0, right0);
        AstSetExpr right = typedBeside(right0, left0);
        return switch (op) {
            case LT, LE, GT, GE -> new AstCompare(left, CompareOps.get(op), right);
            case EQ -> new AstSetTest(left, AstSetTest.Op.Equal, right);
            case NE -> new AstSetTest(left, AstSetTest.Op.NotEqual, right);
            case IN -> new AstMembership(left, AstMembership.Op.In, right);
            case NOT_IN -> new AstMembership(left, AstMembership.Op.NotIn, right);
            case MOD -> new AstMod(left, right);
            case UNION -> new AstUnion(left, right);
            case DIFF -> new AstDifference(left, right);
            case INTER -> new AstIntersection(left, right);
            case DOMAIN -> new AstDomainRestriction(left, right);
            case RANGE -> new AstRangeRestriction(left, right);
            default -> throw error(e, "Unknown operator " + op.syntax());
        };
    }

    private AstExpr quantified(Ir.Quantified e) {
        AstQuantify.Quantifier quantifier = switch (e.quantifier()) {
            case ALL -> AstQuantify.Quantifier.All;
            case SOME -> AstQuantify.Quantifier.Some;
            case NO -> AstQuantify.Quantifier.None;
            case LONE -> AstQuantify.Quantifier.Lone;
            case ONE -> AstQuantify.Quantifier.One;
        };
        Map<String, AstLocal> scope = new HashMap<>();
        List<AstDecl> decls = new ArrayList<>();
        for (Ir.Decl decl : e.decls()) {
            List<AstLocal> names = decl.locals().stream().map(n -> Asts.local(n.text())).toList();
            // The body of a declaration sees the variables declared before it.
            locals.push(scope);
            AstSetExpr body;
            try {
                body = set(decl, convert(decl.set(), false));
            } finally {
                locals.pop();
            }
            decls.add(new AstDecl(decl.disjoint(), names.toArray(new AstLocal[0]), body));
            for (AstLocal local : names) {
                scope.put(local.getName(), local);
            }
        }
        locals.push(scope);
        try {
            return new AstQuantify(quantifier, decls.toArray(new AstDecl[0]), bool(e, convert(e.body(), false)));
        } finally {
            locals.pop();
        }
    }

    private ProductType type(List<Ir.Name> names) {
        return new ProductType(names.stream().map(n -> clafer(n.span(), n.text())).toArray(AstClafer[]::new));
    }

    private AstClafer clafer(SourceSpan span, String name) {
        AstClafer clafer = clafers.get(name);
        if (clafer == null) {
            throw new BuildError(span, Unsolved.contains(name)
                    ? "Chocosolver does not solve " + name + ", nor does any backend"
                    : "No clafer named " + name);
        }
        return clafer;
    }

    private static AstSetExpr set(Node at, AstExpr expr) {
        if (expr instanceof AstSetExpr set) {
            return set;
        }
        throw error(at, "Expected a set or a number, not a constraint");
    }

    private static AstBoolExpr bool(Node at, AstExpr expr) {
        if (expr instanceof AstBoolExpr bool) {
            return bool;
        }
        throw error(at, "Expected a constraint, not a set or a number");
    }

    // ------------------------------------------------------------------
    // noclafer, the empty set
    // ------------------------------------------------------------------
    /**
     * The noclafers built, by identity: each is an empty set of the model's
     * first clafer until a set beside it gives it its type.
     */
    private final Set<AstConstant> noClafers = Collections.newSetFromMap(new IdentityHashMap<>());

    /**
     * Clafer's {@code noclafer}: an empty set of the given clafer, as the
     * Clafer compiler writes it ({@code constant(c0_a)}), or, without one, of
     * the model's first clafer, as for {@code #noclafer}.
     */
    private AstConstant noClafer(AstClafer type) {
        AstClafer t = type != null ? type
                : declOf.keySet().stream().findFirst().orElse(Asts.IntType);
        AstConstant empty = Asts.constant(t);
        noClafers.add(empty);
        return empty;
    }

    /**
     * A noclafer beside another set, e.g., in {@code a ++ noclafer} or
     * {@code if c then a else noclafer}, is an empty set of that set's clafer.
     */
    private AstSetExpr typedBeside(AstSetExpr expr, AstSetExpr other) {
        if (expr instanceof AstConstant c && noClafers.contains(c) && !noClafers.contains(other)) {
            AstClafer type = claferOf(other);
            if (type != null) {
                return noClafer(type);
            }
        }
        return expr;
    }

    /**
     * The clafer whose instances a set is of, as far as the expression shows
     * it, or null.
     */
    private static AstClafer claferOf(AstSetExpr expr) {
        return switch (expr) {
            case AstGlobal e -> e.type();
            case AstJoin e when e.right() instanceof AstChildRelation r -> r.childType();
            case AstJoinRef e -> {
                AstClafer c = claferOf(e.deref());
                AstRef ref = c == null ? null : AstUtil.getInheritedRef(c);
                yield ref == null ? null : ref.getTargetType();
            }
            case AstJoinParent e -> {
                AstClafer c = claferOf(e.children());
                yield c == null || !c.hasParent() ? null : c.getParent();
            }
            case AstConstant e -> e.type().arity() == 1 ? e.type().get(0) : null;
            case AstUnion e -> firstNonNull(claferOf(e.left()), claferOf(e.right()));
            case AstIntersection e -> firstNonNull(claferOf(e.right()), claferOf(e.left()));
            case AstDifference e -> claferOf(e.left());
            case AstTernary e -> firstNonNull(claferOf(e.consequent()), claferOf(e.alternative()));
            default -> null;
        };
    }

    private static AstClafer firstNonNull(AstClafer a, AstClafer b) {
        return a != null ? a : b;
    }
}
