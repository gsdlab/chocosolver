package org.clafer.frontend;

import java.math.BigDecimal;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.clafer.assertion.Assertion;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstArithm;
import org.clafer.ast.AstBoolArithm;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstCard;
import org.clafer.ast.AstChildRelation;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstConcat;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConnected;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstDecimalConstant;
import org.clafer.ast.AstDecl;
import org.clafer.ast.AstDifference;
import org.clafer.ast.AstDomainRestriction;
import org.clafer.ast.AstDowncast;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstGlobal;
import org.clafer.ast.AstIfThenElse;
import org.clafer.ast.AstIntersection;
import org.clafer.ast.AstInverse;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstLength;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstMax;
import org.clafer.ast.AstMembership;
import org.clafer.ast.AstMin;
import org.clafer.ast.AstMinus;
import org.clafer.ast.AstMod;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstNot;
import org.clafer.ast.AstParentRelation;
import org.clafer.ast.AstPrefix;
import org.clafer.ast.AstProduct;
import org.clafer.ast.AstQuantify;
import org.clafer.ast.AstRangeRestriction;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstRefRelation;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstStringConstant;
import org.clafer.ast.AstSuffix;
import org.clafer.ast.AstSum;
import org.clafer.ast.AstTernary;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstTransitiveClosure;
import org.clafer.ast.AstUnion;
import org.clafer.ast.AstUpcast;
import org.clafer.ast.AstUtil;
import org.clafer.ast.Asts;
import org.clafer.ast.Card;
import org.clafer.ast.ProductType;
import org.clafer.ast.SourceSpan;
import org.clafer.objective.Objective;

/**
 * Reads the explicit form of a Clafer model, as
 * {@link org.clafer.ast.AstPrinter} prints it, into chocosolver's
 * {@link AstModel} with its objectives and assertions. Printing the result
 * gives the same text back. The scopes are not in the model: they are in its
 * {@code .cfr-scope} file, see {@link org.clafer.scope.ScopeFile}.
 * <p>
 * The explicit form has no sugar and no defaults: every card and group card
 * is given, every name is a UID, nesting is in braces, and expressions are
 * parenthesized except for navigation. Names may be used before they are
 * declared, so the parser reads the declarations first and the expressions
 * after all clafers exist.
 * <p>
 * What the printer writes because it applies, the parser sets only where it
 * is not inherited: a group card equal to the super clafer's is the super
 * clafer's, as in the model the printer printed. A reference is a declaration,
 * printed only where it is declared or redefined, so a written reference is
 * the clafer's own.
 * <p>
 * Clafers, constraints, objectives and assertions get the spans of their
 * source, so chocosolver's errors name their positions. Errors do not stop
 * the parser: it reports a diagnostic and goes on with the next declaration
 * or constraint.
 */
public final class ExplicitParser {

    /**
     * A parsed model.
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
     * An error at a token, unwinding to the declaration or expression that
     * contains it.
     */
    private static final class ParseError extends RuntimeException {

        private final Diagnostic diagnostic;

        ParseError(SourceSpan span, String message) {
            super(message, null, false, false);
            this.diagnostic = new Diagnostic(span, message);
        }
    }

    /**
     * The tokens of an expression, [start, end), parsed after the
     * declarations.
     */
    private record ExprRange(int start, int end, SourceSpan span) {
    }

    private sealed interface Element permits ClaferDecl, ConstraintDecl {
    }

    private record ClaferDecl(Token name, boolean isAbstract, Card groupCard, Token superName,
            Token refArrow, Token refTarget, Card card, Token cardStart, List<Element> elements) implements Element {
    }

    private record ConstraintDecl(ExprRange expr) implements Element {
    }

    private record ObjectiveDecl(boolean maximize, ExprRange expr) {
    }

    private static final Set<String> Quantifiers = Set.of("all", "some", "no", "lone", "one");
    private static final Map<String, AstArithm.Op> ArithmOps = Map.of(
            "+", AstArithm.Op.Add, "-", AstArithm.Op.Sub, "*", AstArithm.Op.Mul, "/", AstArithm.Op.Div,
            "*.", AstArithm.Op.DecimalMul, "/.", AstArithm.Op.DecimalDiv);
    private static final Map<String, AstBoolArithm.Op> BoolOps = Map.of(
            "&&", AstBoolArithm.Op.And, "||", AstBoolArithm.Op.Or, "=>", AstBoolArithm.Op.Implies,
            "<=>", AstBoolArithm.Op.IfOnlyIf, "xor", AstBoolArithm.Op.Xor);
    private static final Map<String, AstCompare.Op> CompareOps = Map.of(
            "<", AstCompare.Op.LessThan, "<=", AstCompare.Op.LessThanEqual,
            ">", AstCompare.Op.GreaterThan, ">=", AstCompare.Op.GreaterThanEqual);
    private static final Set<String> BinaryOps = Set.of(
            "<", "<=", ">", ">=", "=", "!=", "in", "not", "%", "++", "--", "**", "<:", ":>");

    private final List<Token> tokens;
    private final List<Diagnostic> diagnostics = new ArrayList<>();
    private int pos = 0;

    // Read by the first pass.
    private final List<Element> topLevel = new ArrayList<>();
    private final List<ExprRange> rootConstraints = new ArrayList<>();
    private final List<ObjectiveDecl> objectives = new ArrayList<>();
    private final List<ExprRange> assertions = new ArrayList<>();

    // Built by the second pass.
    private final AstModel model = Asts.newModel();
    private final Map<String, AstClafer> clafers = new LinkedHashMap<>();
    // In the order of declaration, which is the order in which sub clafers
    // extend their super clafer, as in the compiler's output.
    private final Map<AstClafer, ClaferDecl> declOf = new LinkedHashMap<>();
    private final Deque<Map<String, AstLocal>> locals = new ArrayDeque<>();

    private ExplicitParser(List<Token> tokens) {
        this.tokens = tokens;
        clafers.put("int", Asts.IntType);
        clafers.put("string", Asts.StringType);
        clafers.put("decimal", Asts.DecimalType);
        clafers.put("bool", Asts.BoolType);
    }

    /**
     * Parses the explicit form of a model.
     *
     * @param source the text
     * @return the model, its objectives and assertions, and the errors of the
     * lexer and the parser
     */
    public static Result parse(String source) {
        Lexer.Result lexed = Lexer.lex(source);
        List<Token> parseable = lexed.tokens().stream().filter(t -> t.kind() != TokenKind.ERROR).toList();
        ExplicitParser parser = new ExplicitParser(parseable);
        parser.diagnostics.addAll(lexed.diagnostics());
        parser.readDeclarations();
        parser.build();
        List<Diagnostic> diagnostics = new ArrayList<>(parser.diagnostics);
        diagnostics.sort((a, b) -> a.span().line() != b.span().line()
                ? Integer.compare(a.span().line(), b.span().line())
                : Integer.compare(a.span().column(), b.span().column()));
        return new Result(parser.model,
                parser.builtObjectives.toArray(new Objective[0]),
                parser.builtAssertions.toArray(new Assertion[0]),
                Collections.unmodifiableList(diagnostics));
    }

    // ------------------------------------------------------------------
    // Tokens
    // ------------------------------------------------------------------
    private Token peek() {
        return tokens.get(pos);
    }

    private Token peek(int offset) {
        return tokens.get(Math.min(pos + offset, tokens.size() - 1));
    }

    private Token next() {
        Token t = tokens.get(pos);
        if (t.kind() != TokenKind.EOF) {
            pos++;
        }
        return t;
    }

    private boolean atSymbol(String symbol) {
        return peek().isSymbol(symbol);
    }

    private boolean atWord(String word) {
        return peek().isWord(word);
    }

    private ParseError error(Token at, String message) {
        return new ParseError(at.span(), message);
    }

    private static String show(Token t) {
        return t.kind() == TokenKind.EOF ? "the end" : "'" + t.text() + "'";
    }

    private Token expectSymbol(String symbol) {
        if (!atSymbol(symbol)) {
            throw error(peek(), "Expected '" + symbol + "' but found " + show(peek()));
        }
        return next();
    }

    private Token expectName(String what) {
        if (peek().kind() != TokenKind.WORD) {
            throw error(peek(), "Expected " + what + " but found " + show(peek()));
        }
        return next();
    }

    private int expectInt() {
        boolean negative = false;
        if (atSymbol("-") && peek(1).kind() == TokenKind.INT) {
            next();
            negative = true;
        }
        Token t = peek();
        if (t.kind() != TokenKind.INT) {
            throw error(t, "Expected a number but found " + show(t));
        }
        next();
        try {
            int value = Integer.parseInt(t.text());
            return negative ? -value : value;
        } catch (NumberFormatException e) {
            throw error(t, "The number " + t.text() + " is too large");
        }
    }

    private Card expectCard() {
        Token start = peek();
        int low = expectInt();
        expectSymbol("..");
        if (atSymbol("*")) {
            next();
            return new Card(low);
        }
        int high = expectInt();
        if (low > high) {
            throw error(start, "The card " + low + ".." + high + " is empty");
        }
        return new Card(low, high);
    }

    private SourceSpan spanFrom(Token start, Token end) {
        return new SourceSpan(start.span().line(), start.span().column(), end.span().endLine(), end.span().endColumn());
    }

    // ------------------------------------------------------------------
    // First pass: declarations
    // ------------------------------------------------------------------
    private void readDeclarations() {
        while (peek().kind() != TokenKind.EOF) {
            int start = pos;
            try {
                readTopLevel();
            } catch (ParseError e) {
                diagnostics.add(e.diagnostic);
                recover(start);
            }
        }
    }

    /**
     * Skips to the start of the next top-level declaration: a token that can
     * start one at the beginning of a line, outside braces.
     */
    private void recover(int start) {
        if (pos == start) {
            next();
        }
        int depth = 0;
        while (peek().kind() != TokenKind.EOF) {
            Token t = peek();
            if (depth == 0 && t.span().column() == 1 && startsTopLevel(t)) {
                return;
            }
            if (t.isSymbol("{")) {
                depth++;
            } else if (t.isSymbol("}")) {
                depth = Math.max(0, depth - 1);
            }
            next();
        }
    }

    private boolean startsTopLevel(Token t) {
        return t.kind() == TokenKind.INT || t.isSymbol("[") || t.isSymbol("<<")
                || (t.kind() == TokenKind.WORD && Set.of("abstract", "root", "assert").contains(t.text()));
    }

    private void readTopLevel() {
        Token t = peek();
        if (t.isWord("root")) {
            next();
            expectSymbol("{");
            while (atSymbol("[")) {
                rootConstraints.add(readConstraint());
            }
            expectSymbol("}");
        } else if (t.isSymbol("<<")) {
            Token start = next();
            boolean maximize;
            if (atWord("max") || atWord("min")) {
                maximize = next().text().equals("max");
            } else {
                throw error(peek(), "Expected 'max' or 'min' but found " + show(peek()));
            }
            int exprStart = pos;
            while (!atSymbol(">>") && peek().kind() != TokenKind.EOF) {
                next();
            }
            int exprEnd = pos;
            Token end = expectSymbol(">>");
            objectives.add(new ObjectiveDecl(maximize, new ExprRange(exprStart, exprEnd, spanFrom(start, end))));
        } else if (t.isWord("assert")) {
            Token start = next();
            ExprRange expr = readConstraint();
            assertions.add(new ExprRange(expr.start(), expr.end(), spanFrom(start, tokens.get(pos - 1))));
        } else {
            topLevel.add(readElement());
        }
    }

    private Element readElement() {
        if (atSymbol("[")) {
            return new ConstraintDecl(readConstraint());
        }
        if (atSymbol("(")) {
            throw error(peek(), "Soft constraints are not supported");
        }
        return readClafer();
    }

    /**
     * {@code [ expr ]}: notes the tokens of the expression for the second
     * pass.
     */
    private ExprRange readConstraint() {
        Token open = expectSymbol("[");
        int start = pos;
        int depth = 1;
        while (peek().kind() != TokenKind.EOF) {
            if (atSymbol("[")) {
                depth++;
            } else if (atSymbol("]") && --depth == 0) {
                break;
            }
            next();
        }
        int end = pos;
        Token close = expectSymbol("]");
        if (end == start) {
            throw error(open, "Empty constraint");
        }
        return new ExprRange(start, end, spanFrom(open, close));
    }

    /**
     * {@code [abstract] gcard name [: super] [-> | ->> target] card { elements }}
     */
    private ClaferDecl readClafer() {
        boolean isAbstract = false;
        if (atWord("abstract")) {
            next();
            isAbstract = true;
        }
        if (peek().kind() != TokenKind.INT && !atSymbol("-")) {
            throw error(peek(), "Expected a clafer, a constraint, an objective or an assertion but found " + show(peek()));
        }
        Card groupCard = expectCard();
        Token name = expectName("the name of a clafer");
        Token superName = null;
        if (atSymbol(":")) {
            next();
            superName = expectName("the name of the super clafer");
        }
        Token refArrow = null, refTarget = null;
        if (atSymbol("->") || atSymbol("->>")) {
            refArrow = next();
            refTarget = expectName("the target of the reference");
        }
        Token cardStart = peek();
        Card card = expectCard();
        expectSymbol("{");
        List<Element> elements = new ArrayList<>();
        while (!atSymbol("}")) {
            if (peek().kind() == TokenKind.EOF) {
                throw error(peek(), "Expected '}' to close " + name.text());
            }
            int start = pos;
            try {
                elements.add(readElement());
            } catch (ParseError e) {
                // Go on with the next element of the same clafer.
                diagnostics.add(e.diagnostic);
                skipElement(start);
            }
        }
        next();
        return new ClaferDecl(name, isAbstract, groupCard, superName, refArrow, refTarget, card, cardStart, elements);
    }

    /**
     * Skips the rest of a broken element of a clafer: to its end at the same
     * depth, or to the clafer's closing brace.
     */
    private void skipElement(int start) {
        if (pos == start) {
            next();
        }
        int depth = 0;
        while (peek().kind() != TokenKind.EOF) {
            if (atSymbol("{")) {
                depth++;
            } else if (atSymbol("}")) {
                if (depth == 0) {
                    return;
                }
                depth--;
                if (depth == 0) {
                    next();
                    return;
                }
            } else if (depth == 0 && (atSymbol("[") || atWord("abstract"))) {
                return;
            }
            next();
        }
    }

    // ------------------------------------------------------------------
    // Second pass: the model
    // ------------------------------------------------------------------
    private final List<Objective> builtObjectives = new ArrayList<>();
    private final List<Assertion> builtAssertions = new ArrayList<>();

    private void build() {
        // The clafers, so that every name can be found.
        for (Element element : topLevel) {
            if (element instanceof ClaferDecl decl) {
                declare(decl, null);
            }
        }
        // Supers, then references and group cards, which depend on the
        // super clafers' ones.
        for (Map.Entry<AstClafer, ClaferDecl> entry : declOf.entrySet()) {
            setSuper(entry.getKey(), entry.getValue());
        }
        Set<AstClafer> done = new java.util.HashSet<>();
        for (AstClafer clafer : new ArrayList<>(declOf.keySet())) {
            setInherited(clafer, done, new java.util.HashSet<>());
        }
        // The expressions.
        for (Element element : topLevel) {
            if (element instanceof ConstraintDecl constraint) {
                addConstraint(model.getAbstractRoot(), constraint.expr());
            }
        }
        for (Map.Entry<AstClafer, ClaferDecl> entry : declOf.entrySet()) {
            for (Element element : entry.getValue().elements()) {
                if (element instanceof ConstraintDecl constraint) {
                    addConstraint(entry.getKey(), constraint.expr());
                }
            }
        }
        for (ExprRange range : rootConstraints) {
            addConstraint(model.getRoot(), range);
        }
        for (ObjectiveDecl objective : objectives) {
            AstExpr expr = parseExprRange(objective.expr());
            if (expr instanceof AstSetExpr set) {
                builtObjectives.add(objective.maximize()
                        ? Objective.maximize(set, objective.expr().span())
                        : Objective.minimize(set, objective.expr().span()));
            } else if (expr != null) {
                diagnostics.add(new Diagnostic(objective.expr().span(), "An objective must be a number, not a constraint"));
            }
        }
        for (ExprRange range : assertions) {
            AstExpr expr = parseExprRange(range);
            if (expr instanceof AstBoolExpr bool) {
                builtAssertions.add(new Assertion(bool, range.span()));
            } else if (expr != null) {
                diagnostics.add(new Diagnostic(range.span(), "An assertion must be a constraint"));
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
            if (decl.card().getLow() != 0 || decl.card().hasHigh()) {
                diagnostics.add(new Diagnostic(decl.cardStart().span(),
                        "The multiplicity of the abstract clafer " + name + " must be 0..*"));
            }
        } else {
            AstConcreteClafer concrete = parent == null ? model.addChild(name) : parent.addChild(name);
            concrete.withCard(decl.card());
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

    private void setSuper(AstClafer clafer, ClaferDecl decl) {
        if (decl.superName() == null) {
            return;
        }
        AstClafer sup = clafers.get(decl.superName().text());
        if (sup == null) {
            diagnostics.add(new Diagnostic(decl.superName().span(), "No clafer named " + decl.superName().text()));
        } else if (!(sup instanceof AstAbstractClafer abstractSuper) || sup.isPrimitive()) {
            diagnostics.add(new Diagnostic(decl.superName().span(), decl.superName().text() + " is not an abstract clafer"));
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
        if (decl.refTarget() != null) {
            AstClafer target = clafers.get(decl.refTarget().text());
            if (target == null) {
                diagnostics.add(new Diagnostic(decl.refTarget().span(), "No clafer named " + decl.refTarget().text()));
            } else if (decl.refArrow().text().equals("->")) {
                clafer.refToUnique(target);
            } else {
                clafer.refTo(target);
            }
        }
        Card inheritedGroupCard = sup == null ? null : AstUtil.getInheritedGroupCard(sup);
        Card groupCard = decl.groupCard();
        boolean asInherited = inheritedGroupCard == null
                ? groupCard.getLow() == 0 && !groupCard.hasHigh()
                : inheritedGroupCard.equals(groupCard);
        if (!asInherited) {
            clafer.withGroupCard(groupCard);
        }
        visiting.remove(clafer);
        done.add(clafer);
    }

    private void addConstraint(AstClafer context, ExprRange range) {
        AstExpr expr = parseExprRange(range);
        if (expr instanceof AstBoolExpr bool) {
            context.addConstraint(bool, range.span());
        } else if (expr != null) {
            diagnostics.add(new Diagnostic(range.span(), "A constraint must be true or false, not a set or a number"));
        }
    }

    // ------------------------------------------------------------------
    // Expressions
    // ------------------------------------------------------------------
    /**
     * Parses the tokens of an expression, or reports why not and returns
     * null.
     */
    private AstExpr parseExprRange(ExprRange range) {
        int saved = pos;
        pos = range.start();
        try {
            AstExpr expr = parseExpr(range.end(), false);
            if (pos != range.end()) {
                Token t = tokens.get(pos);
                throw error(t, "Unexpected " + show(t) + ": the explicit form needs parentheses around an operand that is not a name, a number or a navigation");
            }
            return expr;
        } catch (ParseError e) {
            diagnostics.add(e.diagnostic);
            return null;
        } finally {
            pos = saved;
            locals.clear();
        }
    }

    private Token at(int end) {
        return pos < end ? tokens.get(pos) : new Token(TokenKind.EOF, "", tokens.get(Math.max(0, end - 1)).span());
    }

    private Token take(int end) {
        Token t = at(end);
        if (pos >= end) {
            throw error(t, "The expression ends too early");
        }
        pos++;
        return t;
    }

    private void expect(int end, String symbol) {
        Token t = at(end);
        if (!t.isSymbol(symbol)) {
            throw error(t, "Expected '" + symbol + "' but found " + (pos >= end ? "the end of the expression" : show(t)));
        }
        pos++;
    }

    /**
     * expr := quantified | if | unary | operand (op operand)*
     *
     * @param relation whether names are child relations, as on the right of
     * a navigation
     */
    private AstExpr parseExpr(int end, boolean relation) {
        Token t = at(end);
        if (t.kind() == TokenKind.WORD && Quantifiers.contains(t.text())) {
            return parseQuantified(end);
        }
        if (t.isWord("if")) {
            pos++;
            AstBoolExpr antecedent = bool(t, parseOperand(end, relation));
            expectWordIn(end, "then");
            AstExpr consequent = parseOperand(end, relation);
            expectWordIn(end, "else");
            AstExpr alternative = parseOperand(end, relation);
            if (consequent instanceof AstBoolExpr c && alternative instanceof AstBoolExpr a) {
                return new AstIfThenElse(antecedent, c, a);
            }
            return new AstTernary(antecedent, set(t, consequent), set(t, alternative));
        }
        AstExpr unary = parseUnary(end, relation);
        if (unary != null) {
            return unary;
        }
        AstExpr first = parseOperand(end, relation);
        return parseOperators(first, end, relation);
    }

    private void expectWordIn(int end, String word) {
        Token t = at(end);
        if (!t.isWord(word)) {
            throw error(t, "Expected '" + word + "' but found " + (pos >= end ? "the end of the expression" : show(t)));
        }
        pos++;
    }

    /**
     * A prefix operator applied to an operand, or null if there is none here.
     * A minus before a number is part of the number.
     */
    private AstExpr parseUnary(int end, boolean relation) {
        Token t = at(end);
        if (t.kind() == TokenKind.SYMBOL) {
            switch (t.text()) {
                case "!" -> {
                    pos++;
                    return new AstNot(bool(t, parseOperand(end, relation)));
                }
                case "-" -> {
                    Token number = pos + 1 < end ? tokens.get(pos + 1) : null;
                    if (number != null && (number.kind() == TokenKind.INT || number.kind() == TokenKind.DECIMAL)) {
                        return null;
                    }
                    pos++;
                    return new AstMinus(set(t, parseOperand(end, relation)));
                }
                case "#" -> {
                    pos++;
                    return new AstCard(set(t, parseOperand(end, relation)));
                }
                case "~" -> {
                    pos++;
                    return new AstInverse(set(t, parseOperand(end, relation)));
                }
                case "^", "*" -> {
                    pos++;
                    return new AstTransitiveClosure(set(t, parseOperand(end, relation)), t.text().equals("*"));
                }
                default -> {
                    return null;
                }
            }
        }
        if (t.kind() == TokenKind.WORD) {
            switch (t.text()) {
                case "sum", "product" -> {
                    pos++;
                    // The printer writes the dref that sum and product take
                    // from each instance: drop it.
                    AstExpr operand = parseOperand(end, relation);
                    if (!(operand instanceof AstJoinRef joinRef)) {
                        throw error(t, t.text() + " takes the dref of each instance: write " + t.text() + " x . dref");
                    }
                    return t.text().equals("sum") ? new AstSum(joinRef.deref()) : new AstProduct(joinRef.deref());
                }
                case "max" -> {
                    pos++;
                    return new AstMax(set(t, parseOperand(end, relation)));
                }
                case "min" -> {
                    pos++;
                    return new AstMin(set(t, parseOperand(end, relation)));
                }
                case "length" -> {
                    pos++;
                    return new AstLength(set(t, parseOperand(end, relation)));
                }
                default -> {
                    return null;
                }
            }
        }
        return null;
    }

    /**
     * A chain of one binary operator. The explicit form has no precedence:
     * different operators need parentheses.
     */
    private AstExpr parseOperators(AstExpr first, int end, boolean relation) {
        Token op = at(end);
        String symbol = operator(end);
        if (symbol == null) {
            return first;
        }
        List<AstExpr> operands = new ArrayList<>();
        operands.add(first);
        while (true) {
            skipOperator(symbol);
            operands.add(parseOperand(end, relation));
            String nextSymbol = operator(end);
            if (nextSymbol == null) {
                break;
            }
            if (!nextSymbol.equals(symbol)) {
                throw error(at(end), "Parenthesize: '" + symbol + "' and '" + nextSymbol + "' without parentheses");
            }
        }
        if (ArithmOps.containsKey(symbol)) {
            return new AstArithm(ArithmOps.get(symbol), operands.stream().map(e -> set(op, e)).toArray(AstSetExpr[]::new));
        }
        if (BoolOps.containsKey(symbol)) {
            return new AstBoolArithm(BoolOps.get(symbol), operands.stream().map(e -> bool(op, e)).toArray(AstBoolExpr[]::new));
        }
        if (operands.size() > 2) {
            throw error(op, "Parenthesize: '" + symbol + "' takes two operands");
        }
        AstSetExpr left = set(op, operands.get(0));
        AstSetExpr right = set(op, operands.get(1));
        return switch (symbol) {
            case "<", "<=", ">", ">=" -> new AstCompare(left, CompareOps.get(symbol), right);
            case "=" -> new AstSetTest(left, AstSetTest.Op.Equal, right);
            case "!=" -> new AstSetTest(left, AstSetTest.Op.NotEqual, right);
            case "in" -> new AstMembership(left, AstMembership.Op.In, right);
            case "not in" -> new AstMembership(left, AstMembership.Op.NotIn, right);
            case "%" -> new AstMod(left, right);
            case "++" -> new AstUnion(left, right);
            case "--" -> new AstDifference(left, right);
            case "**" -> new AstIntersection(left, right);
            case "<:" -> new AstDomainRestriction(left, right);
            case ":>" -> new AstRangeRestriction(left, right);
            default -> throw error(op, "Unknown operator " + symbol);
        };
    }

    /**
     * The binary operator at the current token, or null.
     */
    private String operator(int end) {
        Token t = at(end);
        if (pos >= end) {
            return null;
        }
        if (t.isWord("not") && pos + 1 < end && tokens.get(pos + 1).isWord("in")) {
            return "not in";
        }
        if (t.isWord("in") || t.isWord("xor")) {
            return t.text();
        }
        if (t.kind() == TokenKind.SYMBOL && (ArithmOps.containsKey(t.text()) || BoolOps.containsKey(t.text())
                || BinaryOps.contains(t.text()))) {
            return t.text();
        }
        return null;
    }

    private void skipOperator(String symbol) {
        pos += symbol.equals("not in") ? 2 : 1;
    }

    /**
     * operand := primary ("." step)*
     */
    private AstExpr parseOperand(int end, boolean relation) {
        AstExpr expr = parsePrimary(end, relation);
        while (at(end).isSymbol(".") && pos < end) {
            Token dot = take(end);
            Token step = at(end);
            AstSetExpr left = set(dot, expr);
            if (step.isWord("dref") && !(pos + 1 < end && tokens.get(pos + 1).isSymbol("("))) {
                pos++;
                expr = new AstJoinRef(left);
            } else if (step.isWord("parent") && !(pos + 1 < end && tokens.get(pos + 1).isSymbol("("))) {
                pos++;
                expr = new AstJoinParent(left);
            } else if (step.kind() == TokenKind.WORD && !isCall(end)) {
                pos++;
                expr = new AstJoin(left, new AstChildRelation(clafer(step)));
            } else {
                expr = new AstJoin(left, set(dot, parsePrimary(end, true)));
            }
        }
        return expr;
    }

    private boolean isCall(int end) {
        Token t = at(end);
        return t.kind() == TokenKind.WORD && pos + 1 < end && tokens.get(pos + 1).isSymbol("(")
                && Set.of("upcast", "downcast", "concat", "prefix", "suffix", "connected", "dref", "parent").contains(t.text());
    }

    private AstExpr parsePrimary(int end, boolean relation) {
        Token t = at(end);
        if (pos >= end) {
            throw error(t, "The expression ends too early");
        }
        switch (t.kind()) {
            case INT, DECIMAL -> {
                pos++;
                return number(t, false);
            }
            case STRING -> {
                pos++;
                // As from Clafer's JavaScript: the literal, with its quotes.
                return new AstStringConstant(t.text());
            }
            case SYMBOL -> {
                switch (t.text()) {
                    case "(" -> {
                        pos++;
                        AstExpr inner = parseExpr(end, relation);
                        expect(end, ")");
                        return inner;
                    }
                    case "-" -> {
                        pos++;
                        Token number = take(end);
                        return number(number, true);
                    }
                    case "{" -> {
                        return parseConstantSet(end);
                    }
                    default -> throw error(t, "Unexpected " + show(t));
                }
            }
            case WORD -> {
                if (t.isWord("this")) {
                    pos++;
                    return new AstThis();
                }
                if (isCall(end)) {
                    return parseCall(end);
                }
                pos++;
                for (Map<String, AstLocal> scope : locals) {
                    AstLocal local = scope.get(t.text());
                    if (local != null) {
                        return local;
                    }
                }
                AstClafer clafer = clafer(t);
                return relation ? new AstChildRelation(clafer) : new AstGlobal(clafer);
            }
            default -> throw error(t, "Unexpected " + show(t));
        }
    }

    private AstSetExpr number(Token t, boolean negative) {
        if (t.kind() == TokenKind.DECIMAL) {
            BigDecimal value = new BigDecimal(t.text());
            return new AstDecimalConstant(negative ? value.negate() : value);
        }
        if (t.kind() != TokenKind.INT) {
            throw error(t, "Expected a number after '-' but found " + show(t));
        }
        try {
            int value = Integer.parseInt(t.text());
            return Asts.constant(negative ? -value : value);
        } catch (NumberFormatException e) {
            throw error(t, "The number " + t.text() + " is too large");
        }
    }

    /**
     * {@code {[1, 2], [3, 4]} :: type}: a constant set, one row per column of
     * the type.
     */
    private AstConstant parseConstantSet(int end) {
        Token open = take(end);
        List<int[]> rows = new ArrayList<>();
        do {
            expect(end, "[");
            List<Integer> row = new ArrayList<>();
            if (!at(end).isSymbol("]")) {
                do {
                    boolean negative = false;
                    if (at(end).isSymbol("-")) {
                        pos++;
                        negative = true;
                    }
                    Token n = take(end);
                    if (n.kind() != TokenKind.INT) {
                        throw error(n, "Expected a number but found " + show(n));
                    }
                    row.add(negative ? -Integer.parseInt(n.text()) : Integer.parseInt(n.text()));
                } while (at(end).isSymbol(",") && take(end) != null);
            }
            expect(end, "]");
            rows.add(row.stream().mapToInt(Integer::intValue).toArray());
        } while (at(end).isSymbol(",") && take(end) != null);
        expect(end, "}");
        expect(end, "::");
        ProductType type = parseType(end);
        if (type.arity() != rows.size()) {
            throw error(open, "The constant has " + rows.size() + " columns but its type " + type + " has " + type.arity());
        }
        return new AstConstant(type, rows.toArray(new int[0][]));
    }

    /**
     * type := name ("*" name)*
     */
    private ProductType parseType(int end) {
        List<AstClafer> types = new ArrayList<>();
        types.add(clafer(take(end)));
        while (at(end).isSymbol("*") && pos < end) {
            pos++;
            types.add(clafer(take(end)));
        }
        return new ProductType(types.toArray(new AstClafer[0]));
    }

    private AstExpr parseCall(int end) {
        Token name = take(end);
        expect(end, "(");
        AstExpr result;
        switch (name.text()) {
            case "dref" -> {
                Token clafer = take(end);
                AstClafer source = clafer(clafer);
                if (!source.hasRef()) {
                    throw error(clafer, clafer.text() + " has no reference of its own");
                }
                result = new AstRefRelation(source.getRef());
            }
            case "parent" -> {
                Token clafer = take(end);
                if (!(clafer(clafer) instanceof AstConcreteClafer concrete)) {
                    throw error(clafer, clafer.text() + " is not a concrete clafer");
                }
                result = new AstParentRelation(concrete);
            }
            case "upcast", "downcast" -> {
                AstSetExpr base = set(name, parseExpr(end, false));
                expect(end, ",");
                ProductType type = parseType(end);
                result = name.text().equals("upcast") ? new AstUpcast(base, type) : new AstDowncast(base, type);
            }
            case "connected" -> {
                AstSetExpr nodes = set(name, parseExpr(end, false));
                expect(end, ",");
                AstSetExpr relation = set(name, parseExpr(end, false));
                expect(end, ",");
                Token directed = take(end);
                if (!directed.isWord("directed") && !directed.isWord("undirected")) {
                    throw error(directed, "Expected 'directed' or 'undirected' but found " + show(directed));
                }
                result = new AstConnected(nodes, relation, directed.isWord("directed"));
            }
            default -> {
                AstSetExpr first = set(name, parseExpr(end, false));
                expect(end, ",");
                AstSetExpr second = set(name, parseExpr(end, false));
                result = switch (name.text()) {
                    case "concat" -> new AstConcat(first, second);
                    case "prefix" -> new AstPrefix(first, second);
                    default -> new AstSuffix(first, second);
                };
            }
        }
        expect(end, ")");
        return result;
    }

    /**
     * {@code q [disj] x, y : e; z : f | body}
     */
    private AstExpr parseQuantified(int end) {
        Token q = take(end);
        AstQuantify.Quantifier quantifier = switch (q.text()) {
            case "all" -> AstQuantify.Quantifier.All;
            case "some" -> AstQuantify.Quantifier.Some;
            case "no" -> AstQuantify.Quantifier.None;
            case "lone" -> AstQuantify.Quantifier.Lone;
            default -> AstQuantify.Quantifier.One;
        };
        Map<String, AstLocal> scope = new HashMap<>();
        List<AstDecl> decls = new ArrayList<>();
        do {
            boolean disjoint = false;
            if (at(end).isWord("disj")) {
                pos++;
                disjoint = true;
            }
            List<AstLocal> names = new ArrayList<>();
            do {
                Token name = take(end);
                if (name.kind() != TokenKind.WORD) {
                    throw error(name, "Expected the name of a variable but found " + show(name));
                }
                names.add(Asts.local(name.text()));
            } while (at(end).isSymbol(";") && take(end) != null);
            expect(end, ":");
            // The body of a declaration sees the variables declared before it.
            locals.push(scope);
            AstSetExpr body;
            try {
                body = set(q, parseExpr(end, false));
            } finally {
                locals.pop();
            }
            decls.add(new AstDecl(disjoint, names.toArray(new AstLocal[0]), body));
            for (AstLocal local : names) {
                scope.put(local.getName(), local);
            }
        } while (at(end).isSymbol(",") && take(end) != null);
        expect(end, "|");
        locals.push(scope);
        try {
            return new AstQuantify(quantifier, decls.toArray(new AstDecl[0]), bool(q, parseExpr(end, false)));
        } finally {
            locals.pop();
        }
    }

    private AstClafer clafer(Token name) {
        if (name.kind() != TokenKind.WORD) {
            throw error(name, "Expected the name of a clafer but found " + show(name));
        }
        AstClafer clafer = clafers.get(name.text());
        if (clafer == null) {
            throw error(name, "No clafer named " + name.text());
        }
        return clafer;
    }

    private AstSetExpr set(Token at, AstExpr expr) {
        if (expr instanceof AstSetExpr set) {
            return set;
        }
        throw error(at, "Expected a set or a number, not a constraint");
    }

    private AstBoolExpr bool(Token at, AstExpr expr) {
        if (expr instanceof AstBoolExpr bool) {
            return bool;
        }
        throw error(at, "Expected a constraint, not a set or a number");
    }
}
