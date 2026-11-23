package org.clafer.frontend;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.Set;
import org.clafer.frontend.Ir.*;

/**
 * Reads the rich or the core explicit language, as the Clafer compiler prints
 * them ({@code clafer -m clafer}, and {@code --core}), and the explicit form
 * that chocosolver's {@code AstPrinter} prints, into the {@link Ir explicit
 * IR}. {@link ExplicitPrinter} prints the tree back as it was read.
 * <p>
 * The parser does not resolve names, so it reads the model in one pass; what
 * reads the tree resolves them. It reads the options of the first line and
 * the provenance comments, {@code // construct, file:line:column}, at the end
 * of the line of a clafer's opening brace or of a constraint. Other comments
 * are not in the tree.
 * <p>
 * The explicit languages have no sugar and no defaults: every card and group
 * card is written, every name is a UID, nesting is in braces, and every
 * operand that is not an atom or a navigation is parenthesized, so the
 * expressions have no precedence. Errors are diagnostics: the parser goes on
 * with the next declaration or constraint.
 */
public final class ExplicitParser {

    /**
     * A parsed model.
     *
     * @param model the tree, with what could be read
     * @param diagnostics the errors of the lexer and the parser, by position;
     * empty if the model was read completely
     */
    public record Result(Model model, List<Diagnostic> diagnostics) {
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

    private static final Map<String, Quantifier> Quantifiers = Map.of(
            "all", Quantifier.ALL, "some", Quantifier.SOME, "no", Quantifier.NO,
            "lone", Quantifier.LONE, "one", Quantifier.ONE);
    private static final Map<String, ModifierKind> Modifiers = Map.of(
            "initial", ModifierKind.INITIAL, "final", ModifierKind.FINAL,
            "finalref", ModifierKind.FINALREF, "finaltarget", ModifierKind.FINALTARGET);
    private static final Map<String, BinaryOp> BinaryOps = new HashMap<>();
    private static final Map<String, TemporalOp> UnaryTemporal = new HashMap<>();
    private static final Map<String, TemporalOp> BinaryTemporal = new HashMap<>();
    private static final Map<String, PatternKind> UnaryPatterns = Map.of(
            "never", PatternKind.NEVER, "sometime", PatternKind.SOMETIME,
            "lonce", PatternKind.LONCE, "always", PatternKind.ALWAYS);
    private static final Set<String> Calls = Set.of(
            "concat", "prefix", "suffix", "upcast", "downcast", "connected", "dref", "parent");

    static {
        for (BinaryOp op : BinaryOp.values()) {
            BinaryOps.put(op.syntax(), op);
        }
        for (TemporalOp op : TemporalOp.values()) {
            (op.arity() == 1 ? UnaryTemporal : BinaryTemporal).put(op.keyword(), op);
        }
    }

    private final List<Token> tokens;
    private final Map<Integer, Token> lineComments = new HashMap<>();
    private final List<Diagnostic> diagnostics = new ArrayList<>();
    private int pos = 0;

    private ExplicitParser(List<Token> tokens, List<Token> comments) {
        this.tokens = tokens;
        for (Token comment : comments) {
            if (comment.text().startsWith("//") && !comment.text().startsWith("//#")) {
                lineComments.put(comment.span().line(), comment);
            }
        }
    }

    /**
     * Parses an explicit model.
     *
     * @param source the text
     * @return the tree and the errors
     */
    public static Result parse(String source) {
        Lexer.Result lexed = Lexer.lex(source);
        List<Token> parseable = lexed.tokens().stream().filter(t -> t.kind() != TokenKind.ERROR).toList();
        ExplicitParser parser = new ExplicitParser(parseable, lexed.comments());
        parser.diagnostics.addAll(lexed.diagnostics());
        Options options = parser.readOptions(lexed.comments());
        Model model = parser.readModel(options);
        List<Diagnostic> diagnostics = new ArrayList<>(parser.diagnostics);
        diagnostics.sort((a, b) -> a.span().line() != b.span().line()
                ? Integer.compare(a.span().line(), b.span().line())
                : Integer.compare(a.span().column(), b.span().column()));
        return new Result(model, Collections.unmodifiableList(diagnostics));
    }

    // ------------------------------------------------------------------
    // Options and provenance
    // ------------------------------------------------------------------
    /**
     * The options of the first line, {@code //# OPTIONS --explicit --core --tl=N}.
     */
    private Options readOptions(List<Token> comments) {
        Token first = comments.isEmpty() ? null : comments.get(0);
        if (first == null || first.span().line() != 1 || !first.text().startsWith("//# OPTIONS")) {
            return Options.Rich;
        }
        boolean core = false;
        OptionalInt traceLength = OptionalInt.empty();
        for (String option : first.text().substring("//# OPTIONS".length()).trim().split("\\s+")) {
            if (option.equals("--explicit") || option.isEmpty()) {
                continue;
            }
            if (option.equals("--core")) {
                core = true;
            } else if (option.matches("--tl=[0-9]+")) {
                traceLength = OptionalInt.of(Integer.parseInt(option.substring("--tl=".length())));
            } else {
                diagnostics.add(new Diagnostic(first.span(), "Unknown option of an explicit model: " + option));
            }
        }
        return new Options(core, traceLength);
    }

    /**
     * The provenance comment at the end of the line of a token, if any.
     */
    private Optional<Origin> originAfter(Token token) {
        Token comment = lineComments.get(token.span().endLine());
        if (comment == null || comment.span().column() < token.span().endColumn()) {
            return Optional.empty();
        }
        return Optional.of(origin(comment.text().substring(2).trim()));
    }

    /**
     * {@code construct, file:line:column}, or the construct alone.
     */
    static Origin origin(String text) {
        int comma = text.lastIndexOf(", ");
        if (comma >= 0) {
            String[] parts = text.substring(comma + 2).split(":");
            if (parts.length >= 3 && parts[parts.length - 1].matches("[0-9]+") && parts[parts.length - 2].matches("[0-9]+")) {
                String file = String.join(":", java.util.Arrays.copyOf(parts, parts.length - 2));
                return new Origin(text.substring(0, comma), Optional.of(new Position(file,
                        Integer.parseInt(parts[parts.length - 2]), Integer.parseInt(parts[parts.length - 1]))));
            }
        }
        return new Origin(text, Optional.empty());
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

    private Token previous() {
        return tokens.get(Math.max(0, pos - 1));
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

    private Name expectName(String what) {
        if (peek().kind() != TokenKind.WORD) {
            throw error(peek(), "Expected " + what + " but found " + show(peek()));
        }
        Token t = next();
        return new Name(t.span(), t.text());
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
            return new Card(spanFrom(start, previous()), low, Card.Unbounded);
        }
        int high = expectInt();
        if (low > high) {
            throw error(start, "The card " + low + ".." + high + " is empty");
        }
        return new Card(spanFrom(start, previous()), low, high);
    }

    private static SourceSpan spanFrom(Token start, Token end) {
        return spanFrom(start.span(), end.span());
    }

    private static SourceSpan spanFrom(SourceSpan start, SourceSpan end) {
        if (end.endLine() < start.line() || (end.endLine() == start.line() && end.endColumn() < start.column())) {
            return start;
        }
        return new SourceSpan(start.line(), start.column(), end.endLine(), end.endColumn());
    }

    // ------------------------------------------------------------------
    // Declarations
    // ------------------------------------------------------------------
    private Model readModel(Options options) {
        List<Element> elements = new ArrayList<>();
        List<Objective> objectives = new ArrayList<>();
        List<Assertion> assertions = new ArrayList<>();
        Token first = peek();
        while (peek().kind() != TokenKind.EOF) {
            int start = pos;
            try {
                Token t = peek();
                if (t.isSymbol("<<")) {
                    objectives.add(readObjective());
                } else if (t.isWord("assert")) {
                    Token at = next();
                    Token open = peek();
                    Exp exp = readBracketed();
                    Token close = previous();
                    assertions.add(new Assertion(spanFrom(at, close), exp, originAfter(close)));
                    if (open == close) {
                        throw error(open, "Empty assertion");
                    }
                } else if (t.isWord("root") && peek(1).isSymbol("{")) {
                    elements.add(readRoot());
                } else if (t.isWord("enum") && peek(1).kind() == TokenKind.WORD && peek(2).isSymbol("=")) {
                    elements.add(readEnum());
                } else {
                    elements.add(readElement(true));
                }
            } catch (ParseError e) {
                diagnostics.add(e.diagnostic);
                recover(start);
            }
        }
        return new Model(spanFrom(first, peek()), options, elements, objectives, assertions);
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

    private static boolean startsTopLevel(Token t) {
        return t.kind() == TokenKind.INT || t.isSymbol("[") || t.isSymbol("<<")
                || (t.kind() == TokenKind.WORD && (Set.of("abstract", "root", "assert", "enum").contains(t.text())
                || Modifiers.containsKey(t.text())));
    }

    private Objective readObjective() {
        Token start = expectSymbol("<<");
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
        if (exprEnd == exprStart) {
            throw error(end, "Empty objective");
        }
        return new Objective(spanFrom(start, end), maximize, parseRange(exprStart, exprEnd));
    }

    private RootBlock readRoot() {
        Token start = next();
        expectSymbol("{");
        List<Constraint> constraints = new ArrayList<>();
        while (atSymbol("[")) {
            constraints.add(readConstraint());
        }
        Token end = expectSymbol("}");
        return new RootBlock(spanFrom(start, end), constraints);
    }

    /**
     * {@code enum E = a | b}
     */
    private EnumDecl readEnum() {
        Token start = next();
        Name name = expectName("the name of the enumeration");
        expectSymbol("=");
        List<Name> literals = new ArrayList<>();
        literals.add(expectName("a literal"));
        while (atSymbol("|")) {
            next();
            literals.add(expectName("a literal"));
        }
        return new EnumDecl(spanFrom(start, previous()), name, literals);
    }

    private Element readElement(boolean topLevel) {
        if (atSymbol("[")) {
            return readConstraint();
        }
        if (atSymbol("(")) {
            throw error(peek(), "Soft constraints are not supported");
        }
        return readClafer(topLevel);
    }

    private Constraint readConstraint() {
        Token open = peek();
        Exp exp = readBracketed();
        Token close = previous();
        return new Constraint(spanFrom(open, close), exp, originAfter(close));
    }

    /**
     * {@code [ exp ]}
     */
    private Exp readBracketed() {
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
        expectSymbol("]");
        if (end == start) {
            throw error(open, "Empty constraint");
        }
        return parseRange(start, end);
    }

    /**
     * {@code [abstract] [modifiers] gcard name [: super] [-> | ->> target] card [initializer] { elements }}
     */
    private ClaferDecl readClafer(boolean topLevel) {
        Token start = peek();
        boolean isAbstract = false;
        if (atWord("abstract")) {
            next();
            isAbstract = true;
        }
        List<Modifier> modifiers = new ArrayList<>();
        while (peek().kind() == TokenKind.WORD && Modifiers.containsKey(peek().text())) {
            Token m = next();
            modifiers.add(new Modifier(m.span(), Modifiers.get(m.text())));
        }
        if (peek().kind() != TokenKind.INT && !atSymbol("-")) {
            throw error(peek(), "Expected a clafer, a constraint, an objective or an assertion but found " + show(peek()));
        }
        Card groupCard = expectCard();
        Name name = expectName("the name of a clafer");
        Optional<Name> superName = Optional.empty();
        if (atSymbol(":")) {
            next();
            superName = Optional.of(expectName("the name of the super clafer"));
        }
        Optional<RefDecl> ref = Optional.empty();
        if (atSymbol("->") || atSymbol("->>")) {
            Token arrow = next();
            Name target = expectName("the target of the reference");
            ref = Optional.of(new RefDecl(spanFrom(arrow.span(), target.span()), arrow.text().equals("->"), target));
        }
        Card card = expectCard();
        // The written bound of a top-level abstract clafer counts all its
        // instances, a rich construct; the core has it as a constraint.
        Optional<AbstractMultiplicity> abstractMultiplicity = Optional.empty();
        if (isAbstract && topLevel && !card.isAny()) {
            abstractMultiplicity = Optional.of(new AbstractMultiplicity(card.span(), card));
            card = new Card(card.span(), 0, Card.Unbounded);
        }
        Optional<Initializer> initializer = Optional.empty();
        if (atSymbol("=") || atSymbol(":=")) {
            Token op = next();
            InitKind kind = InitKind.FIXED;
            if (op.text().equals(":=")) {
                if (atWord("each")) {
                    kind = InitKind.EACH;
                } else if (atWord("together")) {
                    kind = InitKind.TOGETHER;
                } else {
                    throw error(peek(), "Expected 'each' or 'together' after ':=' but found " + show(peek()));
                }
                next();
            }
            int valueStart = pos;
            int valueEnd = endOfValue();
            if (valueEnd == valueStart) {
                throw error(peek(), "Expected the value of the initializer but found " + show(peek()));
            }
            Exp value = parseRange(valueStart, valueEnd);
            pos = valueEnd;
            initializer = Optional.of(new Initializer(spanFrom(op.span(), value.span()), kind, value));
        }
        Token open = expectSymbol("{");
        Optional<Origin> origin = originAfter(open);
        List<Element> elements = new ArrayList<>();
        while (!atSymbol("}")) {
            if (peek().kind() == TokenKind.EOF) {
                throw error(peek(), "Expected '}' to close " + name.text());
            }
            int elementStart = pos;
            try {
                elements.add(readElement(false));
            } catch (ParseError e) {
                // Go on with the next element of the same clafer.
                diagnostics.add(e.diagnostic);
                skipElement(elementStart);
            }
        }
        Token close = expectSymbol("}");
        if (elements.isEmpty() && origin.isEmpty()) {
            origin = originAfter(close);
        }
        return new ClaferDecl(spanFrom(start, close), isAbstract, modifiers, groupCard, name, superName, ref, card,
                abstractMultiplicity, initializer, elements, origin);
    }

    /**
     * The end of the value of an initializer: the clafer's opening brace,
     * outside parentheses.
     */
    private int endOfValue() {
        int depth = 0;
        for (int i = pos; i < tokens.size(); i++) {
            Token t = tokens.get(i);
            if (t.kind() == TokenKind.EOF) {
                return i;
            }
            if (t.isSymbol("(") || t.isSymbol("[") || t.isSymbol("-[")) {
                depth++;
            } else if (t.isSymbol(")") || t.isSymbol("]") || t.isSymbol("]->") || t.isSymbol("]->>")) {
                depth--;
            } else if (t.isSymbol("{") && depth == 0) {
                return i;
            }
        }
        return tokens.size() - 1;
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
    // Expressions
    // ------------------------------------------------------------------
    /**
     * Parses the tokens [start, end) as one expression.
     */
    private Exp parseRange(int start, int end) {
        int saved = pos;
        pos = start;
        try {
            Exp exp = parseExpr(end);
            if (pos != end) {
                Token t = tokens.get(pos);
                throw error(t, "Unexpected " + show(t) + ": the explicit form needs parentheses around an operand that is not a name, a number or a navigation");
            }
            return exp;
        } finally {
            pos = saved;
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

    private void expectWord(int end, String word) {
        Token t = at(end);
        if (!t.isWord(word)) {
            throw error(t, "Expected '" + word + "' but found " + (pos >= end ? "the end of the expression" : show(t)));
        }
        pos++;
    }

    private SourceSpan from(Token start) {
        return spanFrom(start, previous());
    }

    /**
     * Whether the token after the current one starts an operand, so that a
     * keyword here is an operator, e.g., {@code F (x)}, and not a clafer of
     * that name, e.g., {@code F = x} or {@code F.x}.
     */
    private boolean operandFollows(int end) {
        if (pos + 1 >= end) {
            return false;
        }
        Token t = tokens.get(pos + 1);
        return switch (t.kind()) {
            case WORD -> !BinaryOps.containsKey(t.text()) && !BinaryTemporal.containsKey(t.text())
                    && !Set.of("not", "must", "then", "else", "before", "after", "between", "and", "until").contains(t.text());
            case INT, DECIMAL, DOUBLE, STRING -> true;
            case SYMBOL -> Set.of("(", "!", "#", "~", "^", "{").contains(t.text())
                    || (t.text().equals("-") && pos + 2 < end);
            default -> false;
        };
    }

    /**
     * expr := quantified | if | pattern | temporal | anchor | unary
     * | operand [must ... | transition | (op operand)*]
     */
    private Exp parseExpr(int end) {
        Token t = at(end);
        if (t.kind() == TokenKind.WORD) {
            if (Quantifiers.containsKey(t.text()) && operandFollows(end)) {
                return parseQuantified(end);
            }
            if (t.isWord("if")) {
                pos++;
                Exp condition = parseOperand(end);
                expectWord(end, "then");
                Exp then = parseOperand(end);
                expectWord(end, "else");
                Exp otherwise = parseOperand(end);
                return new IfThenElse(from(t), condition, then, otherwise);
            }
            if (UnaryPatterns.containsKey(t.text()) && operandFollows(end)) {
                pos++;
                List<Exp> args = new ArrayList<>();
                args.add(parseOperand(end));
                return parseScope(t, UnaryPatterns.get(t.text()), args, end);
            }
            if (UnaryTemporal.containsKey(t.text()) && operandFollows(end)) {
                pos++;
                Exp arg = parseOperand(end);
                return new Temporal(from(t), UnaryTemporal.get(t.text()), List.of(arg));
            }
            if ((t.isWord("initially") || t.isWord("finally")) && operandFollows(end)) {
                pos++;
                Exp body = parseOperand(end);
                return new Anchor(from(t), t.isWord("initially"), body);
            }
        }
        Exp unary = parseUnary(end);
        if (unary != null) {
            return unary;
        }
        Exp first = parseOperand(end);
        Token next = at(end);
        if (pos < end && next.isWord("must")) {
            pos++;
            Token kind = take(end);
            PatternKind pattern = kind.isWord("precede") ? PatternKind.PRECEDE
                    : kind.isWord("follow") ? PatternKind.FOLLOW : null;
            if (pattern == null) {
                throw error(kind, "Expected 'precede' or 'follow' but found " + show(kind));
            }
            List<Exp> args = new ArrayList<>();
            args.add(first);
            args.add(parseOperand(end));
            return parseScope(t, pattern, args, end);
        }
        if (pos < end && (next.isSymbol("-->") || next.isSymbol("-->>") || next.isSymbol("-["))) {
            return parseTransition(t, first, end);
        }
        return parseOperators(t, first, end);
    }

    /**
     * The scope of a pattern, after its own formulas.
     */
    private Exp parseScope(Token start, PatternKind kind, List<Exp> args, int end) {
        PatternScope scope = PatternScope.GLOBALLY;
        Token t = at(end);
        if (pos < end && t.isWord("before")) {
            pos++;
            scope = PatternScope.BEFORE;
            args.add(parseOperand(end));
        } else if (pos < end && t.isWord("after")) {
            pos++;
            args.add(parseOperand(end));
            scope = PatternScope.AFTER;
            if (pos < end && at(end).isWord("until")) {
                pos++;
                args.add(parseOperand(end));
                scope = PatternScope.AFTER_UNTIL;
            }
        } else if (pos < end && t.isWord("between")) {
            pos++;
            args.add(parseOperand(end));
            expectWord(end, "and");
            args.add(parseOperand(end));
            scope = PatternScope.BETWEEN;
        }
        return new Pattern(from(start), kind, scope, args);
    }

    /**
     * {@code from --> to}, {@code from -[guard]-> to}, and the synchronous
     * {@code -->>} and {@code -[guard]->>}; the target may be a chain.
     */
    private Exp parseTransition(Token start, Exp from, int end) {
        Token arrow = take(end);
        Optional<Exp> guard = Optional.empty();
        boolean sync;
        if (arrow.isSymbol("-[")) {
            guard = Optional.of(parseOperand(end));
            Token close = take(end);
            if (!close.isSymbol("]->") && !close.isSymbol("]->>")) {
                throw error(close, "Expected ']->' or ']->>' but found " + show(close));
            }
            sync = close.isSymbol("]->>");
        } else {
            sync = arrow.isSymbol("-->>");
        }
        Token targetStart = at(end);
        Exp to = parseOperand(end);
        Token next = at(end);
        if (pos < end && (next.isSymbol("-->") || next.isSymbol("-->>") || next.isSymbol("-["))) {
            to = parseTransition(targetStart, to, end);
        }
        return new Transition(from(start), sync, from, guard, to);
    }

    /**
     * A prefix operator applied to an operand, or null if there is none here.
     * A minus before a number is part of the number.
     */
    private Exp parseUnary(int end) {
        Token t = at(end);
        if (pos >= end) {
            return null;
        }
        UnaryOp op = null;
        if (t.kind() == TokenKind.SYMBOL) {
            op = switch (t.text()) {
                case "!" -> UnaryOp.NOT;
                case "-" -> {
                    Token number = pos + 1 < end ? tokens.get(pos + 1) : null;
                    yield number != null && (number.kind() == TokenKind.INT || number.kind() == TokenKind.DECIMAL
                            || number.kind() == TokenKind.DOUBLE) ? null : UnaryOp.MINUS;
                }
                case "#" -> UnaryOp.CARD;
                case "~" -> UnaryOp.INVERSE;
                case "^" -> UnaryOp.CLOSURE;
                case "*" -> UnaryOp.REFLEXIVE_CLOSURE;
                default -> null;
            };
        } else if (t.kind() == TokenKind.WORD && operandFollows(end)) {
            op = switch (t.text()) {
                case "sum" -> UnaryOp.SUM;
                case "product" -> UnaryOp.PRODUCT;
                case "max" -> UnaryOp.MAX;
                case "min" -> UnaryOp.MIN;
                case "length" -> UnaryOp.LENGTH;
                default -> null;
            };
        }
        if (op == null) {
            return null;
        }
        pos++;
        Exp arg = parseOperand(end);
        if (op == UnaryOp.SUM || op == UnaryOp.PRODUCT) {
            // The dref that sum and product take from each instance is
            // printed, but not in the tree.
            if (!(arg instanceof Dref dref)) {
                throw error(t, t.text() + " takes the dref of each instance: write " + t.text() + " x.dref");
            }
            arg = dref.left();
        }
        return new Unary(from(t), op, arg);
    }

    /**
     * A chain of one binary operator. The explicit form has no precedence:
     * different operators need parentheses.
     */
    private Exp parseOperators(Token start, Exp first, int end) {
        String symbol = operator(end);
        if (symbol == null) {
            return first;
        }
        Token op = at(end);
        List<Exp> operands = new ArrayList<>();
        operands.add(first);
        while (true) {
            pos += symbol.equals("not in") ? 2 : 1;
            operands.add(parseOperand(end));
            String nextSymbol = operator(end);
            if (nextSymbol == null) {
                break;
            }
            if (!nextSymbol.equals(symbol)) {
                throw error(at(end), "Parenthesize: '" + symbol + "' and '" + nextSymbol + "' without parentheses");
            }
        }
        if (BinaryTemporal.containsKey(symbol)) {
            if (operands.size() > 2) {
                throw error(op, "Parenthesize: '" + symbol + "' takes two operands");
            }
            return new Temporal(from(start), BinaryTemporal.get(symbol), operands);
        }
        return new Binary(from(start), BinaryOps.get(symbol), operands);
    }

    /**
     * The binary operator at the current token, or null.
     */
    private String operator(int end) {
        if (pos >= end) {
            return null;
        }
        Token t = at(end);
        if (t.isWord("not") && pos + 1 < end && tokens.get(pos + 1).isWord("in")) {
            return "not in";
        }
        if (t.kind() == TokenKind.WORD && (t.text().equals("in") || t.text().equals("xor")
                || BinaryTemporal.containsKey(t.text()))) {
            return t.text();
        }
        if (t.kind() == TokenKind.SYMBOL && BinaryOps.containsKey(t.text())) {
            return t.text();
        }
        return null;
    }

    /**
     * operand := primary ("." step)*
     */
    private Exp parseOperand(int end) {
        Token start = at(end);
        Exp exp = parsePrimary(end);
        while (pos < end && at(end).isSymbol(".")) {
            pos++;
            Token step = at(end);
            boolean call = pos + 1 < end && tokens.get(pos + 1).isSymbol("(");
            if (step.isWord("dref") && !call) {
                pos++;
                exp = new Dref(from(start), exp);
            } else if (step.isWord("parent") && !call) {
                pos++;
                exp = new ParentOf(from(start), exp);
            } else if (step.kind() == TokenKind.WORD && !isCall(end)) {
                pos++;
                exp = new Join(from(start), exp, new NameExp(step.span(), step.text()));
            } else {
                Exp right = parsePrimary(end);
                exp = new Join(from(start), exp, right);
            }
        }
        return exp;
    }

    private boolean isCall(int end) {
        Token t = at(end);
        return t.kind() == TokenKind.WORD && pos + 1 < end && tokens.get(pos + 1).isSymbol("(")
                && Calls.contains(t.text());
    }

    private Exp parsePrimary(int end) {
        Token t = at(end);
        if (pos >= end) {
            throw error(t, "The expression ends too early");
        }
        switch (t.kind()) {
            case INT, DECIMAL, DOUBLE -> {
                pos++;
                return number(t, t, false);
            }
            case STRING -> {
                pos++;
                return new StringLit(t.span(), t.text());
            }
            case SYMBOL -> {
                switch (t.text()) {
                    case "(" -> {
                        pos++;
                        Exp inner = parseExpr(end);
                        expect(end, ")");
                        return inner;
                    }
                    case "-" -> {
                        pos++;
                        Token number = take(end);
                        return number(t, number, true);
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
                    return new This(t.span());
                }
                if (t.isWord("noclafer")) {
                    pos++;
                    return new NoClafer(t.span());
                }
                if (isCall(end)) {
                    return parseCall(end);
                }
                pos++;
                return new NameExp(t.span(), t.text());
            }
            default -> throw error(t, "Unexpected " + show(t));
        }
    }

    private Exp number(Token start, Token t, boolean negative) {
        SourceSpan span = spanFrom(start, t);
        return switch (t.kind()) {
            case DECIMAL -> new DecimalLit(span, (negative ? "-" : "") + t.text());
            case DOUBLE -> new DoubleLit(span, (negative ? "-" : "") + t.text());
            case INT -> {
                try {
                    long value = Long.parseLong(t.text());
                    yield new IntLit(span, negative ? -value : value);
                } catch (NumberFormatException e) {
                    throw error(t, "The number " + t.text() + " is too large");
                }
            }
            default -> throw error(t, "Expected a number after '-' but found " + show(t));
        };
    }

    /**
     * {@code {[1, 2], [3, 4]} :: type}: a constant set, one row per column of
     * the type.
     */
    private Exp parseConstantSet(int end) {
        Token open = take(end);
        List<List<Integer>> rows = new ArrayList<>();
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
            rows.add(row);
        } while (at(end).isSymbol(",") && take(end) != null);
        expect(end, "}");
        expect(end, "::");
        List<Name> type = parseType(end);
        return new ConstantSet(from(open), rows, type);
    }

    /**
     * type := name ("*" name)*
     */
    private List<Name> parseType(int end) {
        List<Name> types = new ArrayList<>();
        types.add(typeName(take(end)));
        while (pos < end && at(end).isSymbol("*")) {
            pos++;
            types.add(typeName(take(end)));
        }
        return types;
    }

    private Name typeName(Token t) {
        if (t.kind() != TokenKind.WORD) {
            throw error(t, "Expected the name of a clafer but found " + show(t));
        }
        return new Name(t.span(), t.text());
    }

    private Exp parseCall(int end) {
        Token name = take(end);
        expect(end, "(");
        Exp result;
        switch (name.text()) {
            case "dref", "parent" -> {
                Name clafer = typeName(take(end));
                expect(end, ")");
                return new RelationOf(from(name), name.text().equals("dref"), clafer);
            }
            case "upcast", "downcast" -> {
                Exp base = parseExpr(end);
                expect(end, ",");
                List<Name> type = parseType(end);
                expect(end, ")");
                return new Cast(from(name), name.text().equals("upcast"), base, type);
            }
            case "connected" -> {
                Exp nodes = parseExpr(end);
                expect(end, ",");
                Exp relation = parseExpr(end);
                expect(end, ",");
                Token directed = take(end);
                if (!directed.isWord("directed") && !directed.isWord("undirected")) {
                    throw error(directed, "Expected 'directed' or 'undirected' but found " + show(directed));
                }
                expect(end, ")");
                return new Connected(from(name), nodes, relation, directed.isWord("directed"));
            }
            default -> {
                Exp first = parseExpr(end);
                expect(end, ",");
                Exp second = parseExpr(end);
                expect(end, ")");
                result = new Call(from(name), name.text(), List.of(first, second));
            }
        }
        return result;
    }

    /**
     * {@code q [disj] x; y : e, z : f | body}, or {@code all e}
     */
    private Exp parseQuantified(int end) {
        Token q = take(end);
        Quantifier quantifier = Quantifiers.get(q.text());
        // all e, without declarations
        boolean declares = at(end).isWord("disj") || (at(end).kind() == TokenKind.WORD && pos + 1 < end
                && (tokens.get(pos + 1).isSymbol(":") || tokens.get(pos + 1).isSymbol(";")));
        if (!declares) {
            if (quantifier != Quantifier.ALL) {
                throw error(at(end), "Expected the variables of '" + q.text() + "' but found " + show(at(end)));
            }
            Exp set = parseOperand(end);
            return new AllOf(from(q), set);
        }
        List<Decl> decls = new ArrayList<>();
        do {
            Token declStart = at(end);
            boolean disjoint = false;
            if (at(end).isWord("disj")) {
                pos++;
                disjoint = true;
            }
            List<Name> names = new ArrayList<>();
            do {
                Token name = take(end);
                if (name.kind() != TokenKind.WORD) {
                    throw error(name, "Expected the name of a variable but found " + show(name));
                }
                names.add(new Name(name.span(), name.text()));
            } while (at(end).isSymbol(";") && take(end) != null);
            expect(end, ":");
            Exp set = parseExpr(end);
            decls.add(new Decl(from(declStart), disjoint, names, set));
        } while (at(end).isSymbol(",") && take(end) != null);
        expect(end, "|");
        Exp body = parseExpr(end);
        return new Quantified(from(q), quantifier, decls, body);
    }
}
