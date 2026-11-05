package org.clafer.ast;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import org.clafer.assertion.Assertion;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;

/**
 * Prints a model in a Clafer-like syntax with nothing left implicit: the
 * clafers by their names (the compiler's UIDs), every card and group card,
 * every {@code this}, {@code parent} and {@code dref}, and the scopes. Two
 * models print the same when they have the same clafers, constraints, scopes,
 * objectives and assertions, so the output can be compared, e.g., between two
 * compilers of the same Clafer model. The solver options (e.g., branching
 * priorities) are not printed.
 * <p>
 * The clafers, their children and their constraints are printed in the order
 * they were added, which is the order of the instances. Every clafer has a
 * scope line, sorted by name, whether its scope was given or is the default.
 * Source spans are not printed.
 * <p>
 * What applies is printed, not what is written: a clafer without its own group
 * card has its super clafer's, as chocosolver solves it, and {@code 0..*} if
 * no super clafer has one; a clafer without its own reference has its super
 * clafer's; an abstract clafer's multiplicity is {@code 0..*}. Inherited
 * children and constraints stay under the clafer that declares them.
 * <p>
 * Expressions are fully parenthesized, except for navigation ({@code .}),
 * which binds tightest. {@code sum} and {@code product} take a set of clafers
 * and add (multiply) the {@code dref} of each instance, so two instances with
 * the same value count twice; this is printed as in Clafer, e.g.,
 * {@code sum c0_Phone . c0_cost . dref}. An explicit dereference inside would
 * print as {@code . dref . dref}.
 */
public class AstPrinter {

    private static final String Indent = "  ";

    private AstPrinter() {
    }

    /**
     * Prints a model with its scopes, objectives and assertions.
     *
     * @param model the model
     * @param scope the scope
     * @param objectives the objectives
     * @param assertions the assertions
     * @return the model as text, one line per clafer, constraint, scope,
     * objective and assertion
     */
    public static String print(AstModel model, Scope scope, Objective[] objectives, Assertion[] assertions) {
        StringBuilder out = new StringBuilder();
        printScope(model, scope, out);
        out.append('\n');
        out.append(print(model));
        if (objectives.length > 0 || assertions.length > 0) {
            out.append('\n');
        }
        for (Objective objective : objectives) {
            out.append("<< ").append(objective.isMaximize() ? "max " : "min ")
                    .append(print(objective.getExpr())).append(" >>\n");
        }
        for (Assertion assertion : assertions) {
            out.append("assert [").append(print(assertion.getExpr())).append("]\n");
        }
        return out.toString();
    }

    /**
     * Prints the clafers and constraints of a model.
     *
     * @param model the model
     * @return the model as text
     */
    public static String print(AstModel model) {
        StringBuilder out = new StringBuilder();
        AstAbstractClafer top = model.getAbstractRoot();
        for (AstConstraint constraint : top.getConstraints()) {
            printConstraint(constraint, "", out);
        }
        for (AstAbstractClafer clafer : top.getAbstractChildren()) {
            if (clafer != model.getTypeRoot()) {
                printClafer(clafer, model, "", out);
            }
        }
        for (AstConcreteClafer clafer : top.getChildren()) {
            printClafer(clafer, model, "", out);
        }
        // Constraints directly under root, as opposed to the top level, are
        // possible in the Java API.
        if (model.getRoot().hasConstraints()) {
            out.append("root\n");
            for (AstConstraint constraint : model.getRoot().getConstraints()) {
                printConstraint(constraint, Indent, out);
            }
        }
        return out.toString();
    }

    private static void printScope(AstModel model, Scope scope, StringBuilder out) {
        // Every clafer's scope, the default included; root's is always 1.
        List<AstClafer> clafers = AstUtil.getClafers(model).stream()
                .filter(c -> c != model.getAbstractRoot() && c != model.getRoot() && c != model.getTypeRoot())
                .sorted(Comparator.comparing(AstClafer::getName))
                .collect(Collectors.toList());
        for (AstClafer clafer : clafers) {
            out.append("scope ").append(clafer.getName()).append(' ').append(scope.getScope(clafer)).append('\n');
        }
        out.append("int ").append(scope.getIntLow()).append("..").append(scope.getIntHigh()).append('\n');
        out.append("mul ").append(scope.getMulLow()).append("..").append(scope.getMulHigh()).append('\n');
        out.append("string length ").append(scope.getStringLength())
                .append(", chars ").append((int) scope.getCharLow()).append("..").append((int) scope.getCharHigh()).append('\n');
        out.append("decimal digits ").append(scope.getDecimalScale())
                .append(", scaled ").append(scope.getDecimalLow()).append("..").append(scope.getDecimalHigh()).append('\n');
    }

    private static void printClafer(AstClafer clafer, AstModel model, String indent, StringBuilder out) {
        out.append(indent);
        if (clafer instanceof AstAbstractClafer) {
            out.append("abstract ");
        }
        Card groupCard = AstUtil.getInheritedGroupCard(clafer);
        out.append(groupCard == null ? "0..*" : card(groupCard)).append(' ');
        out.append(clafer.getName());
        if (clafer.hasSuperClafer() && clafer.getSuperClafer() != model.getTypeRoot()) {
            out.append(" : ").append(clafer.getSuperClafer().getName());
        }
        AstRef ref = AstUtil.getInheritedRef(clafer);
        if (ref != null) {
            out.append(ref.isUnique() ? " -> " : " ->> ").append(ref.getTargetType().getName());
        }
        // An abstract clafer has no card of its own: its instances are those
        // of the clafers that extend it.
        out.append(' ').append(clafer instanceof AstConcreteClafer concrete ? card(concrete.getCard()) : "0..*");
        out.append('\n');
        String inner = indent + Indent;
        for (AstConstraint constraint : clafer.getConstraints()) {
            printConstraint(constraint, inner, out);
        }
        for (AstAbstractClafer child : clafer.getAbstractChildren()) {
            printClafer(child, model, inner, out);
        }
        for (AstConcreteClafer child : clafer.getChildren()) {
            printClafer(child, model, inner, out);
        }
    }

    private static void printConstraint(AstConstraint constraint, String indent, StringBuilder out) {
        String expr = print(constraint.getExpr());
        out.append(indent).append(constraint.isHard() ? "[" + expr + "]" : "(" + expr + ")").append('\n');
    }

    private static String card(Card card) {
        return card.getLow() + ".." + (card.hasHigh() ? Integer.toString(card.getHigh()) : "*");
    }

    /**
     * Prints an expression.
     *
     * @param expr the expression
     * @return the expression as text
     */
    public static String print(AstExpr expr) {
        return switch (expr) {
            case AstThis e -> "this";
            case AstGlobal e -> e.type().getName();
            case AstLocal e -> e.getName();
            case AstChildRelation e -> e.childType().getName();
            case AstParentRelation e -> "parent(" + e.parentRelation().getName() + ")";
            case AstRefRelation e -> "dref(" + e.ref().getSourceType().getName() + ")";
            case AstConstant e -> constant(e);
            case AstDecimalConstant e -> e.value().toPlainString();
            case AstStringConstant e -> string(e.value());
            case AstJoin e -> operand(e.left()) + " . " + step(e.right());
            case AstJoinParent e -> operand(e.children()) + " . parent";
            case AstJoinRef e -> operand(e.deref()) + " . dref";
            case AstNot e -> "!" + operand(e.expr());
            case AstMinus e -> "-" + operand(e.expr());
            case AstCard e -> "#" + operand(e.set());
            case AstSum e -> "sum " + operand(e.set()) + " . dref";
            case AstProduct e -> "product " + operand(e.set()) + " . dref";
            case AstMax e -> "max " + operand(e.set());
            case AstMin e -> "min " + operand(e.set());
            case AstLength e -> "length " + operand(e.string());
            case AstInverse e -> "~" + operand(e.relation());
            case AstTransitiveClosure e -> (e.reflexive() ? "*" : "^") + operand(e.relation());
            case AstCompare e -> binary(e.left(), e.op().getSyntax(), e.right());
            case AstSetTest e -> binary(e.left(), e.op().getSyntax(), e.right());
            case AstMembership e -> binary(e.member(), e.op().getSyntax(), e.set());
            case AstMod e -> binary(e.dividend(), "%", e.divisor());
            case AstUnion e -> binary(e.left(), "++", e.right());
            case AstDifference e -> binary(e.left(), "--", e.right());
            case AstIntersection e -> binary(e.left(), "**", e.right());
            case AstDomainRestriction e -> binary(e.domain(), "<:", e.relation());
            case AstRangeRestriction e -> binary(e.relation(), ":>", e.range());
            case AstArithm e -> nary(e.operands(), e.op() == AstArithm.Op.DecimalMul ? "*."
                    : e.op() == AstArithm.Op.DecimalDiv ? "/." : e.op().getSyntax());
            case AstBoolArithm e -> nary(e.operands(), e.op().getSyntax());
            case AstConcat e -> "concat(" + print(e.left()) + ", " + print(e.right()) + ")";
            case AstPrefix e -> "prefix(" + print(e.prefix()) + ", " + print(e.word()) + ")";
            case AstSuffix e -> "suffix(" + print(e.suffix()) + ", " + print(e.word()) + ")";
            case AstConnected e -> "connected(" + print(e.nodes()) + ", " + print(e.relation())
                    + (e.directed() ? ", directed)" : ", undirected)");
            case AstUpcast e -> "upcast(" + print(e.base()) + ", " + e.target() + ")";
            case AstDowncast e -> "downcast(" + print(e.base()) + ", " + e.target() + ")";
            case AstTernary e -> "if " + operand(e.antecedent()) + " then " + operand(e.consequent())
                    + " else " + operand(e.alternative());
            case AstIfThenElse e -> "if " + operand(e.antecedent()) + " then " + operand(e.consequent())
                    + " else " + operand(e.alternative());
            case AstQuantify e -> e.quantifier().getSyntax() + " "
                    + Arrays.stream(e.decls()).map(AstPrinter::decl).collect(Collectors.joining("; "))
                    + " | " + print(e.body());
        };
    }

    private static String decl(AstDecl decl) {
        return (decl.isDisjoint() ? "disj " : "")
                + Arrays.stream(decl.getLocals()).map(AstLocal::getName).collect(Collectors.joining(", "))
                + " : " + print(decl.getBody());
    }

    private static boolean isAtom(AstExpr expr) {
        return switch (expr) {
            case AstThis e -> true;
            case AstGlobal e -> true;
            case AstLocal e -> true;
            case AstChildRelation e -> true;
            case AstParentRelation e -> true;
            case AstRefRelation e -> true;
            case AstConstant e -> true;
            case AstDecimalConstant e -> true;
            case AstStringConstant e -> true;
            case AstConcat e -> true;
            case AstPrefix e -> true;
            case AstSuffix e -> true;
            case AstConnected e -> true;
            case AstUpcast e -> true;
            case AstDowncast e -> true;
            default -> false;
        };
    }

    private static boolean isNavigation(AstExpr expr) {
        return expr instanceof AstJoin || expr instanceof AstJoinParent || expr instanceof AstJoinRef;
    }

    /**
     * An operand of an operator: navigation binds tighter than any operator,
     * so only the other compound expressions are parenthesized.
     */
    private static String operand(AstExpr expr) {
        return isAtom(expr) || isNavigation(expr) ? print(expr) : "(" + print(expr) + ")";
    }

    /**
     * The right of a navigation, parenthesized unless it is an atom.
     */
    private static String step(AstExpr expr) {
        return isAtom(expr) ? print(expr) : "(" + print(expr) + ")";
    }

    private static String binary(AstExpr left, String op, AstExpr right) {
        return operand(left) + " " + op + " " + operand(right);
    }

    private static String nary(AstExpr[] operands, String op) {
        return Arrays.stream(operands).map(AstPrinter::operand).collect(Collectors.joining(" " + op + " "));
    }

    private static String constant(AstConstant constant) {
        int[][] value = constant.value();
        if (constant.type().isInt() && value.length == 1 && value[0].length == 1) {
            return Integer.toString(value[0][0]);
        }
        String tuples = Arrays.stream(value).map(Arrays::toString).collect(Collectors.joining(", "));
        return "{" + tuples + "} :: " + constant.type();
    }

    private static String string(String value) {
        StringBuilder out = new StringBuilder("\"");
        for (char c : value.toCharArray()) {
            switch (c) {
                case '"' -> out.append("\\\"");
                case '\\' -> out.append("\\\\");
                case '\n' -> out.append("\\n");
                case '\t' -> out.append("\\t");
                default -> {
                    if (c < ' ' || c > '~') {
                        out.append(String.format("\\u%04x", (int) c));
                    } else {
                        out.append(c);
                    }
                }
            }
        }
        return out.append('"').toString();
    }
}
