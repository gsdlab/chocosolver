package org.clafer.frontend;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.clafer.frontend.Ir.*;

/**
 * Prints the {@link Ir explicit IR} exactly as the Clafer compiler prints the
 * rich and the core explicit model ({@code Generator/Explicit.hs}), with or
 * without the provenance comments, so that every comparison of two models is a
 * comparison of text. A tree that {@link ExplicitParser} read from the
 * compiler's output prints as that output.
 * <p>
 * The declarations and constraints are printed where they were written, the
 * objectives and assertions at the end. Every operand that is not an atom or a
 * navigation is parenthesized. Literals are printed as they were written.
 */
public final class ExplicitPrinter {

    private static final String Indent = "  ";

    private ExplicitPrinter() {
    }

    /**
     * @param model the model
     * @return the model, with its provenance comments
     */
    public static String print(Model model) {
        return print(model, true);
    }

    /**
     * @param model the model
     * @param withOrigins whether to print the provenance comments
     * @return the model as text
     */
    public static String print(Model model, boolean withOrigins) {
        StringBuilder out = new StringBuilder();
        Options options = model.options();
        out.append("//# OPTIONS --explicit");
        if (options.core()) {
            out.append(" --core");
        }
        options.traceLength().ifPresent(tl -> out.append(" --tl=").append(tl));
        out.append('\n');
        for (Element element : model.elements()) {
            printElement(element, "", withOrigins, out);
        }
        if (!model.objectives().isEmpty() || !model.assertions().isEmpty()) {
            out.append('\n');
        }
        for (Objective objective : model.objectives()) {
            out.append("<< ").append(objective.maximize() ? "max " : "min ").append(print(objective.exp())).append(" >>\n");
        }
        for (Assertion assertion : model.assertions()) {
            out.append("assert [").append(print(assertion.exp())).append(']')
                    .append(origin(assertion.origin(), withOrigins)).append('\n');
        }
        return out.toString();
    }

    private static String origin(Optional<Origin> origin, boolean withOrigins) {
        if (!withOrigins || origin.isEmpty()) {
            return "";
        }
        Origin o = origin.get();
        return " // " + o.construct() + o.position().map(p -> ", " + p).orElse("");
    }

    private static void printElement(Element element, String indent, boolean withOrigins, StringBuilder out) {
        switch (element) {
            case Constraint c -> out.append(indent).append('[').append(print(c.exp())).append(']')
                    .append(origin(c.origin(), withOrigins)).append('\n');
            case EnumDecl e -> out.append(indent).append("enum ").append(e.name().text()).append(" = ")
                    .append(e.literals().stream().map(Name::text).collect(Collectors.joining(" | "))).append('\n');
            case RootBlock r -> {
                out.append(indent).append("root {\n");
                for (Constraint c : r.constraints()) {
                    printElement(c, indent + Indent, withOrigins, out);
                }
                out.append(indent).append("}\n");
            }
            case ClaferDecl c -> printClafer(c, indent, withOrigins, out);
        }
    }

    private static void printClafer(ClaferDecl c, String indent, boolean withOrigins, StringBuilder out) {
        out.append(indent);
        if (c.isAbstract()) {
            out.append("abstract ");
        }
        for (Modifier m : c.modifiers()) {
            out.append(m.kind().keyword()).append(' ');
        }
        out.append(c.groupCard()).append(' ').append(c.name().text());
        c.superName().ifPresent(s -> out.append(" : ").append(s.text()));
        c.ref().ifPresent(r -> out.append(r.isSet() ? " -> " : " ->> ").append(r.target().text()));
        out.append(' ').append(c.abstractMultiplicity().map(AbstractMultiplicity::card).orElse(c.card()));
        c.initializer().ifPresent(i -> out.append(' ').append(i.kind().syntax()).append(' ').append(print(i.value())));
        if (c.elements().isEmpty()) {
            out.append(" {}").append(origin(c.origin(), withOrigins)).append('\n');
            return;
        }
        out.append(" {").append(origin(c.origin(), withOrigins)).append('\n');
        for (Element e : c.elements()) {
            printElement(e, indent + Indent, withOrigins, out);
        }
        out.append(indent).append("}\n");
    }

    /**
     * @param exp an expression
     * @return the expression as text, its operands parenthesized unless they
     * are atoms or navigations
     */
    public static String print(Exp exp) {
        return switch (exp) {
            case This e -> "this";
            case NameExp e -> e.name();
            case IntLit e -> Long.toString(e.value());
            case DecimalLit e -> e.text();
            case DoubleLit e -> e.text();
            case StringLit e -> e.text();
            case NoClafer e -> "noclafer";
            case Join e -> operand(e.left()) + "." + step(e.right());
            case Dref e -> operand(e.left()) + ".dref";
            case ParentOf e -> operand(e.left()) + ".parent";
            case Unary e -> switch (e.op()) {
                // -1 is a constant, so the negation of a number is -(1)
                case MINUS -> "-" + (isConstant(e.arg()) ? "(" + print(e.arg()) + ")" : operand(e.arg()));
                case SUM, PRODUCT -> e.op().syntax() + operand(e.arg()) + ".dref";
                default -> e.op().syntax() + operand(e.arg());
            };
            case Binary e -> e.operands().stream().map(ExplicitPrinter::operand)
                    .collect(Collectors.joining(" " + e.op().syntax() + " "));
            case IfThenElse e -> "if " + operand(e.condition()) + " then " + operand(e.then())
                    + " else " + operand(e.otherwise());
            case Quantified e -> e.quantifier().keyword() + " "
                    + e.decls().stream().map(ExplicitPrinter::decl).collect(Collectors.joining(", "))
                    + " | " + print(e.body());
            case AllOf e -> "all " + operand(e.set());
            case Call e -> e.function() + "(" + e.args().stream().map(ExplicitPrinter::print)
                    .collect(Collectors.joining(", ")) + ")";
            case RelationOf e -> (e.isDref() ? "dref(" : "parent(") + e.clafer().text() + ")";
            case Cast e -> (e.up() ? "upcast(" : "downcast(") + print(e.base()) + ", " + type(e.type()) + ")";
            case Connected e -> "connected(" + print(e.nodes()) + ", " + print(e.relation())
                    + (e.directed() ? ", directed)" : ", undirected)");
            case ConstantSet e -> "{" + e.rows().stream().map(List::toString).collect(Collectors.joining(", "))
                    + "} :: " + type(e.type());
            case Temporal e -> e.op().arity() == 1
                    ? e.op().keyword() + " " + operand(e.args().get(0))
                    : operand(e.args().get(0)) + " " + e.op().keyword() + " " + operand(e.args().get(1));
            case Anchor e -> (e.initially() ? "initially " : "finally ") + operand(e.body());
            case Pattern e -> pattern(e);
            case Transition e -> operand(e.from())
                    + e.guard().map(g -> " -[" + operand(g) + (e.sync() ? "]->> " : "]-> "))
                            .orElse(e.sync() ? " -->> " : " --> ")
                    + (e.to() instanceof Transition ? print(e.to()) : operand(e.to()));
        };
    }

    private static String pattern(Pattern p) {
        List<Exp> args = p.args();
        int own = p.kind().arity();
        String pattern = own == 2
                ? operand(args.get(0)) + " must " + p.kind().keyword() + " " + operand(args.get(1))
                : p.kind().keyword() + " " + operand(args.get(0));
        List<Exp> scope = args.subList(own, args.size());
        return pattern + switch (p.scope()) {
            case GLOBALLY -> "";
            case BEFORE -> " before " + operand(scope.get(0));
            case AFTER -> " after " + operand(scope.get(0));
            case BETWEEN -> " between " + operand(scope.get(0)) + " and " + operand(scope.get(1));
            case AFTER_UNTIL -> " after " + operand(scope.get(0)) + " until " + operand(scope.get(1));
        };
    }

    private static String decl(Decl decl) {
        return (decl.disjoint() ? "disj " : "")
                + decl.locals().stream().map(Name::text).collect(Collectors.joining("; "))
                + " : " + print(decl.set());
    }

    private static String type(List<Name> type) {
        return type.stream().map(Name::text).collect(Collectors.joining(" * "));
    }

    private static boolean isConstant(Exp exp) {
        return exp instanceof IntLit || exp instanceof DecimalLit || exp instanceof NoClafer
                || exp instanceof ConstantSet;
    }

    /**
     * Atoms: names, literals and calls, which need no parentheses anywhere.
     */
    private static boolean isAtom(Exp exp) {
        return switch (exp) {
            case This e -> true;
            case NameExp e -> true;
            case IntLit e -> true;
            case DecimalLit e -> true;
            case DoubleLit e -> true;
            case StringLit e -> true;
            case NoClafer e -> true;
            case Call e -> true;
            case RelationOf e -> true;
            case Cast e -> true;
            case Connected e -> true;
            case ConstantSet e -> true;
            default -> false;
        };
    }

    private static boolean isNavigation(Exp exp) {
        return exp instanceof Join || exp instanceof Dref || exp instanceof ParentOf;
    }

    /**
     * An operand of an operator: navigation binds tighter than any operator.
     */
    private static String operand(Exp exp) {
        return isAtom(exp) || isNavigation(exp) ? print(exp) : "(" + print(exp) + ")";
    }

    /**
     * The right of a navigation, parenthesized unless it is an atom.
     */
    private static String step(Exp exp) {
        return isAtom(exp) ? print(exp) : "(" + print(exp) + ")";
    }
}
