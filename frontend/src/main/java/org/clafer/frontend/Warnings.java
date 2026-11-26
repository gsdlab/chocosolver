package org.clafer.frontend;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import org.clafer.frontend.Ir.BinaryOp;

/**
 * The warnings that the compiler prints after its statistics, in its order,
 * each at its position in its file:
 * <ul>
 * <li>an expression compared with, subtracted from or intersected with
 * itself ({@code redundantExps}): the resolved operands print the same;</li>
 * <li>a navigation from {@code noclafer} ({@code noClaferNavs}), as
 * written;</li>
 * <li>a clafer name with {@code __}, unless the names are the UIDs
 * ({@code showDoubleUnderscoreNames}).</li>
 * </ul>
 */
final class Warnings {

    private Warnings() {
    }

    private static final Map<BinaryOp, String> Meanings = Map.of(BinaryOp.EQ, "always true",
            BinaryOp.NE, "always false", BinaryOp.DIFF, "always empty",
            BinaryOp.INTER, "the same as either subexpression");

    /**
     * @param result a model without errors
     * @param source its parse tree
     * @param skipResolver {@code -f}: the names are the UIDs
     * @return the lines, each ending with a newline
     */
    static String of(ToIr.Result result, Syntax.Model source, boolean skipResolver) {
        ModelFiles files = result.files();
        StringBuilder out = new StringBuilder();
        forEachExp(result.model(), e -> {
            if (e instanceof Ir.Binary b && Meanings.containsKey(b.op()) && b.operands().size() == 2
                    && ExplicitPrinter.print(b.operands().get(0)).equals(ExplicitPrinter.print(b.operands().get(1)))) {
                out.append("Warning: ").append(at(files, b.span())).append(": '").append(b.op().syntax())
                        .append("' is redundant because the two subexpressions are always equivalent, so it is ")
                        .append(Meanings.get(b.op())).append(".\n");
            }
        });
        List<SourceSpan> navs = new ArrayList<>();
        Resolver.forEachExp(source, e -> {
            if (e instanceof Syntax.Binary b && b.op() == Syntax.BinaryOp.Join
                    && b.left() instanceof Syntax.ClaferId id && id.name().path().size() == 1
                    && id.name().path().get(0).text().equals("noclafer")) {
                navs.add(b.span());
            }
        });
        navs.sort(Comparator.comparingInt(SourceSpan::line).thenComparingInt(SourceSpan::column));
        for (SourceSpan s : navs) {
            out.append("Warning: ").append(at(files, s))
                    .append(": a navigation from 'noclafer' is redundant, since it is always empty.\n");
        }
        if (!skipResolver) {
            for (Symbols.ClaferSymbol c : result.names().clafers()) {
                if (c.name().contains("__")) {
                    out.append("Warning: ").append(at(files, c.declaration().span())).append(": the name '")
                            .append(c.name()).append("' contains '__', which the compiler uses in the unique names"
                            + " of clafers (e.g., x__size for the size in x).\n");
                }
            }
        }
        return out.toString();
    }

    private static String at(ModelFiles files, SourceSpan span) {
        ModelFiles.Position p = files.position(span.line());
        return p.path() + ":" + p.line() + ":" + span.column();
    }

    /**
     * Each expression of the model, in pre-order.
     */
    private static void forEachExp(Ir.Model model, Consumer<Ir.Exp> f) {
        forEachExp(model.elements(), f);
        model.objectives().forEach(o -> walk(o.exp(), f));
        model.assertions().forEach(a -> walk(a.exp(), f));
    }

    private static void forEachExp(List<Ir.Element> elements, Consumer<Ir.Exp> f) {
        for (Ir.Element e : elements) {
            switch (e) {
                case Ir.Constraint c -> walk(c.exp(), f);
                case Ir.ClaferDecl c -> {
                    c.initializer().ifPresent(i -> walk(i.value(), f));
                    forEachExp(c.elements(), f);
                }
                case Ir.RootBlock r -> r.constraints().forEach(c -> walk(c.exp(), f));
                case Ir.EnumDecl en -> {
                }
            }
        }
    }

    private static void walk(Ir.Exp e, Consumer<Ir.Exp> f) {
        f.accept(e);
        ToCore.mapChildren(e, c -> {
            walk(c, f);
            return c;
        });
    }
}
