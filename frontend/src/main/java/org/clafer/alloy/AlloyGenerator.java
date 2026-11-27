package org.clafer.alloy;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.clafer.alloy.Concat.ExactCard;
import org.clafer.alloy.Concat.IrPExp;
import org.clafer.alloy.Concat.LowerCard;
import org.clafer.alloy.Concat.UpperCard;
import org.clafer.alloy.Mirror.IClafer;
import org.clafer.alloy.Mirror.IConstraint;
import org.clafer.alloy.Mirror.IDeclPExp;
import org.clafer.alloy.Mirror.IElement;
import org.clafer.alloy.Mirror.IFunExp;
import org.clafer.alloy.Mirror.PExp;
import org.clafer.frontend.Ir;
import org.clafer.frontend.SourceSpan;

/**
 * The Alloy 6 code of a temporal model, as {@code clafer -m alloy} writes it
 * ({@code P.als}): the port of the compiler's {@code Generator/AlloyLtl.hs}
 * ({@link AlloyLtl}) on the input the compiler builds for it
 * ({@link AlloyInput}). Static models are solved with Choco, and have no
 * Alloy code here.
 */
public final class AlloyGenerator {

    private AlloyGenerator() {
    }

    /**
     * A model that the port does not translate.
     */
    public static final class UnsupportedModel extends Exception {

        UnsupportedModel(String message) {
            super(message);
        }
    }

    /**
     * @param rich the rich explicit IR of a temporal model, as {@code ToIr}
     * gives it
     * @param scopes its scopes, by UID, in the order of the UIDs
     * @param traceLength the trace length ({@code --tl})
     * @param keepUnused {@code -k}: keep the abstract clafers that nothing
     * needs
     * @param validate {@code -v}: without the {@code run} command
     * @return the Alloy code, without the statistics comment
     */
    public static String generate(Ir.Model rich, Map<String, Long> scopes, int traceLength, boolean keepUnused,
            boolean validate)
            throws UnsupportedModel {
        try {
            AlloyInput.Input in = AlloyInput.input(rich, scopes, traceLength, keepUnused);
            return AlloyLtl.genAlloyLtlModule(in.statics(), in.staticRefs(), in.literals(), validate, in.root(),
                    in.scopes(), in.traceLength()).code();
        } catch (AlloyInput.Unsupported e) {
            throw new UnsupportedModel(e.getMessage());
        }
    }

    /**
     * What the code of a region of the Alloy code is: a constraint of the
     * model, or a cardinality of a clafer (not a group cardinality).
     */
    public enum Kind {
        CONSTRAINT, LOWER_CARD, UPPER_CARD, EXACT_CARD
    }

    /**
     * A region of the Alloy code (1-based lines and columns, as
     * {@code mapLineCol} gives them) and what it is the code of.
     *
     * @param clafer for a cardinality, the clafer's Alloy name
     * @param span for a constraint, its span in the model's text
     */
    public record Mapped(long line, long column, long endLine, long endColumn, Kind kind, String clafer,
            SourceSpan span) {
    }

    /**
     * An assertion of the model, with its name in the Alloy code.
     */
    public record Assertion(String name, SourceSpan span) {
    }

    /**
     * The Alloy code of a model for solving it, as claferIG has the compiler
     * write it, and what claferIG reads from the compiler with it.
     *
     * @param code the Alloy code, with the {@code run} command
     * @param mapping the regions of the code that are constraints and
     * cardinalities, for the unsat cores
     * @param strings each string literal by its integer
     * @param alloyNames the Alloy name of each clafer whose UID is not one,
     * by UID
     * @param assertions the assertions, in the order of the model
     * @param maxInt the largest integer of the model, for the bitwidth
     */
    public record Translation(String code, List<Mapped> mapping, Map<Integer, String> strings,
            Map<String, String> alloyNames, List<Assertion> assertions, long maxInt) {
    }

    /**
     * The Alloy code of a temporal model for solving it, as claferIG has the
     * compiler write it: with {@code -k}, so that no abstract clafer is
     * removed.
     *
     * @param rich the rich explicit IR of a temporal model
     * @param scopes its scopes, by UID, in the order of the UIDs
     * @param traceLength the trace length ({@code --tl})
     */
    public static Translation translate(Ir.Model rich, Map<String, Long> scopes, int traceLength)
            throws UnsupportedModel {
        AlloyInput.Input in;
        AlloyLtl.Result result;
        try {
            in = AlloyInput.input(rich, scopes, traceLength, true);
            result = AlloyLtl.genAlloyLtlModule(in.statics(), in.staticRefs(), in.literals(), false, in.root(),
                    in.scopes(), in.traceLength());
        } catch (AlloyInput.Unsupported e) {
            throw new UnsupportedModel(e.getMessage());
        }
        Map<Object, SourceSpan> spans = new IdentityHashMap<>();
        List<Assertion> assertions = new ArrayList<>();
        positions(in.root(), spans, assertions);
        assertions.sort(Comparator.comparing(Assertion::span,
                Comparator.comparingInt(SourceSpan::line).thenComparingInt(SourceSpan::column)));
        List<Mapped> mapping = new ArrayList<>();
        for (Map.Entry<AlloyLtl.Span, Concat.IrTrace> e : result.mapping()) {
            AlloyLtl.Span at = e.getKey();
            Mapped m = switch (e.getValue()) {
                case IrPExp p -> spans.containsKey(p.pUid())
                        ? new Mapped(at.startLine(), at.startColumn(), at.endLine(), at.endColumn(), Kind.CONSTRAINT,
                                null, spans.get(p.pUid())) : null;
                case LowerCard c when !c.isGroup() -> new Mapped(at.startLine(), at.startColumn(), at.endLine(),
                        at.endColumn(), Kind.LOWER_CARD, c.pUid(), null);
                case UpperCard c when !c.isGroup() -> new Mapped(at.startLine(), at.startColumn(), at.endLine(),
                        at.endColumn(), Kind.UPPER_CARD, c.pUid(), null);
                case ExactCard c when !c.isGroup() -> new Mapped(at.startLine(), at.startColumn(), at.endLine(),
                        at.endColumn(), Kind.EXACT_CARD, c.pUid(), null);
                default -> null;
            };
            if (m != null) {
                mapping.add(m);
            }
        }
        Map<Integer, String> strings = new LinkedHashMap<>();
        in.strings().forEach((text, i) -> strings.put(i, text));
        return new Translation(result.code(), List.copyOf(mapping), strings, Map.copyOf(in.names()),
                List.copyOf(assertions), AlloyInput.maxInModel(rich));
    }

    /**
     * The spans of the expressions of the constraints, by their ids, and the
     * assertions ({@code parseConstraints}, {@code parseAssertions}).
     */
    private static void positions(IClafer c, Map<Object, SourceSpan> spans, List<Assertion> assertions) {
        for (IElement e : c.elements) {
            switch (e) {
                case IClafer sub -> positions(sub, spans, assertions);
                case IConstraint k -> {
                    subexpressions(k.pexp(), spans);
                    if (!k.isHard()) {
                        assertions.add(new Assertion(AlloyLtl.assertionName(k.pexp()), k.pexp().inPos()));
                    }
                }
                default -> {
                }
            }
        }
    }

    private static void subexpressions(PExp p, Map<Object, SourceSpan> spans) {
        spans.putIfAbsent(p.pid(), p.inPos());
        switch (p.exp()) {
            case IDeclPExp d -> {
                d.oDecls().forEach(x -> subexpressions(x.body(), spans));
                subexpressions(d.bpexp(), spans);
            }
            case IFunExp f -> f.exps().forEach(x -> subexpressions(x, spans));
            default -> {
            }
        }
    }
}
