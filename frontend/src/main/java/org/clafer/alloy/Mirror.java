package org.clafer.alloy;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.clafer.frontend.Ir;
import org.clafer.frontend.SourceSpan;

/**
 * The compiler's intermediate representation ({@code Intclafer.hs}) as the
 * Alloy generator reads it: the clafers, and the expressions typed and shaped
 * as the compiler's {@code PExp}, so that {@link AlloyLtl} is a line-by-line
 * port. {@link AlloyInput} builds it from the explicit core IR.
 */
final class Mirror {

    private Mirror() {
    }

    // ------------------------------------------------------------------
    // Types
    // ------------------------------------------------------------------
    sealed interface IType permits TClafer, TMap, TPrim {
    }

    /**
     * A clafer type: the clafer and its supers, most specific first.
     */
    record TClafer(List<String> hi) implements IType {

        TClafer {
            hi = List.copyOf(hi);
        }
    }

    /**
     * The type of {@code dref}: from the clafer that refers to its target.
     */
    record TMap(IType so, IType ta) implements IType {
    }

    enum TPrim implements IType {
        TBoolean, TInteger, TString, TDecimal, TDouble, TReal
    }

    // ------------------------------------------------------------------
    // Expressions
    // ------------------------------------------------------------------
    /**
     * A typed expression, the compiler's {@code PExp}.
     *
     * @param type its type, or null where the compiler's is not read
     * @param pid the identity of the expression for the traces
     * @param inPos its position, for the names of the assertions
     */
    record PExp(IType type, Object pid, SourceSpan inPos, IExp exp) {

        PExp withExp(IExp e) {
            return new PExp(type, pid, inPos, e);
        }

        PExp withType(IType t) {
            return new PExp(t, pid, inPos, exp);
        }
    }

    sealed interface IExp permits IClaferId, IFunExp, IDeclPExp, IInt {
    }

    /**
     * A name: a clafer, {@code this}, {@code parent}, {@code dref},
     * {@code root}, a primitive type, or a local of a quantifier.
     *
     * @param isTop whether it names a top-level clafer, or {@code this}
     * @param local whether it is a local of a quantifier ({@code LocalBind})
     */
    record IClaferId(String sident, boolean isTop, boolean local) implements IExp {

        IClaferId withSident(String s) {
            return new IClaferId(s, isTop, local);
        }
    }

    record IFunExp(String op, List<PExp> exps) implements IExp {

        IFunExp {
            exps = List.copyOf(exps);
        }
    }

    /**
     * A quantifier; without declarations, a quantifier over a set
     * ({@code some x}).
     */
    record IDeclPExp(Ir.Quantifier quant, List<IDecl> oDecls, PExp bpexp) implements IExp {

        IDeclPExp {
            oDecls = List.copyOf(oDecls);
        }
    }

    record IDecl(boolean isDisj, List<String> decls, PExp body) {

        IDecl {
            decls = List.copyOf(decls);
        }
    }

    record IInt(long value) implements IExp {
    }

    // ------------------------------------------------------------------
    // Clafers
    // ------------------------------------------------------------------
    /**
     * An interval, the high bound -1 for unbounded.
     */
    record Interval(long low, long high) {
    }

    /**
     * A reference, the compiler's {@code IReference}.
     *
     * @param target the target's name: a clafer, or a primitive type
     * @param ref the target, a set expression, or null for a name
     */
    record IReference(boolean isSet, String target, PExp ref) {
    }

    sealed interface IElement permits IClafer, IConstraint {
    }

    /**
     * A constraint ({@code isHard}) or an assertion, the compiler's
     * {@code IEConstraint'}.
     *
     * @param origin the construct it was lowered from, if any
     */
    record IConstraint(boolean isHard, PExp pexp, Optional<Ir.Origin> origin) implements IElement {
    }

    /**
     * A clafer, the compiler's {@code IClafer}. The root is a clafer too,
     * with the top-level declarations as its elements.
     */
    static final class IClafer implements IElement {

        final String uid;
        final boolean isAbstract;
        final String superUid;
        final IReference reference;
        final Interval card;
        final Interval gcard;
        final String parentUid;
        final List<IElement> elements = new ArrayList<>();

        IClafer(String uid, boolean isAbstract, String superUid, IReference reference, Interval card,
                Interval gcard, String parentUid) {
            this.uid = uid;
            this.isAbstract = isAbstract;
            this.superUid = superUid;
            this.reference = reference;
            this.card = card;
            this.gcard = gcard;
            this.parentUid = parentUid;
        }

        boolean isTopLevel() {
            return "root".equals(parentUid);
        }

        List<IClafer> subclafers() {
            List<IClafer> out = new ArrayList<>();
            for (IElement e : elements) {
                if (e instanceof IClafer c) {
                    out.add(c);
                }
            }
            return out;
        }

        @Override
        public String toString() {
            return uid;
        }
    }
}
