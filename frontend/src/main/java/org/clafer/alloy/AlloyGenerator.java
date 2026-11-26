package org.clafer.alloy;

import java.util.Map;
import org.clafer.frontend.Ir;

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
}
