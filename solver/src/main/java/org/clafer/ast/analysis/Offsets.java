package org.clafer.ast.analysis;

import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class Offsets {

    private final AstAbstractClafer sup;
    private final Map<AstClafer, Integer> offsets;
    private final AstClafer[] reverseOffsets;

    Offsets(AstAbstractClafer sup, Map<AstClafer, Integer> offsets, AstClafer[] reverseOffsets) {
        this.sup = Check.notNull(sup);
        this.offsets = Check.notNull(offsets);
        this.reverseOffsets = Check.noNulls(reverseOffsets);
    }

    public int getOffset(AstClafer sub) {
        Integer offset = offsets.get(sub);
        if (offset == null) {
            throw new AnalysisException(sub + " is not a direct sub clafer of " + sup);
        }
        return offset.intValue();
    }

    public AstClafer getClafer(int offset) {
        return reverseOffsets[offset];
    }

    @Override
    public String toString() {
        return sup + "=>" + offsets;
    }
}
