package org.clafer.ir.analysis.deduction;

import java.util.stream.IntStream;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import org.clafer.ir.IrBoolChannel;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrSetExpr;

/**
 *
 * @author jimmy
 */
class BoolChannelDeducer implements BoolDeducer<IrBoolChannel> {

    @Override
    public void deduce(IrBoolChannel ir, Deduction deduction) {
        IrBoolExpr[] bools = ir.getBools();
        IrSetExpr set = ir.getSet();
        Domain ker = set.getKer();
        Domain env = set.getEnv();
        for (int i = 0; i < bools.length; i++) {
            if (!env.contains(i)) {
                deduction.equal(bools[i], 0);
            } else if (ker.contains(i)) {
                deduction.equal(bools[i], 1);
            }
        }
        Domain trues = Domains.enumDomain(IntStream.range(0, bools.length).filter(i -> bools[i].getDomain().isTrue()));
        deduction.kerContains(set, trues);
        Domain notFalses = env.removeAll(i -> i < 0 || i >= bools.length || bools[i].getDomain().isFalse());
        deduction.envSubsetOf(set, notFalses);
    }
}
