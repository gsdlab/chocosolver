package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrElement;
import org.clafer.ir.IrIntExpr;

/**
 *
 * @author jimmy
 */
class ElementDeducer implements IntDeducer<IrElement> {

    @Override
    public void deduce(IrElement ir, Domain domain, Deduction deduction) {
        IrIntExpr index = ir.getIndex();
        Domain dom = index.getDomain().retainAll(i
                -> domain.intersects(ir.getArray().getDomains()[i]));
        deduction.within(index, dom);
    }
}
