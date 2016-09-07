package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import static org.clafer.domain.Domains.boundDomain;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSortSets;
import org.clafer.ir.Irs;
import static org.clafer.ir.Irs.Zero;
import static org.clafer.ir.Irs.add;

/**
 *
 * @author jimmy
 */
class SortSetsDeducer implements BoolDeducer<IrSortSets> {

    private boolean isZero(Domain domain) {
        return domain.isConstant() && domain.getLowBound() == 0;
    }

    @Override
    public void deduce(IrSortSets ir, Deduction deduction) {
        IrSetExpr[] sets = ir.getSets();
        IrIntExpr[] bounds = ir.getBounds();
        IrIntExpr[] boundary = new IrIntExpr[sets.length + 1];
        boundary[0] = Zero;
        for (int i = 0; i < sets.length; i++) {
            if (isZero(boundary[i].getDomain())) {
                boundary[i + 1] = Irs.card(sets[i]);
            } else if (isZero(sets[i].getCard())) {
                boundary[i + 1] = boundary[i];
            } else {
                boundary[i + 1] = bounds[i];
                deduction.equal(add(boundary[i], Irs.card(sets[i])), boundary[i + 1]);
            }
            if (!boundary[i + 1].equals(bounds[i])) {
                deduction.equal(boundary[i + 1], bounds[i]);
            }
            if (boundary[i].getDomain().isConstant()) {
                int constant = boundary[i].getDomain().getLowBound();
                if (sets[i].getCard().getLowBound() > 0 && !sets[i].getKer().contains(constant)) {
                    deduction.kerContains(sets[i], constant);
                }
            }
            if (boundary[i].getLowBound() < boundary[i + 1].getHighBound()) {
                Domain domain = boundDomain(boundary[i].getLowBound(), boundary[i + 1].getHighBound() - 1);
                deduction.envSubsetOf(sets[i], domain);
            }
            if (boundary[i].getHighBound() < boundary[i + 1].getLowBound()) {
                Domain domain = boundDomain(boundary[i].getHighBound(), boundary[i + 1].getLowBound() - 1);
                deduction.kerContains(sets[i], domain);
            }
        }
    }
}
