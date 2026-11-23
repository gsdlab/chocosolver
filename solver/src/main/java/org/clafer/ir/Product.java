package org.clafer.ir;

import org.clafer.collection.Monoid;
import org.clafer.common.Check;
import org.clafer.domain.Domain;
import static org.clafer.ir.Irs.One;
import static org.clafer.ir.Irs.mul;

/**
 *
 * @author jimmy
 */
public class Product implements Monoid<IrIntExpr> {

    private final Domain intRange;

    public Product(Domain intRange) {
        this.intRange = Check.notNull(intRange);
    }

    @Override
    public IrIntExpr empty() {
        return One;
    }

    @Override
    public IrIntExpr append(IrIntExpr a, IrIntExpr b) {
        return mul(a, b, intRange);
    }

    @Override
    public IrIntExpr concat(IrIntExpr... ts) {
        return mul(ts, intRange);
    }
}
