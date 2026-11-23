package org.clafer.ir;

import org.clafer.collection.Monoid;
import static org.clafer.ir.Irs.Zero;
import static org.clafer.ir.Irs.add;

/**
 *
 * @author jimmy
 */
public class Sum implements Monoid<IrIntExpr> {

    public static final Sum Singleton = new Sum();

    private Sum() {
    }

    @Override
    public IrIntExpr empty() {
        return Zero;
    }

    @Override
    public IrIntExpr append(IrIntExpr a, IrIntExpr b) {
        return add(a, b);
    }

    @Override
    public IrIntExpr concat(IrIntExpr... ts) {
        return add(ts);
    }
}
