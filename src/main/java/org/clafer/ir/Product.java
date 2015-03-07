package org.clafer.ir;

import org.clafer.collection.Monoid;
import static org.clafer.ir.Irs.One;
import static org.clafer.ir.Irs.mul;

/**
 *
 * @author jimmy
 */
public class Product implements Monoid<IrIntExpr> {

    public static final Product Singleton = new Product();

    private Product() {
    }

    @Override
    public IrIntExpr empty() {
        return One;
    }

    @Override
    public IrIntExpr append(IrIntExpr a, IrIntExpr b) {
        return mul(a, b);
    }

    @Override
    public IrIntExpr concat(IrIntExpr... ts) {
        return mul(ts);
    }
}
