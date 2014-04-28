package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrLength extends IrAbstractInt {

    private final IrStringExpr string;

    IrLength(IrStringExpr string, Domain domain) {
        super(domain);
        this.string = Check.notNull(string);
    }

    public IrStringExpr getString() {
        return string;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrLength) {
            IrLength other = (IrLength) obj;
            return string.equals(other.string) ;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 41 * string.hashCode();
    }

    @Override
    public String toString() {
        return "|" + string + "|";
    }
}
