package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrJoinRef extends IrAbstractSet implements IrSetExpr {

    private final IrSetExpr take;
    private final IrIntExpr[] refs;

    IrJoinRef(IrSetExpr take, IrIntExpr[] refs, IrDomain env, IrDomain ker, IrDomain card) {
        super(env, ker, card);
        this.take = Check.notNull(take);
        this.refs = Check.noNullsNotEmpty(refs);
    }

    public IrSetExpr getTake() {
        return take;
    }

    public IrIntExpr[] getRefs() {
        return refs;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrJoinRef) {
            IrJoinRef other = (IrJoinRef) obj;
            return take.equals(other.take) && Arrays.equals(refs, other.refs) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return take.hashCode() ^ Arrays.hashCode(refs);
    }

    @Override
    public String toString() {
        return take + " . " + Arrays.toString(refs);
    }
}
