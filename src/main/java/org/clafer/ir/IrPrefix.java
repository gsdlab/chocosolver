package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrPrefix extends IrAbstractBool {

    private final IrStringExpr prefix;
    private final IrStringExpr word;

    IrPrefix(IrStringExpr prefix, IrStringExpr word, BoolDomain domain) {
        super(domain);
        this.prefix = Check.notNull(prefix);
        this.word = Check.notNull(word);
    }

    public IrStringExpr getPrefix() {
        return prefix;
    }

    public IrStringExpr getWord() {
        return word;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrNot(this, getDomain().invert());
    }

    @Override
    public boolean isNegative() {
        return false;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrPrefix) {
            IrPrefix other = (IrPrefix) obj;
            return prefix.equals(other.prefix) && word.equals(other.word);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return prefix.hashCode() ^ word.hashCode();
    }

    @Override
    public String toString() {
        return prefix + " prefix " + word;
    }
}
