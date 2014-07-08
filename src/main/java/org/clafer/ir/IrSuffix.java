package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSuffix extends IrAbstractBool {

    private final IrStringExpr suffix;
    private final IrStringExpr word;

    IrSuffix(IrStringExpr suffix, IrStringExpr word, BoolDomain domain) {
        super(domain);
        this.suffix = Check.notNull(suffix);
        this.word = Check.notNull(word);
    }

    public IrStringExpr getSuffix() {
        return suffix;
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
        if (obj instanceof IrSuffix) {
            IrSuffix other = (IrSuffix) obj;
            return suffix.equals(other.suffix) && word.equals(other.word);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return suffix.hashCode() ^ word.hashCode();
    }

    @Override
    public String toString() {
        return suffix + " suffix " + word;
    }
}
