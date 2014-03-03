package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrStringVar extends IrAbstractString implements IrVar {

    private final String name;
    private final IrIntVar[] chars;
    private final IrIntVar length;

    protected IrStringVar(String name, IrIntVar[] chars, IrIntVar length) {
        super(getCharDomains(Check.noNulls(chars)), Check.notNull(length).getDomain());
        this.name = Check.notNull(name);
        this.chars = chars;
        this.length = length;
    }

    private static IrDomain[] getCharDomains(IrIntExpr[] chars) {
        IrDomain[] charDomains = new IrDomain[chars.length];
        for (int i = 0; i < charDomains.length; i++) {
            charDomains[i] = chars[i].getDomain();
        }
        return charDomains;
    }

    @Override
    public String getName() {
        return name;
    }

    public IrIntVar[] getChars() {
        return chars;
    }

    public IrIntVar getLength() {
        return length;
    }

    @Override
    public <A, B> B accept(IrStringExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        return Arrays.toString(chars) + "[length=" + length + "]";
    }
}
