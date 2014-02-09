package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrStringVar extends IrAbstractString implements IrVar {

    private final String name;
    private final IrIntVar length;
    private final IrIntVar[] chars;

    IrStringVar(String name, IrIntVar length, IrIntVar[] chars) {
        super(Check.notNull(length).getDomain(), getCharDomains(Check.noNulls(chars)));
        this.name = Check.notNull(name);
        this.length = length;
        this.chars = chars;
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

    public IrIntVar getLength() {
        return length;
    }

    public IrIntVar[] getChars() {
        return chars;
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
