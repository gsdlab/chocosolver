package org.clafer.ast;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author jimmy
 */
public class AstModel implements Serializable {

    private final List<AstAbstractClafer> abstractClafers;
    private final List<AstConcreteClafer> topClafers;

    AstModel() {
        this.abstractClafers = new ArrayList<AstAbstractClafer>();
        this.topClafers = new ArrayList<AstConcreteClafer>();
    }

    public List<AstAbstractClafer> getAbstractClafers() {
        return abstractClafers;
    }

    public AstAbstractClafer addAbstractClafer(String name) {
        AstAbstractClafer abstractClafer = new AstAbstractClafer(name);
        abstractClafers.add(abstractClafer);
        return abstractClafer;
    }

    public List<AstConcreteClafer> getTopClafers() {
        return topClafers;
    }

    public AstConcreteClafer addTopClafer(String name) {
        AstConcreteClafer topClafer = new AstConcreteClafer(name);
        topClafers.add(topClafer);
        return topClafer;
    }
}
