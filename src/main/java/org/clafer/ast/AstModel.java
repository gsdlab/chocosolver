package org.clafer.ast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author jimmy
 */
public class AstModel {

    private final List<AstAbstractClafer> abstractClafers;
    private final List<AstConcreteClafer> topClafers;

    public AstModel() {
        this.abstractClafers = new ArrayList<AstAbstractClafer>();
        this.topClafers = new ArrayList<AstConcreteClafer>();
    }

    public AstModel(List<AstAbstractClafer> abstractClafers, List<AstConcreteClafer> topClafers) {
        this.abstractClafers = new ArrayList<AstAbstractClafer>(abstractClafers);
        this.topClafers = new ArrayList<AstConcreteClafer>(topClafers);
    }

    public List<AstAbstractClafer> getAbstractClafers() {
        return Collections.unmodifiableList(abstractClafers);
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
