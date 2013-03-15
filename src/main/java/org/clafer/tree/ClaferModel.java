package org.clafer.tree;

import static choco.Choco.*;
import choco.Options;
import choco.kernel.model.Model;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.clafer.Check;
import org.clafer.Util;
import org.clafer.tree.analysis.Analysis;

/**
 *
 * @author jimmy
 */
public class ClaferModel {

    private final RootClafer root;
    private final IntClafer intClafer;
    private final List<AbstractClafer> abstractClafers = new ArrayList<AbstractClafer>();

    public ClaferModel(int bitwidth) {
        root = new RootClafer();
        intClafer = new IntClafer(bitwidth);
    }

    public RootClafer getRoot() {
        return root;
    }

    public IntClafer getIntClafer() {
        return intClafer;
    }

    public List<AbstractClafer> getAbstractClafers() {
        return Collections.unmodifiableList(abstractClafers);
    }

    public ConcreteClafer newTopClafer(String name, int scope, Card card) {
        return root.addChildClafer(name, scope, card);
    }

    public AbstractClafer newAbstractClafer(String name, int scope) {
        AbstractClafer abstractClafer = new AbstractClafer(name, scope);
        abstractClafers.add(abstractClafer);
        return abstractClafer;
    }

    public void build(Model model) {
        Analysis analysis = Analysis.analyze(this);

        List<AtomicClafer> atomicClafers = new ArrayList<AtomicClafer>();

        for (AtomicClafer start : Util.cons(root, abstractClafers)) {
            for (Clafer clafer : TreeUtil.getAllNestedClafers(start)) {
                clafer.build(model, analysis);
                if (clafer instanceof AtomicClafer) {
                    atomicClafers.add((AtomicClafer) clafer);
                }
            }
        }

        for (AtomicClafer atomicClafer : atomicClafers) {
            ThisFactory thisFactory = new CacheThisFactory(model, atomicClafer);
            for (ClaferConstraint constraint : atomicClafer.getConstraints()) {
                constraint.build(model, thisFactory, analysis);
            }
        }
    }

    /**
     * Creating SetVariables for "this" has a cost since it requires a constraints.
     * The idea is to not create SetVariables unless they are needed, and reuse them
     * if they are needed mutiple times.
     */
    private class CacheThisFactory implements ThisFactory {

        private final Model model;
        private final AtomicClafer thisType;
        private final SetExpr[] cache;

        public CacheThisFactory(Model model, AtomicClafer thisType) {
            if (thisType.getScopeLow() != 0) {
                throw new IllegalArgumentException();
            }
            this.model = Check.notNull(model);
            this.thisType = Check.notNull(thisType);
            this.cache = new SetExpr[thisType.getScope()];
        }

        private void checkId(int id) {
            if (id >= cache.length) {
                throw new IllegalArgumentException(id + " is outside the scope of " + thisType.getName());
            }
        }

        @Override
        public IntExpr newIntThis(int id) {
            checkId(id);
            return new IntExpr(thisType, constant(id));
        }

        @Override
        public SetExpr newSetThis(int id) {
            checkId(id);
            if (cache[id] == null) {
                SetVariable thisV = makeSetVar("constraintUnder" + thisType.getName() + id, id, id, Options.V_NO_DECISION);
                model.addConstraint(eq(thisType.getMembership()[id], thisV.getCard()));
                /*
                 * Essentially the same as:
                 *   model.addConstraint(implies(eq(thisType.getMembership()[id], 1), eq(thisV, constant(new int[]{id}))));
                 *   model.addConstraint(implies(eq(thisType.getMembership()[id], 0), eq(thisV, emptySet())));
                 */
                cache[id] = new SetExpr(thisType, thisV);
            }
            return cache[id];
        }
    }

    public void print(Solver solver, Appendable output) throws IOException {
        root.print(solver, output);
    }
}
