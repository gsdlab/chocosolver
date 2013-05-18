package org.clafer.ir;

import org.clafer.common.Check;
import solver.Solver;
import solver.constraints.IntConstraintFactory;
import solver.constraints.LogicalConstraintFactory;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class IrCard extends IrAbstractInt implements IrIntExpr {

    private final IrSetExpr set;

    IrCard(IrSetExpr set, IrDomain domain) {
        super(domain);
        this.set = Check.notNull(set);
        if (!IrUtil.isSubsetOf(domain, set.getCard())) {
            throw new IllegalArgumentException();
        }
    }

    public IrSetExpr getSet() {
        return set;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrCard) {
            IrCard other = (IrCard) obj;
            return set.equals(other.set) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 31 * set.hashCode();
    }

    @Override
    public String toString() {
        return "|" + set + "|";
    }

//    public static void main(String[] args) {
//        AstModel m = newModel();
//        AstConcreteClafer hand = m.addTopClafer("Hand").withCard(0, 1);
//        AstConcreteClafer age = hand.addChild("Age").withCard(1, 1).refToUnique(IntType);
//        hand.addConstraint(equal(joinRef(join($this(), age)), constant(3)));
//        
//        ClaferSolver solver = ClaferCompiler.compile(m, new Scope(1));
//        System.out.println(solver.solver);
//        while (solver.find()) {
//            System.out.println(solver.solver);
//            System.out.println(solver.instance());
//        }
//    }
public static void main(String[] args) {
    Solver solver = new Solver();
    BoolVar v = VariableFactory.bool("v", solver);
    IntVar i1 = VariableFactory.enumerated("i1", 0, 10, solver);
    IntVar i2 = VariableFactory.enumerated("i2", 0, 10, solver);
    solver.post(LogicalConstraintFactory.ifThenElse(v,
            IntConstraintFactory.arithm(i1, "=", 3),
            IntConstraintFactory.arithm(i2, "=", 3)));
    solver.post(LogicalConstraintFactory.ifThenElse(v,
            IntConstraintFactory.arithm(i2, "=", 0),
            IntConstraintFactory.arithm(i1, "=", 0)));

    solver.set(IntStrategyFactory.firstFail_InDomainMin(new IntVar[]{i1, i2}));

    if (solver.findSolution()) {
        do {
            System.out.println(solver);
        } while (solver.nextSolution());
    }
}
}
