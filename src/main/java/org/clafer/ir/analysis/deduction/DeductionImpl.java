package org.clafer.ir.analysis.deduction;

//package org.clafer.ir.analysis.propagate;
//
//import java.util.HashMap;
//import java.util.Map;
//import org.clafer.collection.DisjointSets;
//import org.clafer.common.UnsatisfiableException;
//import org.clafer.domain.Domain;
//import org.clafer.ir.IrIntExpr;
//import org.clafer.ir.IrIntVar;
//import org.clafer.ir.IrSetExpr;
//
///**
// *
// * @author jimmy
// */
//public class DeductionImpl implements Deduction {
//
//    private final Map<Class<?>, IntDeducer<?>> intDeducers = new HashMap<>();
//
//    private final DisjointSets<IrIntVar> intEquals = new DisjointSets<>();
//    private final DisjointSets<IrIntVar> intNotEquals = new DisjointSets<>();
//    private final Map<IrIntVar, Domain> intRetains = new HashMap<>();
//
//    @Override
//    public void equal(IrIntExpr left, IrIntExpr right) {
//        if (left instanceof IrIntVar && right instanceof IrIntVar) {
//            intEquals.union((IrIntVar) left, (IrIntVar) right);
//        } else {
//            within(left, right.getDomain());
//            within(right, left.getDomain());
//        }
//    }
//
//    @Override
//    public void notEqual(IrIntExpr left, IrIntExpr right) {
//        if (left instanceof IrIntVar && right instanceof IrIntVar) {
//            intNotEquals.union((IrIntVar) left, (IrIntVar) right);
//        } else if (left.getDomain().isConstant()) {
//            within(right, right.getDomain().remove(left.getDomain().getLowBound()));
//        } else if (right.getDomain().isConstant()) {
//            within(left, left.getDomain().remove(right.getDomain().getLowBound()));
//        }
//    }
//
//    @Override
//    public void lessThan(IrIntExpr left, IrIntExpr right) {
//        Domain leftDomain = left.getDomain();
//        Domain rightDomain = right.getDomain();
//        if (leftDomain.getHighBound() >= rightDomain.getHighBound()) {
//            within(left, leftDomain.boundHigh(rightDomain.getHighBound() - 1));
//        }
//        if (rightDomain.getLowBound() <= leftDomain.getLowBound()) {
//            within(right, rightDomain.boundLow(leftDomain.getLowBound() + 1));
//        }
//    }
//
//    @Override
//    public void lessThanEqual(IrIntExpr left, IrIntExpr right) {
//        Domain leftDomain = left.getDomain();
//        Domain rightDomain = right.getDomain();
//        if (leftDomain.getHighBound() > rightDomain.getHighBound()) {
//            within(left, leftDomain.boundHigh(rightDomain.getHighBound()));
//        }
//        if (rightDomain.getLowBound() < leftDomain.getLowBound()) {
//            within(right, rightDomain.boundLow(leftDomain.getLowBound()));
//        }
//    }
//
//    @Override
//    public void atLeastNValue(IrIntExpr[] exprs, int n) {
//    }
//
//    @Override
//    public void atMostNValue(IrIntExpr[] exprs, int n) {
//    }
//
//    @Override
//    public void within(IrIntExpr expr, Domain domain) {
//        if (!domain.isSubsetOf(expr.getDomain())) {
//            if (expr instanceof IrIntVar) {
//                intRetains.merge((IrIntVar) expr, domain, Domain::intersection);
//            } else {
//                IntDeducer intDeducer = intDeducers.get(expr.getClass());
//                if (intDeducer != null) {
//                    intDeducer.deduce(expr, domain, this);
//                }
//            }
//        }
//    }
//
//    @Override
//    public void kerContains(IrSetExpr expr, Domain domain) {
//        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
//    }
//
//    @Override
//    public void envSubsetOf(IrSetExpr expr, Domain domain) {
//        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
//    }
//
//    @Override
//    public void cardWithin(IrSetExpr expr, Domain domain) {
//        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
//    }
//
//    @Override
//    public void fail() {
//        throw new UnsatisfiableException();
//    }
//
//    @Override
//    public void failIf(boolean condition) {
//        if (condition) {
//            throw new UnsatisfiableException();
//        }
//    }
//}
