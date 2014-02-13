package org.clafer.ir;

/**
 *
 * @param <T> the parameter type
 * @author jimmy
 */
public abstract class IrTraverser<T>
        implements IrIntExprVisitor<T, IrIntExpr>,
        IrSetExprVisitor<T, IrSetExpr>,
        IrStringExprVisitor<T, IrStringExpr> {

    public void traverse(IrModule module, T a) {
        for (IrVar variable : module.getVariables()) {
            if (variable instanceof IrBoolVar) {
                traverse((IrBoolVar) variable, a);
            } else if (variable instanceof IrIntVar) {
                traverse((IrIntVar) variable, a);
            } else if (variable instanceof IrSetVar) {
                traverse((IrSetVar) variable, a);
            } else {
                traverse((IrStringVar) variable, a);
            }
        }
        for (IrBoolExpr constraint : module.getConstraints()) {
            traverse(constraint, a);
        }
    }

    public void traverse(IrBoolExpr expr, T a) {
        expr.accept(this, a);
    }

    public void traverse(IrBoolExpr[] exprs, T a) {
        for (IrBoolExpr expr : exprs) {
            traverse(expr, a);
        }
    }

    public void traverse(IrIntExpr expr, T a) {
        expr.accept(this, a);
    }

    public void traverse(IrIntExpr[] exprs, T a) {
        for (IrIntExpr expr : exprs) {
            traverse(expr, a);
        }
    }

    public void traverse(IrIntExpr[][] exprs, T a) {
        for (IrIntExpr[] expr : exprs) {
            traverse(expr, a);
        }
    }

    public void traverse(IrSetExpr expr, T a) {
        expr.accept(this, a);
    }

    public void traverse(IrSetExpr[] exprs, T a) {
        for (IrSetExpr expr : exprs) {
            traverse(expr, a);
        }
    }

    public void traverse(IrStringExpr expr, T a) {
        expr.accept(this, a);
    }

    public void traverse(IrStringExpr[] exprs, T a) {
        for (IrStringExpr expr : exprs) {
            traverse(expr, a);
        }
    }

    @Override
    public IrBoolVar visit(IrBoolVar ir, T a) {
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrNot ir, T a) {
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrAnd ir, T a) {
        traverse(ir.getOperands(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrLone ir, T a) {
        traverse(ir.getOperands(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrOne ir, T a) {
        traverse(ir.getOperands(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrOr ir, T a) {
        traverse(ir.getOperands(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrImplies ir, T a) {
        traverse(ir.getAntecedent(), a);
        traverse(ir.getConsequent(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrNotImplies ir, T a) {
        traverse(ir.getAntecedent(), a);
        traverse(ir.getConsequent(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrIfThenElse ir, T a) {
        traverse(ir.getAntecedent(), a);
        traverse(ir.getConsequent(), a);
        traverse(ir.getAlternative(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrIfOnlyIf ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrXor ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrWithin ir, T a) {
        traverse(ir.getValue(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrNotWithin ir, T a) {
        traverse(ir.getValue(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrCompare ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrSetTest ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrStringCompare ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrMember ir, T a) {
        traverse(ir.getElement(), a);
        traverse(ir.getSet(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrNotMember ir, T a) {
        traverse(ir.getElement(), a);
        traverse(ir.getSet(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrSubsetEq ir, T a) {
        traverse(ir.getSubset(), a);
        traverse(ir.getSuperset(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrBoolChannel ir, T a) {
        traverse(ir.getBools(), a);
        traverse(ir.getSet(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrIntChannel ir, T a) {
        traverse(ir.getInts(), a);
        traverse(ir.getSets(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrSortStrings ir, T a) {
        traverse(ir.getStrings(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrSortSets ir, T a) {
        traverse(ir.getSets(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrSortStringsChannel ir, T a) {
        traverse(ir.getStrings(), a);
        traverse(ir.getInts(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrAllDifferent ir, T a) {
        traverse(ir.getOperands(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrSelectN ir, T a) {
        traverse(ir.getBools(), a);
        traverse(ir.getN(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrAcyclic ir, T a) {
        traverse(ir.getEdges(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrUnreachable ir, T a) {
        traverse(ir.getEdges(), a);
        return ir;
    }

    @Override
    public IrBoolExpr visit(IrFilterString ir, T a) {
        traverse(ir.getSet(), a);
        traverse(ir.getString(), a);
        traverse(ir.getResult(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrPrefix ir, T a) {
        traverse(ir.getPrefix(), a);
        traverse(ir.getWord(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrSuffix ir, T a) {
        traverse(ir.getSuffix(), a);
        traverse(ir.getWord(), a);
        return ir;
    }

    @Override
    public IrIntVar visit(IrIntVar ir, T a) {
        return ir;
    }

    @Override
    public IrIntExpr visit(IrMinus ir, T a) {
        traverse(ir.getExpr(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrCard ir, T a) {
        traverse(ir.getSet(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrAdd ir, T a) {
        traverse(ir.getAddends(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrMul ir, T a) {
        traverse(ir.getMultiplicand(), a);
        traverse(ir.getMultiplier(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrDiv ir, T a) {
        traverse(ir.getDividend(), a);
        traverse(ir.getDivisor(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrElement ir, T a) {
        traverse(ir.getArray(), a);
        traverse(ir.getIndex(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrCount ir, T a) {
        traverse(ir.getArray(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrSetSum ir, T a) {
        traverse(ir.getSet(), a);
        return ir;
    }

    @Override
    public IrIntExpr visit(IrTernary ir, T a) {
        traverse(ir.getAntecedent(), a);
        traverse(ir.getConsequent(), a);
        traverse(ir.getAlternative(), a);
        return ir;
    }

    @Override
    public IrSetVar visit(IrSetVar ir, T a) {
        return ir;
    }

    @Override
    public IrSetExpr visit(IrSingleton ir, T a) {
        traverse(ir.getValue(), a);
        return ir;
    }

    @Override
    public IrSetExpr visit(IrArrayToSet ir, T a) {
        traverse(ir.getArray(), a);
        return ir;
    }

    @Override
    public IrSetExpr visit(IrJoinRelation ir, T a) {
        traverse(ir.getTake(), a);
        traverse(ir.getChildren(), a);
        return ir;
    }

    @Override
    public IrSetExpr visit(IrJoinFunction ir, T a) {
        traverse(ir.getTake(), a);
        traverse(ir.getRefs(), a);
        return ir;
    }

    @Override
    public IrSetExpr visit(IrSetDifference ir, T a) {
        traverse(ir.getMinuend(), a);
        traverse(ir.getSubtrahend(), a);
        return ir;
    }

    @Override
    public IrSetExpr visit(IrSetIntersection ir, T a) {
        traverse(ir.getOperands(), a);
        return ir;
    }

    @Override
    public IrSetExpr visit(IrSetUnion ir, T a) {
        traverse(ir.getOperands(), a);
        return ir;
    }

    @Override
    public IrSetExpr visit(IrOffset ir, T a) {
        traverse(ir.getSet(), a);
        return ir;
    }

    @Override
    public IrSetExpr visit(IrMask ir, T a) {
        traverse(ir.getSet(), a);
        return ir;
    }

    @Override
    public IrSetExpr visit(IrSetTernary ir, T a) {
        traverse(ir.getAntecedent(), a);
        traverse(ir.getConsequent(), a);
        traverse(ir.getAlternative(), a);
        return ir;
    }

    @Override
    public IrStringExpr visit(IrStringVar ir, T a) {
        return ir;
    }

    @Override
    public IrStringExpr visit(IrElementString ir, T a) {
        traverse(ir.getArray(), a);
        traverse(ir.getIndex(), a);
        return ir;
    }

    @Override
    public IrStringExpr visit(IrConcat ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return ir;
    }
}
