package org.clafer.ir;

/**
 *
 * @param <T> the parameter type
 * @author jimmy
 */
public abstract class IrTraverser<T>
        implements IrIntExprVisitor<T, Void>,
        IrSetExprVisitor<T, Void>,
        IrStringExprVisitor<T, Void>,
        IrIntArrayExprVisitor<T, Void>,
        IrSetArrayExprVisitor<T, Void> {

    public void traverse(IrModule module, T a) {
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

    public void traverse(IrIntArrayExpr expr, T a) {
        expr.accept(this, a);
    }

    public void traverse(IrSetArrayExpr expr, T a) {
        expr.accept(this, a);
    }

    @Override
    public Void visit(IrRegister ir, T a) {
        IrVar variable = ir.getVariable();
        if (variable instanceof IrBoolVar) {
            traverse((IrBoolVar) variable, a);
        } else if (variable instanceof IrIntVar) {
            traverse((IrIntVar) variable, a);
        } else if (variable instanceof IrSetVar) {
            traverse((IrSetVar) variable, a);
        } else {
            traverse((IrStringVar) variable, a);
        }
        return null;
    }

    @Override
    public Void visit(IrBoolVar ir, T a) {
        return null;
    }

    @Override
    public Void visit(IrNot ir, T a) {
        return null;
    }

    @Override
    public Void visit(IrAnd ir, T a) {
        traverse(ir.getOperands(), a);
        return null;
    }

    @Override
    public Void visit(IrLone ir, T a) {
        traverse(ir.getOperands(), a);
        return null;
    }

    @Override
    public Void visit(IrOne ir, T a) {
        traverse(ir.getOperands(), a);
        return null;
    }

    @Override
    public Void visit(IrOr ir, T a) {
        traverse(ir.getOperands(), a);
        return null;
    }

    @Override
    public Void visit(IrImplies ir, T a) {
        traverse(ir.getAntecedent(), a);
        traverse(ir.getConsequent(), a);
        return null;
    }

    @Override
    public Void visit(IrNotImplies ir, T a) {
        traverse(ir.getAntecedent(), a);
        traverse(ir.getConsequent(), a);
        return null;
    }

    @Override
    public Void visit(IrIfThenElse ir, T a) {
        traverse(ir.getAntecedent(), a);
        traverse(ir.getConsequent(), a);
        traverse(ir.getAlternative(), a);
        return null;
    }

    @Override
    public Void visit(IrIfOnlyIf ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return null;
    }

    @Override
    public Void visit(IrXor ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return null;
    }

    @Override
    public Void visit(IrWithin ir, T a) {
        traverse(ir.getValue(), a);
        return null;
    }

    @Override
    public Void visit(IrCompare ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return null;
    }

    @Override
    public Void visit(IrSetEquality ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return null;
    }

    @Override
    public Void visit(IrStringCompare ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return null;
    }

    @Override
    public Void visit(IrMember ir, T a) {
        traverse(ir.getElement(), a);
        traverse(ir.getSet(), a);
        return null;
    }

    @Override
    public Void visit(IrNotMember ir, T a) {
        traverse(ir.getElement(), a);
        traverse(ir.getSet(), a);
        return null;
    }

    @Override
    public Void visit(IrSubsetEq ir, T a) {
        traverse(ir.getSubset(), a);
        traverse(ir.getSuperset(), a);
        return null;
    }

    @Override
    public Void visit(IrBoolChannel ir, T a) {
        traverse(ir.getBools(), a);
        traverse(ir.getSet(), a);
        return null;
    }

    @Override
    public Void visit(IrIntChannel ir, T a) {
        traverse(ir.getInts(), a);
        traverse(ir.getSets(), a);
        return null;
    }

    @Override
    public Void visit(IrSortStrings ir, T a) {
        traverse(ir.getStrings(), a);
        return null;
    }

    @Override
    public Void visit(IrSortSets ir, T a) {
        traverse(ir.getSets(), a);
        return null;
    }

    @Override
    public Void visit(IrSortStringsChannel ir, T a) {
        traverse(ir.getStrings(), a);
        traverse(ir.getInts(), a);
        return null;
    }

    @Override
    public Void visit(IrAllDifferent ir, T a) {
        traverse(ir.getOperands(), a);
        return null;
    }

    @Override
    public Void visit(IrSelectN ir, T a) {
        traverse(ir.getBools(), a);
        traverse(ir.getN(), a);
        return null;
    }

    @Override
    public Void visit(IrAcyclic ir, T a) {
        traverse(ir.getEdges(), a);
        return null;
    }

    @Override
    public Void visit(IrUnreachable ir, T a) {
        traverse(ir.getEdges(), a);
        return null;
    }

    @Override
    public Void visit(IrFilterString ir, T a) {
        traverse(ir.getSet(), a);
        traverse(ir.getString(), a);
        traverse(ir.getResult(), a);
        return null;
    }

    @Override
    public Void visit(IrPrefix ir, T a) {
        traverse(ir.getPrefix(), a);
        traverse(ir.getWord(), a);
        return null;
    }

    @Override
    public Void visit(IrSuffix ir, T a) {
        traverse(ir.getSuffix(), a);
        traverse(ir.getWord(), a);
        return null;
    }

    @Override
    public Void visit(IrIntVar ir, T a) {
        return null;
    }

    @Override
    public Void visit(IrMinus ir, T a) {
        traverse(ir.getExpr(), a);
        return null;
    }

    @Override
    public Void visit(IrCard ir, T a) {
        traverse(ir.getSet(), a);
        return null;
    }

    @Override
    public Void visit(IrAdd ir, T a) {
        traverse(ir.getAddends(), a);
        return null;
    }

    @Override
    public Void visit(IrMul ir, T a) {
        traverse(ir.getMultiplicand(), a);
        traverse(ir.getMultiplier(), a);
        return null;
    }

    @Override
    public Void visit(IrDiv ir, T a) {
        traverse(ir.getDividend(), a);
        traverse(ir.getDivisor(), a);
        return null;
    }

    @Override
    public Void visit(IrMod ir, T a) {
        traverse(ir.getDividend(), a);
        traverse(ir.getDivisor(), a);
        return null;
    }

    @Override
    public Void visit(IrElement ir, T a) {
        traverse(ir.getArray(), a);
        traverse(ir.getIndex(), a);
        return null;
    }

    @Override
    public Void visit(IrCount ir, T a) {
        traverse(ir.getArray(), a);
        return null;
    }

    @Override
    public Void visit(IrCountNotEqual ir, T a) {
        traverse(ir.getArray(), a);
        return null;
    }

    @Override
    public Void visit(IrSetMax ir, T a) {
        traverse(ir.getSet(), a);
        return null;
    }

    @Override
    public Void visit(IrSetSum ir, T a) {
        traverse(ir.getSet(), a);
        return null;
    }

    @Override
    public Void visit(IrTernary ir, T a) {
        traverse(ir.getAntecedent(), a);
        traverse(ir.getConsequent(), a);
        traverse(ir.getAlternative(), a);
        return null;
    }

    @Override
    public Void visit(IrLength ir, T a) {
        traverse(ir.getString(), a);
        return null;
    }

    @Override
    public Void visit(IrSetVar ir, T a) {
        return null;
    }

    @Override
    public Void visit(IrSingleton ir, T a) {
        traverse(ir.getValue(), a);
        return null;
    }

    @Override
    public Void visit(IrArrayToSet ir, T a) {
        traverse(ir.getArray(), a);
        return null;
    }

    @Override
    public Void visit(IrSetElement ir, T a) {
        traverse(ir.getArray(), a);
        traverse(ir.getIndex(), a);
        return null;
    }

    @Override
    public Void visit(IrJoinRelation ir, T a) {
        traverse(ir.getTake(), a);
        traverse(ir.getChildren(), a);
        return null;
    }

    @Override
    public Void visit(IrJoinFunction ir, T a) {
        traverse(ir.getTake(), a);
        traverse(ir.getRefs(), a);
        return null;
    }

    @Override
    public Void visit(IrSetDifference ir, T a) {
        traverse(ir.getMinuend(), a);
        traverse(ir.getSubtrahend(), a);
        return null;
    }

    @Override
    public Void visit(IrSetIntersection ir, T a) {
        traverse(ir.getOperands(), a);
        return null;
    }

    @Override
    public Void visit(IrSetUnion ir, T a) {
        traverse(ir.getOperands(), a);
        return null;
    }

    @Override
    public Void visit(IrOffset ir, T a) {
        traverse(ir.getSet(), a);
        return null;
    }

    @Override
    public Void visit(IrMask ir, T a) {
        traverse(ir.getSet(), a);
        return null;
    }

    @Override
    public Void visit(IrSetTernary ir, T a) {
        traverse(ir.getAntecedent(), a);
        traverse(ir.getConsequent(), a);
        traverse(ir.getAlternative(), a);
        return null;
    }

    @Override
    public Void visit(IrStringVar ir, T a) {
        return null;
    }

    @Override
    public Void visit(IrStringElement ir, T a) {
        traverse(ir.getArray(), a);
        traverse(ir.getIndex(), a);
        return null;
    }

    @Override
    public Void visit(IrConcat ir, T a) {
        traverse(ir.getLeft(), a);
        traverse(ir.getRight(), a);
        return null;
    }

    @Override
    public Void visit(IrIntArrayVar ir, T a) {
        traverse(ir.getArray(), a);
        return null;
    }

    @Override
    public Void visit(IrSetArrayVar ir, T a) {
        traverse(ir.getArray(), a);
        return null;
    }

    @Override
    public Void visit(IrInverse ir, T a) {
        traverse(ir.getRelation(), a);
        return null;
    }

    @Override
    public Void visit(IrTransitiveClosure ir, T a) {
        traverse(ir.getRelation(), a);
        return null;
    }
}
