package org.clafer.ast;

import static org.clafer.ast.Asts.*;

/**
 *
 * @param <T> the parameter type
 * @author jimmy
 */
public abstract class AstExprRewriter<T> implements AstExprVisitor<T, AstExpr> {

    public AstBoolExpr rewrite(AstBoolExpr expr, T t) {
        return (AstBoolExpr) expr.accept(this, t);
    }

    public AstBoolExpr[] rewrite(AstBoolExpr[] exprs, T t) {
        AstBoolExpr[] rewritten = new AstBoolExpr[exprs.length];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(exprs[i], t);
        }
        return rewritten;
    }

    public AstSetExpr rewrite(AstSetExpr expr, T t) {
        return (AstSetExpr) expr.accept(this, t);
    }

    public AstSetExpr[] rewrite(AstSetExpr[] exprs, T t) {
        AstSetExpr[] rewritten = new AstSetExpr[exprs.length];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(exprs[i], t);
        }
        return rewritten;
    }

    @Override
    public AstExpr visit(AstThis ast, T a) {
        return ast;
    }

    @Override
    public AstExpr visit(AstGlobal ast, T a) {
        return ast;
    }

    @Override
    public AstExpr visit(AstConstant ast, T a) {
        return ast;
    }

    @Override
    public AstExpr visit(AstStringConstant ast, T a) {
        return ast;
    }

    @Override
    public AstExpr visit(AstJoin ast, T a) {
        return join(rewrite(ast.getLeft(), a), ast.getRight());
    }

    @Override
    public AstExpr visit(AstJoinParent ast, T a) {
        return joinParent(rewrite(ast.getChildren(), a));
    }

    @Override
    public AstExpr visit(AstJoinRef ast, T a) {
        return joinRef(rewrite(ast.getDeref(), a));
    }

    @Override
    public AstExpr visit(AstNot ast, T a) {
        return not(rewrite(ast.getExpr(), a));
    }

    @Override
    public AstExpr visit(AstMinus ast, T a) {
        return minus(rewrite(ast.getExpr(), a));
    }

    @Override
    public AstExpr visit(AstCard ast, T a) {
        return card(rewrite(ast.getSet(), a));
    }

    @Override
    public AstExpr visit(AstSetTest ast, T a) {
        return test(rewrite(ast.getLeft(), a), ast.getOp(), rewrite(ast.getRight(), a));
    }

    @Override
    public AstExpr visit(AstCompare ast, T a) {
        return compare(rewrite(ast.getLeft(), a), ast.getOp(), rewrite(ast.getRight(), a));
    }

    @Override
    public AstExpr visit(AstArithm ast, T a) {
        return arithm(ast.getOp(), rewrite(ast.getOperands(), a));
    }

    @Override
    public AstExpr visit(AstMod ast, T a) {
        return mod(rewrite(ast.getDividend(), a), rewrite(ast.getDivisor(), a));
    }

    @Override
    public AstExpr visit(AstSum ast, T a) {
        return sum(rewrite(ast.getSet(), a));
    }

    @Override
    public AstExpr visit(AstProduct ast, T a) {
        return product(rewrite(ast.getSet(), a));
    }

    @Override
    public AstExpr visit(AstBoolArithm ast, T a) {
        return arithm(ast.getOp(), rewrite(ast.getOperands(), a));
    }

    @Override
    public AstExpr visit(AstDifference ast, T a) {
        return diff(rewrite(ast.getLeft(), a), rewrite(ast.getRight(), a));
    }

    @Override
    public AstExpr visit(AstIntersection ast, T a) {
        return inter(rewrite(ast.getLeft(), a), rewrite(ast.getRight(), a));
    }

    @Override
    public AstExpr visit(AstUnion ast, T a) {
        return union(rewrite(ast.getLeft(), a), rewrite(ast.getRight(), a));
    }

    @Override
    public AstExpr visit(AstMembership ast, T a) {
        return membership(rewrite(ast.getMember(), a), ast.getOp(), rewrite(ast.getSet(), a));
    }

    @Override
    public AstExpr visit(AstTernary ast, T a) {
        return ifThenElse(rewrite(ast.getAntecedent(), a), rewrite(ast.getConsequent(), a), rewrite(ast.getAlternative(), a));
    }

    @Override
    public AstExpr visit(AstIfThenElse ast, T a) {
        return ifThenElse(rewrite(ast.getAntecedent(), a), rewrite(ast.getConsequent(), a), rewrite(ast.getAlternative(), a));
    }

    @Override
    public AstExpr visit(AstDowncast ast, T a) {
        return downcast(rewrite(ast.getBase(), a), ast.getTarget());
    }

    @Override
    public AstExpr visit(AstUpcast ast, T a) {
        return upcast(rewrite(ast.getBase(), a), ast.getTarget());
    }

    @Override
    public AstExpr visit(AstLocal ast, T a) {
        return ast;
    }

    @Override
    public AstExpr visit(AstQuantify ast, T a) {
        AstDecl[] decls = new AstDecl[ast.getDecls().length];
        for (int i = 0; i < decls.length; i++) {
            AstDecl decl = ast.getDecls()[i];
            AstLocal[] locals = new AstLocal[decl.getLocals().length];
            for (int j = 0; j < locals.length; j++) {
                locals[j] = (AstLocal) rewrite(decl.getLocals()[j], a);
            }
            decls[i] = decl(decl.isDisjoint(), locals, rewrite(decl.getBody(), a));
        }
        return quantify(ast.getQuantifier(), decls, rewrite(ast.getBody(), a));
    }

    @Override
    public AstExpr visit(AstLength ast, T a) {
        return length(rewrite(ast.getString(), a));
    }

    @Override
    public AstExpr visit(AstConcat ast, T a) {
        return concat(rewrite(ast.getLeft(), a), rewrite(ast.getRight(), a));
    }

    @Override
    public AstExpr visit(AstPrefix ast, T a) {
        return prefix(rewrite(ast.getPrefix(), a), rewrite(ast.getWord(), a));
    }

    @Override
    public AstExpr visit(AstSuffix ast, T a) {
        return suffix(rewrite(ast.getSuffix(), a), rewrite(ast.getWord(), a));
    }

    @Override
    public AstExpr visit(AstChildRelation ast, T a) {
        return ast;
    }

    @Override
    public AstExpr visit(AstParentRelation ast, T a) {
        return ast;
    }

    @Override
    public AstExpr visit(AstRefRelation ast, T a) {
        return ast;
    }

    @Override
    public AstExpr visit(AstInverse ast, T a) {
        return inverse(rewrite(ast.getRelation(), a));
    }

    @Override
    public AstExpr visit(AstTransitiveClosure ast, T a) {
        return transitiveClosure(rewrite(ast.getRelation(), a), ast.isReflexive());
    }
}
