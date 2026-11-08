package org.clafer.ast;

import static org.clafer.ast.Asts.arithm;
import static org.clafer.ast.Asts.card;
import static org.clafer.ast.Asts.compare;
import static org.clafer.ast.Asts.concat;
import static org.clafer.ast.Asts.connected;
import static org.clafer.ast.Asts.decl;
import static org.clafer.ast.Asts.diff;
import static org.clafer.ast.Asts.domainRestriction;
import static org.clafer.ast.Asts.downcast;
import static org.clafer.ast.Asts.ifThenElse;
import static org.clafer.ast.Asts.inter;
import static org.clafer.ast.Asts.inverse;
import static org.clafer.ast.Asts.join;
import static org.clafer.ast.Asts.joinParent;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.length;
import static org.clafer.ast.Asts.max;
import static org.clafer.ast.Asts.membership;
import static org.clafer.ast.Asts.min;
import static org.clafer.ast.Asts.minus;
import static org.clafer.ast.Asts.mod;
import static org.clafer.ast.Asts.not;
import static org.clafer.ast.Asts.prefix;
import static org.clafer.ast.Asts.product;
import static org.clafer.ast.Asts.quantify;
import static org.clafer.ast.Asts.rangeRestriction;
import static org.clafer.ast.Asts.suffix;
import static org.clafer.ast.Asts.sum;
import static org.clafer.ast.Asts.test;
import static org.clafer.ast.Asts.transitiveClosure;
import static org.clafer.ast.Asts.union;
import static org.clafer.ast.Asts.upcast;

/**
 *
 * @param <T> the parameter type
 * @author jimmy
 */
public abstract class AstExprRewriter<T> {

    /**
     * Rewrites an expression by the method for its kind.
     *
     * @param expr the expression
     * @param t the parameter
     * @return the rewritten expression
     */
    public AstExpr rewrite(AstExpr expr, T t) {
        return switch (expr) {
            case AstArithm e -> visit(e, t);
            case AstBoolArithm e -> visit(e, t);
            case AstCard e -> visit(e, t);
            case AstChildRelation e -> visit(e, t);
            case AstCompare e -> visit(e, t);
            case AstConcat e -> visit(e, t);
            case AstConnected e -> visit(e, t);
            case AstConstant e -> visit(e, t);
            case AstDecimalConstant e -> visit(e, t);
            case AstDifference e -> visit(e, t);
            case AstDomainRestriction e -> visit(e, t);
            case AstDowncast e -> visit(e, t);
            case AstGlobal e -> visit(e, t);
            case AstIfThenElse e -> visit(e, t);
            case AstIntersection e -> visit(e, t);
            case AstInverse e -> visit(e, t);
            case AstJoin e -> visit(e, t);
            case AstJoinParent e -> visit(e, t);
            case AstJoinRef e -> visit(e, t);
            case AstLength e -> visit(e, t);
            case AstLocal e -> visit(e, t);
            case AstMax e -> visit(e, t);
            case AstMembership e -> visit(e, t);
            case AstMin e -> visit(e, t);
            case AstMinus e -> visit(e, t);
            case AstMod e -> visit(e, t);
            case AstNot e -> visit(e, t);
            case AstParentRelation e -> visit(e, t);
            case AstPrefix e -> visit(e, t);
            case AstProduct e -> visit(e, t);
            case AstQuantify e -> visit(e, t);
            case AstRangeRestriction e -> visit(e, t);
            case AstRefRelation e -> visit(e, t);
            case AstSetTest e -> visit(e, t);
            case AstStringConstant e -> visit(e, t);
            case AstSuffix e -> visit(e, t);
            case AstSum e -> visit(e, t);
            case AstTernary e -> visit(e, t);
            case AstThis e -> visit(e, t);
            case AstTransitiveClosure e -> visit(e, t);
            case AstUnion e -> visit(e, t);
            case AstUpcast e -> visit(e, t);
        };
    }

    public AstBoolExpr rewrite(AstBoolExpr expr, T t) {
        return (AstBoolExpr) rewrite((AstExpr) expr, t);
    }

    public AstBoolExpr[] rewrite(AstBoolExpr[] exprs, T t) {
        AstBoolExpr[] rewritten = new AstBoolExpr[exprs.length];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(exprs[i], t);
        }
        return rewritten;
    }

    public AstSetExpr rewrite(AstSetExpr expr, T t) {
        return (AstSetExpr) rewrite((AstExpr) expr, t);
    }

    public AstSetExpr[] rewrite(AstSetExpr[] exprs, T t) {
        AstSetExpr[] rewritten = new AstSetExpr[exprs.length];
        for (int i = 0; i < rewritten.length; i++) {
            rewritten[i] = rewrite(exprs[i], t);
        }
        return rewritten;
    }

    public AstExpr visit(AstThis ast, T a) {
        return ast;
    }

    public AstExpr visit(AstGlobal ast, T a) {
        return ast;
    }

    public AstExpr visit(AstConstant ast, T a) {
        return ast;
    }

    public AstExpr visit(AstStringConstant ast, T a) {
        return ast;
    }

    public AstExpr visit(AstDecimalConstant ast, T a) {
        return ast;
    }

    public AstExpr visit(AstJoin ast, T a) {
        return join(rewrite(ast.left(), a), ast.right());
    }

    public AstExpr visit(AstJoinParent ast, T a) {
        return joinParent(rewrite(ast.children(), a));
    }

    public AstExpr visit(AstJoinRef ast, T a) {
        return joinRef(rewrite(ast.deref(), a));
    }

    public AstExpr visit(AstNot ast, T a) {
        return not(rewrite(ast.expr(), a));
    }

    public AstExpr visit(AstMinus ast, T a) {
        return minus(rewrite(ast.expr(), a));
    }

    public AstExpr visit(AstCard ast, T a) {
        return card(rewrite(ast.set(), a));
    }

    public AstExpr visit(AstMax ast, T a) {
        return max(rewrite(ast.set(), a));
    }

    public AstExpr visit(AstMin ast, T a) {
        return min(rewrite(ast.set(), a));
    }

    public AstExpr visit(AstSetTest ast, T a) {
        return test(rewrite(ast.left(), a), ast.op(), rewrite(ast.right(), a));
    }

    public AstExpr visit(AstCompare ast, T a) {
        return compare(rewrite(ast.left(), a), ast.op(), rewrite(ast.right(), a));
    }

    public AstExpr visit(AstArithm ast, T a) {
        return arithm(ast.op(), rewrite(ast.operands(), a));
    }

    public AstExpr visit(AstMod ast, T a) {
        return mod(rewrite(ast.dividend(), a), rewrite(ast.divisor(), a));
    }

    public AstExpr visit(AstSum ast, T a) {
        return sum(rewrite(ast.set(), a));
    }

    public AstExpr visit(AstProduct ast, T a) {
        return product(rewrite(ast.set(), a));
    }

    public AstExpr visit(AstBoolArithm ast, T a) {
        return arithm(ast.op(), rewrite(ast.operands(), a));
    }

    public AstExpr visit(AstDifference ast, T a) {
        return diff(rewrite(ast.left(), a), rewrite(ast.right(), a));
    }

    public AstExpr visit(AstIntersection ast, T a) {
        return inter(rewrite(ast.left(), a), rewrite(ast.right(), a));
    }

    public AstExpr visit(AstUnion ast, T a) {
        return union(rewrite(ast.left(), a), rewrite(ast.right(), a));
    }

    public AstExpr visit(AstMembership ast, T a) {
        return membership(rewrite(ast.member(), a), ast.op(), rewrite(ast.set(), a));
    }

    public AstExpr visit(AstTernary ast, T a) {
        return ifThenElse(rewrite(ast.antecedent(), a), rewrite(ast.consequent(), a), rewrite(ast.alternative(), a));
    }

    public AstExpr visit(AstIfThenElse ast, T a) {
        return ifThenElse(rewrite(ast.antecedent(), a), rewrite(ast.consequent(), a), rewrite(ast.alternative(), a));
    }

    public AstExpr visit(AstDowncast ast, T a) {
        return downcast(rewrite(ast.base(), a), ast.target());
    }

    public AstExpr visit(AstUpcast ast, T a) {
        return upcast(rewrite(ast.base(), a), ast.target());
    }

    public AstExpr visit(AstLocal ast, T a) {
        return ast;
    }

    public AstExpr visit(AstQuantify ast, T a) {
        AstDecl[] decls = new AstDecl[ast.decls().length];
        for (int i = 0; i < decls.length; i++) {
            AstDecl decl = ast.decls()[i];
            AstLocal[] locals = new AstLocal[decl.getLocals().length];
            for (int j = 0; j < locals.length; j++) {
                locals[j] = (AstLocal) rewrite(decl.getLocals()[j], a);
            }
            decls[i] = decl(decl.isDisjoint(), locals, rewrite(decl.getBody(), a));
        }
        return quantify(ast.quantifier(), decls, rewrite(ast.body(), a));
    }

    public AstExpr visit(AstLength ast, T a) {
        return length(rewrite(ast.string(), a));
    }

    public AstExpr visit(AstConcat ast, T a) {
        return concat(rewrite(ast.left(), a), rewrite(ast.right(), a));
    }

    public AstExpr visit(AstPrefix ast, T a) {
        return prefix(rewrite(ast.prefix(), a), rewrite(ast.word(), a));
    }

    public AstExpr visit(AstSuffix ast, T a) {
        return suffix(rewrite(ast.suffix(), a), rewrite(ast.word(), a));
    }

    public AstExpr visit(AstChildRelation ast, T a) {
        return ast;
    }

    public AstExpr visit(AstParentRelation ast, T a) {
        return ast;
    }

    public AstExpr visit(AstRefRelation ast, T a) {
        return ast;
    }

    public AstExpr visit(AstDomainRestriction ast, T a) {
        return domainRestriction(rewrite(ast.domain(), a), rewrite(ast.relation(), a));
    }

    public AstExpr visit(AstRangeRestriction ast, T a) {
        return rangeRestriction(rewrite(ast.relation(), a), rewrite(ast.range(), a));
    }

    public AstExpr visit(AstInverse ast, T a) {
        return inverse(rewrite(ast.relation(), a));
    }

    public AstExpr visit(AstTransitiveClosure ast, T a) {
        return transitiveClosure(rewrite(ast.relation(), a), ast.reflexive());
    }

    public AstExpr visit(AstConnected ast, T a) {
        AstSetExpr e = rewrite(ast.relation(), a);
        AstSetExpr n = rewrite(ast.nodes(), a);
        return connected(n, e, ast.directed());
    }
}
