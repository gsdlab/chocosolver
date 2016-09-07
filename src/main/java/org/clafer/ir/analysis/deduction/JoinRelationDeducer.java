package org.clafer.ir.analysis.deduction;

import java.util.Arrays;
import java.util.OptionalInt;
import java.util.PrimitiveIterator;
import org.clafer.common.Util;
import org.clafer.domain.Domain;
import org.clafer.ir.IrJoinRelation;
import org.clafer.ir.IrSetArrayExpr;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.Irs;

/**
 *
 * @author jimmy
 */
class JoinRelationDeducer implements SetDeducer<IrJoinRelation> {

    @Override
    public void deduceKer(IrJoinRelation ir, Domain ker, Deduction deduction) {
        IrSetExpr take = ir.getTake();
        IrSetArrayExpr children = ir.getChildren();

        PrimitiveIterator.OfInt iter = ker.difference(ir.getKer()).iterator();
        while (iter.hasNext()) {
            int val = iter.next();
            OptionalInt index = Util.findUnique(
                    take.getEnv().iterator(),
                    j -> children.getEnvs()[j].contains(val));
            if (index.isPresent()) {
                deduction.kerContains(take, index.getAsInt());
                deduction.kerContains(Irs.get(children, index.getAsInt()), val);
            }
        }
    }

    @Override
    public void deduceEnv(IrJoinRelation ir, Domain env, Deduction deduction) {
        IrSetExpr take = ir.getTake();
        IrSetArrayExpr children = ir.getChildren();

        Domain domain = take.getEnv().retainAll(
                i -> children.getKers()[i].isSubsetOf(env));
        deduction.envSubsetOf(ir.getTake(), domain);

        take.getKer().forEach(
                i -> deduction.envSubsetOf(Irs.get(children, i), env));
    }

    @Override
    public void deduceCard(IrJoinRelation ir, Domain card, Deduction deduction) {
        IrSetExpr take = ir.getTake();
        IrSetArrayExpr children = ir.getChildren();
        int lb = card.getLowBound();
        int ub = card.getHighBound();
        int[] envLbs = new int[take.getEnv().size() - take.getKer().size()];
        int[] envUbs = new int[envLbs.length];
        int kerMinCard = 0;
        int kerMaxCard = 0;
        int env = 0;
        PrimitiveIterator.OfInt iter = take.getEnv().iterator();
        while (iter.hasNext()) {
            int i = iter.next();
            if (take.getKer().contains(i)) {
                kerMinCard += children.getCards()[i].getLowBound();
                kerMaxCard += children.getCards()[i].getHighBound();
                deduction.cardLessThanEqual(Irs.get(children, i), ub);
            } else {
                envLbs[env] = children.getCards()[i].getLowBound();
                envUbs[env] = children.getCards()[i].getHighBound();
                env++;
            }
        }
        Arrays.sort(envLbs);
        Arrays.sort(envUbs);
        int i;
        for (i = 0; i < envLbs.length && (kerMinCard < ub || envLbs[i] == 0); i++) {
            kerMinCard += envLbs[i];
        }
        if (i < envLbs.length) {
            kerMinCard += envLbs[i];
        }
        int high = i + take.getKer().size();
        for (i = envUbs.length - 1; i >= 0 && kerMaxCard < lb; i--) {
            kerMaxCard += envUbs[i];
        }
        if (i >= 0) {
            kerMaxCard += envUbs[i];
        }
        int low = envUbs.length - 1 - i + take.getKer().size();
        if (low > take.getCard().getLowBound() || high < take.getCard().getHighBound()) {
            deduction.cardGreaterThanEqual(take, low);
            deduction.cardLessThanEqual(take, high);
        }
        iter = take.getKer().iterator();
        while (iter.hasNext()) {
            int ker = iter.next();
            deduction.cardGreaterThanEqual(Irs.get(children, ker),
                    lb - kerMaxCard + children.getCards()[ker].getHighBound());
            deduction.cardLessThanEqual(Irs.get(children, ker),
                    ub - kerMinCard + children.getCards()[ker].getLowBound());
        }
    }
}
