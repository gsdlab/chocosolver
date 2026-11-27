package org.clafer.ir.analysis.deduction;

import java.util.Arrays;
import java.util.OptionalInt;
import java.util.PrimitiveIterator;
import org.clafer.common.Util;
import org.clafer.domain.Domain;
import org.clafer.ir.IrIntExpr;
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
        if (ir.isInjective()) {
            IrSetExpr take = ir.getTake();
            IrSetArrayExpr children = ir.getChildren();

            IrIntExpr[] kerChildCards = new IrIntExpr[take.getKer().size()];
            int[] envLbs = new int[take.getEnv().size() - take.getKer().size()];
            int[] envUbs = new int[envLbs.length];

            int ker = 0;
            int env = 0;
            PrimitiveIterator.OfInt iter = take.getEnv().iterator();
            while (iter.hasNext()) {
                int i = iter.next();
                if (take.getKer().contains(i)) {
                    kerChildCards[ker++] = Irs.card(Irs.get(children, i));
                } else {
                    envLbs[env] = children.getCards()[i].getLowBound();
                    envUbs[env] = children.getCards()[i].getHighBound();
                    env++;
                }
            }
            assert ker == kerChildCards.length;
            assert env == envLbs.length;
            assert env == envUbs.length;
            Arrays.sort(envLbs);
            Arrays.sort(envUbs);

            IrIntExpr kerCards = Irs.add(kerChildCards);

            int maxCard = kerCards.getLowBound();
            int i;
            for (i = 0; i < envLbs.length && maxCard + envLbs[i] <= card.getHighBound(); i++) {
                maxCard += envLbs[i];
            }
            int takeHb = take.getKer().size() + i;
            if (takeHb < take.getCard().getHighBound()) {
                deduction.cardLessThanEqual(take, takeHb);
            } else {
                takeHb = take.getCard().getHighBound();
            }

            int minCard = kerCards.getHighBound();
            for (i = envUbs.length - 1; i >= 0 && minCard < card.getLowBound(); i--) {
                minCard += envUbs[i];
            }
            int takeLb = take.getKer().size() + (envUbs.length - i - 1);
            if (takeLb > take.getCard().getLowBound()) {
                deduction.cardGreaterThanEqual(take, takeLb);
            } else {
                takeLb = take.getCard().getLowBound();
            }

            int minRemainingCard = 0;
            for (i = 0; i < takeLb - kerChildCards.length; i++) {
                minRemainingCard += envLbs[i];
            }
            deduction.lessThanEqual(kerCards, card.getHighBound() - minRemainingCard);

            int maxRemainingCard = 0;
            for (i = 0; i < takeHb - kerChildCards.length; i++) {
                maxRemainingCard += envUbs[envUbs.length - i - 1];
            }
            deduction.greaterThanEqual(kerCards, card.getLowBound() - maxRemainingCard);
        }
    }
}
