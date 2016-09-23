package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.search.strategy.Search;
import org.chocosolver.solver.search.strategy.strategy.AbstractStrategy;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstUtil;
import static org.clafer.ast.Asts.IntType;
import org.clafer.ast.Card;
import org.clafer.choco.constraint.Constraints;
import org.clafer.collection.Pair;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.graph.Vertex;

/**
 *
 * @author jimmy
 */
public class GlobalCardAnalyzer implements Analyzer {

    private Iterable<Set<AstClafer>> order(Analysis analysis) {
        KeyGraph<AstClafer> dependency = new KeyGraph<>();
        for (AstAbstractClafer abstractClafer : analysis.getAbstractClafers()) {
            Vertex<AstClafer> node = dependency.getVertex(abstractClafer);
            abstractClafer.getSubs().stream().map(dependency::getVertex).forEach(node::addNeighbour);
        }
        for (AstConcreteClafer concreteClafer : analysis.getConcreteClafers()) {
            if (concreteClafer.hasParent()) {
                dependency.addEdge(concreteClafer, concreteClafer.getParent());
            }
            AstRef ref = AstUtil.getInheritedRef(concreteClafer);
            if (ref != null && ref.isUnique() && !ref.getTargetType().isPrimitive()) {
                dependency.addEdge(concreteClafer, ref.getTargetType());
            }
        }
        return GraphUtil.computeStronglyConnectedComponents(dependency);
    }

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstClafer, Card> globalCardMap = new HashMap<>();
        globalCardMap.put(IntType, new Card(0, analysis.getScope().getIntHigh() - analysis.getScope().getIntLow() + 1));
        List<Pair<AstClafer, Integer>> insufficientScopes = new ArrayList<>();

        // Step 1. Simple pass.
        Iterable<Set<AstClafer>> order = order(analysis);
        for (Set<AstClafer> component : order) {
            for (AstClafer clafer : component) {
                if (clafer instanceof AstConcreteClafer) {
                    analyze((AstConcreteClafer) clafer, analysis, globalCardMap, insufficientScopes);
                } else {
                    analyze((AstAbstractClafer) clafer, analysis, globalCardMap, insufficientScopes);
                }
            }
        }

        // Step 2. Optimization pass with constraint solver.
        if (insufficientScopes.isEmpty()) {
            Model model = new Model();
            Map<AstClafer, IntVar> lowCards = new HashMap<>();
            Map<AstClafer, IntVar> highCards = new HashMap<>();
            Function<AstClafer, IntVar> getLowCard = clafer -> {
                IntVar lowCard = lowCards.get(clafer);
                if (lowCard == null) {
                    lowCard = model.intVar(clafer.getName() + "@LowCard", globalCardMap.get(clafer).getLow(), globalCardMap.get(clafer).getHigh());
                    lowCards.put(clafer, lowCard);
                }
                return lowCard;
            };
            Function<AstClafer, IntVar> getHighCard = clafer -> {
                IntVar highCard = highCards.get(clafer);
                if (highCard == null) {
                    highCard = model.intVar(clafer.getName() + "@HighCard", globalCardMap.get(clafer).getLow(), globalCardMap.get(clafer).getHigh());
                    highCards.put(clafer, highCard);
                }
                return highCard;
            };
            IntVar[] intVars = new IntVar[2 * analysis.getClafers().size()];
            int i = 0;
            for (AstClafer clafer : analysis.getClafers()) {
                IntVar lowCard = getLowCard.apply(clafer);
                IntVar highCard = getHighCard.apply(clafer);
                intVars[i++] = lowCard;
                intVars[i++] = highCard;
                for (AstConcreteClafer child : clafer.getChildren()) {
                    IntVar childLowCard = getLowCard.apply(child);
                    IntVar childHighCard = getHighCard.apply(child);
                    Card card = analysis.getCard(child);
                    if (card.hasLow()) {
                        model.times(lowCard, card.getLow(), childLowCard).post();
                        model.arithm(childHighCard, ">=", model.intScaleView(highCard, card.getLow())).post();
                    }
                    if (card.hasHigh()) {
                        model.arithm(childHighCard, "<=", model.intScaleView(highCard, card.getHigh())).post();
                    }
                }
                if (clafer instanceof AstAbstractClafer) {
                    AstAbstractClafer abstractClafer = (AstAbstractClafer) clafer;
                    IntVar[] subLowCards = abstractClafer.getSubs().stream().map(getLowCard).toArray(IntVar[]::new);
                    IntVar[] subHighCards = abstractClafer.getSubs().stream().map(getHighCard).toArray(IntVar[]::new);
                    model.sum(subLowCards, "=", lowCard).post();
                    model.sum(subHighCards, "=", highCard).post();
                }
            }
            for (Entry<AstClafer, AstClafer> inverse : analysis.getInverseMap().entrySet()) {
                model.arithm(lowCards.get(inverse.getKey()), "=", lowCards.get(inverse.getValue())).post();
                model.arithm(highCards.get(inverse.getKey()), "=", highCards.get(inverse.getValue())).post();
            }
            model.getSolver().limitTime(5000);
            AbstractStrategy<IntVar> strategy = Search.domOverWDegSearch(intVars);

            // Optimize each global cardinality individually.
            for (Set<AstClafer> component : order) {
                for (AstClafer clafer : component) {
                    if (clafer instanceof AstConcreteClafer) {
                        AstConcreteClafer concreteClafer = (AstConcreteClafer) clafer;
                        if (concreteClafer.getCard().isExact()) {
                            if (concreteClafer.hasParent()) {
                                Card parentCard = globalCardMap.get(concreteClafer.getParent());
                                Card globalCard = parentCard.mult(concreteClafer.getCard());
                                globalCardMap.put(clafer, new Card(globalCard.getLow(), globalCard.getHigh()));
                            }
                        } else {
                            IntVar lowCard = lowCards.get(clafer);
                            IntVar highCard = highCards.get(clafer);

                            // We can optimize the low and high card at once since they do not affect each other.
                            IntVar objective = model.intVar("Objective@" + clafer.getName(), highCard.getLB() - lowCard.getUB(), highCard.getUB() - lowCard.getLB());
                            model.post(Constraints.equalArcConsistent(highCard, model.intMinusView(lowCard), objective));

                            model.getSolver().setSearch(Search.inputOrderUBSearch(objective), strategy);
                            Solution solution = model.getSolver().findOptimalSolution(objective, true);

                            if (solution == null) {
                                break;
                            }
                            assert solution.getIntVal(lowCard) >= globalCardMap.get(clafer).getLow();
                            assert solution.getIntVal(highCard) <= globalCardMap.get(clafer).getHigh();
                            globalCardMap.put(clafer, new Card(solution.getIntVal(lowCard), solution.getIntVal(highCard)));

                            model.getSolver().reset();
                        }
                    } else {
                        analyze((AstAbstractClafer) clafer, analysis, globalCardMap, insufficientScopes);
                    }
                }
            }
        }

        // Step 3. Check if mandatory references to an insufficently sized set.
        for (AstClafer clafer : analysis.getClafers()) {
            if (clafer.hasRef()) {
                AstRef ref = clafer.getRef();
                if (!ref.getTargetType().isPrimitive()) {
                    int lowCard = maxLowCard(ref.getSourceType(), globalCardMap);
                    int required = ref.isUnique() ? lowCard : Math.min(1, lowCard);
                    Card targetCard = globalCardMap.get(ref.getTargetType());
                    assert targetCard.hasHigh();
                    if (targetCard.getHigh() < required) {
                        insufficientScopes.add(new Pair<>(ref.getTargetType(), required));
                    }
                }
            }
        }

        if (!insufficientScopes.isEmpty()) {
            throw new InsufficientScopeException(insufficientScopes);
        }
        return analysis.setGlobalCardMap(globalCardMap);
    }

    private static int maxLowCard(AstClafer clafer, Map<AstClafer, Card> globalCardMap) {
        if (!globalCardMap.get(clafer).hasLow()) {
            return 0;
        }
        if (clafer instanceof AstConcreteClafer) {
            return ((AstConcreteClafer) clafer).getCard().getLow();
        }
        AstAbstractClafer abstractClafer = (AstAbstractClafer) clafer;
        int maxLowCard = 0;
        for (AstClafer sub : abstractClafer.getSubs()) {
            maxLowCard = Math.max(maxLowCard, maxLowCard(sub, globalCardMap));
        }
        return maxLowCard;
    }

    private static void analyze(AstAbstractClafer clafer, Analysis analysis,
            Map<AstClafer, Card> globalCardMap, List<Pair<AstClafer, Integer>> insufficientScopes) {
        Card globalCard;
        if (AstUtil.isRoot(clafer)) {
            globalCard = new Card(1, 1);
        } else {
            globalCard = new Card(0, 0);
            for (AstClafer sub : clafer.getSubs()) {
                Card subGlobalCard = globalCardMap.get(sub);
                if (subGlobalCard == null) {
                    // This is possible if a child of an abstract extends the abstract.
                    // Assume the worst possible case.
                    subGlobalCard = new Card(0, analysis.getScope(sub));
                }
                globalCard = globalCard.add(subGlobalCard);
            }
        }
        globalCardMap.put(clafer, globalCard);
    }

    private static void analyze(AstConcreteClafer clafer, Analysis analysis,
            Map<AstClafer, Card> globalCardMap, List<Pair<AstClafer, Integer>> insufficientScopes) {
        Card parentGlobalCard;
        if (!clafer.hasParent()) {
            parentGlobalCard = new Card(1, 1);
        } else {
            parentGlobalCard = globalCardMap.get(clafer.getParent());
            if (parentGlobalCard == null) {
                // Not analyzed yet due to cycle.
                parentGlobalCard = new Card(0, analysis.getScope(clafer.getParent()));
            }
        }
        // Cap by scope
        Card globalCard = parentGlobalCard.mult(getCard(clafer, analysis, globalCardMap));
        int scope = analysis.getScope(clafer);
        if (scope < globalCard.getLow()) {
            insufficientScopes.add(new Pair<>(clafer, globalCard.getLow()));
            globalCard = new Card(0, scope);
        } else {
            globalCard = new Card(
                    globalCard.getLow(),
                    Math.min(globalCard.getHigh(), scope));
        }
        globalCardMap.put(clafer, globalCard);
    }

    private static Card getCard(AstConcreteClafer clafer, Analysis analysis, Map<AstClafer, Card> globalCardMap) {
        Card card = analysis.getCard(clafer);
        AstRef ref = AstUtil.getInheritedRef(clafer);
        if (ref != null && ref.isUnique()) {
            Card targetCard = globalCardMap.get(ref.getTargetType());
            // targetCard can be null if cycle.
            if (targetCard != null && targetCard.getHigh() < card.getHigh()) {
                if (card.getLow() <= targetCard.getHigh()) {
                    return new Card(card.getLow(), targetCard.getHigh());
                }
            }
        }
        return card;
    }
}
