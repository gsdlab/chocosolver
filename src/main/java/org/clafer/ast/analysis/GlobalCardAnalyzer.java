package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import org.chocosolver.solver.ResolutionPolicy;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.objective.ObjectiveManager;
import org.chocosolver.solver.search.loop.monitors.IMonitorSolution;
import org.chocosolver.solver.search.loop.monitors.SMF;
import org.chocosolver.solver.search.solution.Solution;
import org.chocosolver.solver.search.strategy.ISF;
import org.chocosolver.solver.search.strategy.strategy.AbstractStrategy;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.VF;
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
        globalCardMap.put(analysis.getModel(), new Card(1, 1));
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
            Solver solver = new Solver();
            Map<AstClafer, IntVar> lowCards = new HashMap<>();
            Map<AstClafer, IntVar> highCards = new HashMap<>();
            Function<AstClafer, IntVar> getLowCard = clafer -> {
                IntVar lowCard = lowCards.get(clafer);
                if (lowCard == null) {
                    lowCard = VF.enumerated(clafer.getName() + "@LowCard", globalCardMap.get(clafer).getLow(), globalCardMap.get(clafer).getHigh(), solver);
                    lowCards.put(clafer, lowCard);
                }
                return lowCard;
            };
            Function<AstClafer, IntVar> getHighCard = clafer -> {
                IntVar highCard = highCards.get(clafer);
                if (highCard == null) {
                    highCard = VF.enumerated(clafer.getName() + "@HighCard", globalCardMap.get(clafer).getLow(), globalCardMap.get(clafer).getHigh(), solver);
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
                        solver.post(ICF.times(lowCard, card.getLow(), childLowCard));
                        solver.post(ICF.arithm(childHighCard, ">=", VF.scale(highCard, card.getLow())));
                    }
                    if (card.hasHigh()) {
                        solver.post(ICF.arithm(childHighCard, "<=", VF.scale(highCard, card.getHigh())));
                    }
                }
                if (clafer instanceof AstAbstractClafer) {
                    AstAbstractClafer abstractClafer = (AstAbstractClafer) clafer;
                    IntVar[] subLowCards = abstractClafer.getSubs().stream().map(getLowCard).toArray(x -> new IntVar[x]);
                    IntVar[] subHighCards = abstractClafer.getSubs().stream().map(getHighCard).toArray(x -> new IntVar[x]);
                    solver.post(ICF.sum(subLowCards, lowCard));
                    solver.post(ICF.sum(subHighCards, highCard));
                }
            }
            SMF.limitTime(solver, 5000);
            AbstractStrategy<IntVar> strategy = ISF.domOverWDeg(intVars, 0);
            Solution solution = new Solution();
            solver.plugMonitor((IMonitorSolution) () -> solution.record(solver));

            // Optimize each global cardinality individually.
            for (Set<AstClafer> component : order) {
                for (AstClafer clafer : component) {
                    if (clafer instanceof AstConcreteClafer) {
                        AstConcreteClafer concreteClafer = (AstConcreteClafer) clafer;
                        if (concreteClafer.getCard().isExact()) {
                            if (concreteClafer.hasParent()) {
                                Card parentCard = globalCardMap.get(concreteClafer.getParent());
                                globalCardMap.put(clafer, parentCard.mult(concreteClafer.getCard()));
                            }
                        } else {
                            IntVar lowCard = lowCards.get(clafer);
                            IntVar highCard = highCards.get(clafer);

                            // We can optimize the low and high card at once since they do not affect each other.
                            IntVar objective = VF.enumerated("Objective@" + clafer.getName(), highCard.getLB() - lowCard.getUB(), highCard.getUB() - lowCard.getLB(), solver);
                            solver.post(Constraints.equalArcConsistent(highCard, VF.minus(lowCard), objective));

                            solver.set(new ObjectiveManager<>(objective, ResolutionPolicy.MAXIMIZE, true));
                            solver.set(ISF.lexico_UB(objective), strategy);

                            if (solver.findAllSolutions() == 0 || solver.hasReachedLimit()) {
                                break;
                            }
                            assert solution.getIntVal(lowCard) >= globalCardMap.get(clafer).getLow();
                            assert solution.getIntVal(highCard) <= globalCardMap.get(clafer).getHigh();
                            globalCardMap.put(clafer, new Card(solution.getIntVal(lowCard), solution.getIntVal(highCard)));

                            solver.getEngine().flush();
                            solver.getSearchLoop().reset();
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
        Card globalCard = new Card(0, 0);
        for (AstClafer sub : clafer.getSubs()) {
            Card subGlobalCard = globalCardMap.get(sub);
            if (subGlobalCard == null) {
                // This is possible if a child of an abstract extends the abstract.
                // Assume the worst possible case.
                subGlobalCard = new Card(0, analysis.getScope(sub));
            }
            globalCard = globalCard.add(subGlobalCard);
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
