package org.clafer.choco.constraint.propagator;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import gnu.trove.map.TIntObjectMap;
import gnu.trove.map.hash.TIntObjectHashMap;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.util.ESat;
import org.clafer.collection.Counter;
import org.jgrapht.alg.util.UnionFind;

import java.util.*;

/**
 *
 * @author ed
 */
public class PropConnected extends Propagator<SetVar> {

    private static final long serialVersionUID = 1L;

    private final SetVar nodes;
    private final SetVar[] edges;
    private final boolean directed;

    public PropConnected(SetVar nodes, SetVar[] edges, boolean directed) {
        super(buildArray(nodes, edges), PropagatorPriority.CUBIC, false);
        if(directed) {
            System.out.println("Directed graphs not implemented.");
            System.exit(1);
        }
        this.nodes = nodes;
        this.edges = edges;
        this.directed = directed;
    }

    private static SetVar[] buildArray(SetVar nodes, SetVar[] edges) {
        SetVar[] array = new SetVar[edges.length + 1];
        array[0] = nodes;
        System.arraycopy(edges, 0, array, 1, edges.length);
        return array;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        /*
        for (SetVar var : edges) {
            for (int i = var.getEnvelopeFirst(); i != SetVar.END; i = var.getEnvelopeNext()) {
                if (i < 0 || i >= edges.length) {
                    var.removeFromEnvelope(i, aCause);
                }
            }
        }
        */

        TIntSet[] weaklyConnectedComponents = weaklyConnectedComponents(edges);
        if(weaklyConnectedComponents.length > 1) {
            throw new ContradictionException();
        }
        else
            return;
    /*
        for (int i = 0; i < edges.length; i++) {
            SetVar var = edges[i];
            TIntSet reachable = maximalClosure[i];
            for (int k = var.getEnvelopeFirst(); k != SetVar.END; k = var.getEnvelopeNext()) {
                if ((!directed || i != k) && !reachable.contains(k)) {
                    var.removeFromEnvelope(k, aCause);
                }
            }
        }
    */
    }

    @Override
    public ESat isEntailed() {
        /*
        for (SetVar var : edges) {
            for (int i = var.getKernelFirst(); i != SetVar.END; i = var.getKernelNext()) {
                if (i < 0 || i >= edges.length) {
                    return ESat.FALSE;
                }
            }
        }
        */

        TIntSet[] weaklyConnectedComponents = weaklyConnectedComponents(edges);
        //TODO Temp hack
        if(weaklyConnectedComponents.length > 1) {
            return ESat.FALSE;
        }
        else
            return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    /*
        for (int i = 0; i < edges.length; i++) {
            SetVar var = edges[i];
            TIntSet reachable = maximalClosure[i];
            for (int k = var.getKernelFirst(); k != SetVar.END; k = var.getKernelNext()) {
                if ((!directed || i != k) && !reachable.contains(k)) {
                    return ESat.FALSE;
                }
            }
        }

        for (int i = 0; i < edges.length; i++) {
            SetVar var = edges[i];
            TIntSet reachable = weaklyConnectedComponents[i];
            for (int k = nodes.getKernelFirst(); k != SetVar.END; k = nodes.getKernelNext()) {
                if (!reachable.contains(k)) {
                    return ESat.FALSE;
                }
            }
        }
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
        */
    }

    @Override
    public String toString() {
        return "connected(" + Arrays.toString(edges) + ", " + nodes + ")";
    }


    private static TIntSet[] weaklyConnectedComponents(SetVar[] relation){
        HashSet<Integer> elems = new HashSet<Integer>();
        for(int i = 0; i < relation.length; i++)
            elems.add(Integer.valueOf(i));
        UnionFind<Integer> uf = new UnionFind<Integer>(new HashSet<Integer>(elems));
        for(int i = 0; i < relation.length; i++)
            for(int k = relation[i].getEnvelopeFirst(); k != SetVar.END; k = relation[i].getEnvelopeNext())
                if(k != relation.length)
                    uf.union(Integer.valueOf(i), Integer.valueOf(k));

        boolean[] seen = new boolean[relation.length];
        //probably should clean up..
        ArrayList<TIntHashSet> components = new ArrayList<TIntHashSet>();
        for(int i = 0; i < relation.length; i++)
            components.add(new TIntHashSet(relation.length));
        for(int i = 0; i < relation.length; i++){
            int rep = uf.find(i);
            if(!seen[rep]){
                seen[rep] = true;
            }
            components.get(rep).add(i);
        }
        Iterator<TIntHashSet> itr = components.iterator();
        while(itr.hasNext())
        {
            TIntHashSet s = itr.next();
            if(s.size() == 0){
                itr.remove();
            }
        }
        return (TIntHashSet[])components.toArray(new TIntHashSet[components.size()]);
    }




    private static TIntSet[] maximalTransitiveClosure(SetVar[] relation) {
        List<TIntSet> components = computeStronglyConnectedComponents(relation);
        TIntSet[] maximalClosure = new TIntSet[relation.length];

        for (TIntSet component : components) {
            TIntSet reachable = new TIntHashSet(relation.length);

            TIntIterator iter = component.iterator();
            while (iter.hasNext()) {
                int val = iter.next();
                reachable.add(val);
                SetVar var = relation[val];
                assert maximalClosure[val] == null;
                maximalClosure[val] = reachable;
                for (int i = var.getEnvelopeFirst(); i != SetVar.END; i = var.getEnvelopeNext()) {
                    if (i >= 0 && i < relation.length) {
                        reachable.add(i);
                        TIntSet reach = maximalClosure[i];
                        if (reach != null) {
                            reachable.addAll(reach);
                        }
                    }
                }
            }
        }
        return maximalClosure;
    }

    private static List<TIntSet> computeStronglyConnectedComponents(SetVar[] relation) {
        Counter counter = new Counter();
        TIntObjectMap<Index> vertexIndices = new TIntObjectHashMap<>(relation.length);
        TIntList S = new TIntArrayList();
        List<TIntSet> components = new ArrayList<>();

        for (int vertex = 0; vertex < relation.length; vertex++) {
            if (!vertexIndices.containsKey(vertex)) {
                strongConnect(relation, vertex, counter, vertexIndices, S, components);
            }
        }
        return components;
    }

    private static Index strongConnect(SetVar[] relation, int vertex, Counter counter,
            TIntObjectMap<Index> vertexIndices, TIntList S, List<TIntSet> components) {
        int index = counter.next();
        Index vertexIndex = new Index(index, index);
        vertexIndices.put(vertex, vertexIndex);

        S.add(vertex);

        SetVar var = relation[vertex];
        for (int neighbour = var.getEnvelopeFirst(); neighbour != SetVar.END; neighbour = var.getEnvelopeNext()) {
            if (neighbour >= 0 && neighbour < relation.length) {
                Index neighbourIndex = vertexIndices.get(neighbour);
                if (neighbourIndex == null) {
                    neighbourIndex = strongConnect(relation, neighbour, counter, vertexIndices, S, components);
                    vertexIndex.setLowIndexMin(neighbourIndex.getLowIndex());
                } else if (S.contains(neighbour)) {
                    vertexIndex.setLowIndexMin(neighbourIndex.getIndex());
                }
            }
        }

        if (vertexIndex.getLowIndex() == vertexIndex.getIndex()) {
            TIntSet component = new TIntHashSet();

            int cycle;
            do {
                cycle = S.removeAt(S.size() - 1);
                component.add(cycle);
            } while (cycle != vertex);

            components.add(component);
        }
        return vertexIndex;
    }

    private static class Index {

        private final int index;
        private int lowIndex;

        Index(int index, int lowIndex) {
            this.index = index;
            this.lowIndex = lowIndex;
        }

        int getIndex() {
            return index;
        }

        int getLowIndex() {
            return lowIndex;
        }

        void setLowIndexMin(int lowIndex) {
            if (this.lowIndex >= lowIndex) {
                this.lowIndex = lowIndex;
            }
        }
    }
}
