package org.clafer.ir.analysis.deduction;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;

/**
 *
 * @author jimmy
 */
public class Coalesce {

    private final Map<IrIntVar, IrIntVar> coalescedInts;
    private final Map<IrSetVar, IrSetVar> coalescedSets;

    public Coalesce() {
        this.coalescedInts = Collections.emptyMap();
        this.coalescedSets = Collections.emptyMap();
    }

    public Coalesce(
            Map<IrIntVar, IrIntVar> coalescedInts,
            Map<IrSetVar, IrSetVar> coalescedSets) {
        this.coalescedInts = coalescedInts;
        this.coalescedSets = coalescedSets;
    }

    public IrBoolVar get(IrBoolVar var) {
        return (IrBoolVar) coalescedInts.getOrDefault(var, var);
    }

    public IrIntVar get(IrIntVar var) {
        return coalescedInts.getOrDefault(var, var);
    }

    public IrSetVar get(IrSetVar var) {
        return coalescedSets.getOrDefault(var, var);
    }

    public boolean isEmpty() {
        return coalescedInts.isEmpty() && coalescedSets.isEmpty();
    }

    public Coalesce compose(Coalesce other) {
        return new Coalesce(
                compose(coalescedInts, other.coalescedInts),
                compose(coalescedSets, other.coalescedSets));
    }

    private static <T> Map<T, T> compose(Map<T, T> f1, Map<T, T> f2) {
        if (f1.isEmpty()) {
            return f2;
        }
        if (f2.isEmpty()) {
            return f1;
        }
        Map<T, T> composed = new HashMap<>(f1.size() + f2.size());
        composed.putAll(f2);
        for (Map.Entry<T, T> e : f1.entrySet()) {
            T key = e.getKey();
            T value = f2.get(e.getValue());
            if (value == null) {
                value = e.getValue();
            }
            composed.put(key, value);
        }
        return composed;
    }
}
