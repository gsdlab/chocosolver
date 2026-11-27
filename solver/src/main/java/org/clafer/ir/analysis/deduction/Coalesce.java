package org.clafer.ir.analysis.deduction;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrRewriter;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringVar;
import static org.clafer.ir.Irs.string;

/**
 *
 * @author jimmy
 */
public class Coalesce extends IrRewriter<Void> {

    private final Map<IrIntVar, IrIntVar> coalescedInts;
    private final Map<IrSetVar, IrSetVar> coalescedSets;
    private final Map<List<IrIntVar>, IrStringVar> stringVarCache = new HashMap<>();

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

    public void forEachIntVar(BiConsumer<? super IrIntVar, ? super IrIntVar> action) {
        coalescedInts.forEach(action);
    }

    public void forEachSetVar(BiConsumer<? super IrSetVar, ? super IrSetVar> action) {
        coalescedSets.forEach(action);
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

    @Override
    public IrBoolVar visit(IrBoolVar ir, Void a) {
        IrBoolVar var = (IrBoolVar) coalescedInts.get(ir);
        return var == null ? ir : var;
    }

    @Override
    public IrIntVar visit(IrIntVar ir, Void a) {
        IrIntVar var = coalescedInts.get(ir);
        return var == null ? ir : var;
    }

    @Override
    public IrSetVar visit(IrSetVar ir, Void a) {
        IrSetVar var = coalescedSets.get(ir);
        return var == null ? ir : var;
    }

    @Override
    public IrStringVar visit(IrStringVar ir, Void a) {
        boolean changed = false;
        IrIntVar[] chars = new IrIntVar[ir.getCharVars().length];
        for (int i = 0; i < chars.length; i++) {
            chars[i] = coalescedInts.get(ir.getCharVars()[i]);
            if (chars[i] == null) {
                chars[i] = ir.getCharVars()[i];
            } else {
                changed = true;
            }
        }
        IrIntVar length = coalescedInts.get(ir.getLengthVar());
        changed |= length != null;
        if (changed) {
            length = length == null ? ir.getLengthVar() : length;
            List<IrIntVar> key = new ArrayList<>();
            key.addAll(Arrays.asList(chars));
            key.add(length);
            IrStringVar string = stringVarCache.get(key);
            if (string == null) {
                string = string(ir.getName(), chars, length);
                stringVarCache.put(key, string);
            }
            return string;
        }
        return ir;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        coalescedInts.entrySet().stream()
                .collect(Collectors.groupingBy(Entry::getValue, Collectors.mapping(Entry::getKey, Collectors.toList())))
                .forEach((newVar, oldVars) -> builder.append(newVar).append(" <--- ").append(oldVars).append('\n'));
        coalescedSets.entrySet().stream()
                .collect(Collectors.groupingBy(Entry::getValue, Collectors.mapping(Entry::getKey, Collectors.toList())))
                .forEach((newVar, oldVars) -> builder.append(newVar).append(" <--- ").append(oldVars).append('\n'));
        return builder.toString();
    }
}
