package org.clafer.ir.analysis.deduction;

import java.util.Arrays;
import org.clafer.ir.IrAcyclic;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrUtil;

/**
 *
 * @author jimmy
 */
class AcyclicDeducer implements BoolDeducer<IrAcyclic> {

    @Override
    public void deduce(IrAcyclic ir, Deduction deduction) {
        IrIntExpr[] edges = ir.getEdges();
        int[] visited = new int[edges.length];
        Arrays.fill(visited, -1);
        for (int i = 0; i < edges.length; i++) {
            int cur = i;
            deduction.notEqual(edges[cur], i);
            while (visited[cur] != i && edges[cur].getDomain().isConstant()) {
                visited[cur] = i;
                cur = IrUtil.getConstant(edges[cur]);
                if (cur < 0 || cur >= edges.length) {
                    break;
                }
                deduction.notEqual(edges[cur], i);
            }
        }
    }
}
