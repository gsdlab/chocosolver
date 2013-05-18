package org.clafer.javascript;

import java.util.Set;
import org.clafer.Check;
import org.clafer.ast.AstClafer;
import org.clafer.collection.Pair;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferObjective;
import org.clafer.compiler.ClaferSolver;
import org.clafer.compiler.ClaferUnsat;
import org.clafer.instance.InstanceModel;

/**
 *
 * @author jimmy
 */
public class RhinoShellContext {

    private final RhinoContext context;
    private ClaferSolver solver;

    public RhinoShellContext(RhinoContext context) {
        this.context = Check.notNull(context);
    }

    public InstanceModel solve() {
        if (solver == null) {
            solver = ClaferCompiler.compile(context.getModel(), context.getScope());
        }
        return solver.find() ? solver.instance() : null;
    }

    public InstanceModel minimize(AstClafer clafer) {
        if (!clafer.hasRef()) {
            throw new IllegalArgumentException("Cannot minimize " + clafer);
        }
        ClaferObjective objective = ClaferCompiler.compileMinimize(context.getModel(), context.getScope(), clafer.getRef());
        return objective.optimal().getSnd();

    }

    public Pair<Set<String>, InstanceModel> minUnsat() {
        ClaferUnsat unsat = ClaferCompiler.compileUnsat(context.getModel(), context.getScope());
        return unsat.minUnsat();
    }
}
