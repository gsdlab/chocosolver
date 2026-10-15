package org.clafer.javascript;

import org.clafer.assertion.Assertion;
import org.clafer.ast.AstModel;
import org.clafer.compiler.ClaferOption;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;

/**
 *
 * @author jimmy
 */
public class JavascriptFile {

    private final AstModel model;
    private final Scope scope;
    private final ClaferOption option;
    private final Objective[] objectives;
    private final Assertion[] assertions;
    private final boolean hasIntRange;

    public JavascriptFile(AstModel model, Scope scope, ClaferOption option, Objective[] objectives, Assertion[] assertions) {
        this(model, scope, option, objectives, assertions, false);
    }

    public JavascriptFile(AstModel model, Scope scope, ClaferOption option, Objective[] objectives, Assertion[] assertions, boolean hasIntRange) {
        this.model = model;
        this.scope = scope;
        this.option = option;
        this.objectives = objectives;
        this.assertions = assertions;
        this.hasIntRange = hasIntRange;
    }

    public AstModel getModel() {
        return model;
    }

    public Scope getScope() {
        return scope;
    }

    public ClaferOption getOption() {
        return option;
    }

    public Objective[] getObjectives() {
        return objectives;
    }

    public Assertion[] getAssertions() {
        return assertions;
    }

    /**
     * @return whether the model sets its integer range (intRange), so the scope's
     * integer range is not the default
     */
    public boolean hasIntRange() {
        return hasIntRange;
    }
}
