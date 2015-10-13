package org.clafer.javascript;

import org.clafer.assertion.Assertion;
import org.clafer.ast.AstModel;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;

/**
 *
 * @author jimmy
 */
public class JavascriptFile {

    private final AstModel model;
    private final Scope scope;
    private final Objective[] objectives;
    private final Assertion[] assertions;

    public JavascriptFile(AstModel model, Scope scope, Objective[] objectives, Assertion[] assertions) {
        this.model = model;
        this.scope = scope;
        this.objectives = objectives;
        this.assertions = assertions;
    }

    public AstModel getModel() {
        return model;
    }

    public Scope getScope() {
        return scope;
    }

    public Objective[] getObjectives() {
        return objectives;
    }

    public Assertion[] getAssertions() {
        return assertions;
    }
}
