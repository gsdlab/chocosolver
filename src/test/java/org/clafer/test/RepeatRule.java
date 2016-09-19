package org.clafer.test;

import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

/**
 *
 * @author jimmy
 */
public class RepeatRule implements TestRule {

    private static class RepeatStatement extends Statement {

        private final Statement statement;
        private final int repeat;

        public RepeatStatement(Statement statement, int repeat) {
            this.statement = statement;
            this.repeat = repeat;
        }

        @Override
        public void evaluate() throws Throwable {
            for (int i = 0; i < repeat; i++) {
                statement.evaluate();
            }
        }

    }

    private final int repeat;

    public RepeatRule(int repeat) {
        this.repeat = repeat;
    }

    @Override
    public Statement apply(Statement statement, Description description) {
        return repeat > 1 ? new RepeatStatement(statement, repeat) : statement;
    }
}
