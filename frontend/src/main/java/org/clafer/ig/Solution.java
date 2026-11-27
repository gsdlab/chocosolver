package org.clafer.ig;

import java.util.List;

/**
 * A solution of a model, claferIG's {@code ClaferSolution}: an instance of a
 * static model, or a trace of a dynamic model, whose states are instances,
 * with the state to which its last state loops back.
 */
public sealed interface Solution {

    /**
     * @return the instance, or the states of the trace
     */
    List<ClaferModel> models();

    /**
     * An instance, with its text as its backend prints it, if the backend
     * prints it otherwise than claferIG ({@code null} for claferIG's text).
     */
    record Instance(ClaferModel model, String text) implements Solution {

        public Instance(ClaferModel model) {
            this(model, null);
        }

        @Override
        public List<ClaferModel> models() {
            return List.of(model);
        }
    }

    record Trace(List<ClaferModel> states, int loopState) implements Solution {

        public Trace {
            states = List.copyOf(states);
        }

        @Override
        public List<ClaferModel> models() {
            return states;
        }
    }
}
