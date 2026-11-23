package org.clafer.compiler;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 * The default search, PreferSmallerInstances.
 *
 * @author Michał Antkiewicz
 */
public class SearchStrategyTest {

    private static JavascriptFile model(String name) throws IOException {
        try (InputStream in = SearchStrategyTest.class.getResourceAsStream(name)) {
            return Javascript.readModel(new String(in.readAllBytes(), StandardCharsets.UTF_8));
        }
    }

    /**
     * failureRateBased broke its ties at random, also its first decisions, before any
     * failure, and on this model it went into a subtree without instances: no instance after
     * minutes, with minDomLB one in 44 nodes (gsdlab/chocosolver#46). The model is a part of
     * the wiki model ClaferTools/ClaferMooVizAbstractState, with 5 products.
     */
    @Test(timeout = 60000)
    public void testFirstInstanceOfBubbles() throws Exception {
        JavascriptFile js = model("bubbles.js");
        // the default integer range of the command line (see Utils.resolveScopes)
        Scope scope = js.getScope().toBuilder().intLow(-128).intHigh(127).toScope();
        ClaferSearch search = ClaferCompiler.compile(js.getModel(), scope, js.getOption());
        search.limitTime(30000);
        assertTrue(search.find());
    }
}
