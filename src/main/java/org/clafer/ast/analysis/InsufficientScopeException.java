package org.clafer.ast.analysis;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstException;
import org.clafer.ast.AstUtil;
import org.clafer.collection.Pair;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class InsufficientScopeException extends AstException {

    private static final long serialVersionUID = 1L;

    private final Pair<AstClafer, Integer>[] insufficientScopes;

    public InsufficientScopeException(Pair<AstClafer, Integer>[] insufficientScopes) {
        this(false, merge(Arrays.asList(insufficientScopes)));
    }

    public InsufficientScopeException(Collection<Pair<AstClafer, Integer>> insufficientScopes) {
        this(false, merge(insufficientScopes));
    }

    private InsufficientScopeException(boolean flag,
            Pair<AstClafer, Integer>[] insufficientScopes) {
        super(message(insufficientScopes));
        if (insufficientScopes.length == 0) {
            throw new IllegalArgumentException();
        }
        this.insufficientScopes = insufficientScopes.clone();
    }

    private static Pair<AstClafer, Integer>[] merge(
            Iterable<Pair<AstClafer, Integer>> insufficientScopes) {
        Map<AstClafer, Integer> map = new HashMap<>();
        for (Pair<AstClafer, Integer> insufficientScope : insufficientScopes) {
            Integer value = map.get(insufficientScope.getFst());
            if (value == null || value < insufficientScope.getSnd()) {
                map.put(insufficientScope.getFst(), insufficientScope.getSnd());
            }
        }
        return Pair.toPairs(map);
    }

    private static String message(Pair<AstClafer, Integer>[] insufficientScopes) {
        StringBuilder message = new StringBuilder();
        message.append("The provided scope is too low, consider increasing the following scopes:\n\n");
        for (Pair<AstClafer, Integer> insufficientScope : insufficientScopes) {
            message.append("    ").append(insufficientScope.getFst())
                    .append(" to at least ").append(insufficientScope.getSnd());
            if (insufficientScope.getFst() instanceof AstAbstractClafer) {
                List<AstConcreteClafer> subs = AstUtil.getConcreteSubs(insufficientScope.getFst());
                if (subs.isEmpty()) {
                    message.append(" (no concrete subclafers)");
                } else {
                    message.append(" (concrete subclafers are ");
                    message.append(subs);
                    message.append(')');
                }
            }
            message.append('\n');
        }
        return message.toString();
    }

    public Pair<AstClafer, Integer>[] getInsufficientScopes() {
        return insufficientScopes;
    }
}
