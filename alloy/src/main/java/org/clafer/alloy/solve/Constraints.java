package org.clafer.alloy.solve;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.clafer.alloy.AlloyGenerator;
import org.clafer.frontend.ModelFiles;
import org.clafer.frontend.SourceSpan;

/**
 * The constraints of the Alloy code that an unsat core can name, as
 * claferIG's {@code Constraints.hs} has them: the cardinalities of the
 * clafers and the constraints of the model, each at its span in the Alloy
 * code.
 */
public final class Constraints {

    private Constraints() {
    }

    /**
     * A span of the Alloy code, as an unsat core gives it: lines and columns,
     * the end column one past Alloy's.
     */
    public record Range(long line, long column, long endLine, long endColumn) {
    }

    /**
     * A cardinality, {@code upper} -1 for none.
     */
    public record Cardinality(long lower, long upper) {

        @Override
        public String toString() {
            if (upper < 0) {
                return lower == 0 ? "*" : lower == 1 ? "+" : lower + "..*";
            }
            if (lower == 0 && upper == 1) {
                return "?";
            }
            return lower == upper ? Long.toString(lower) : lower + ".." + upper;
        }
    }

    /**
     * A clafer and its cardinality.
     */
    public record ClaferInfo(String uniqueId, Cardinality cardinality) {

        @Override
        public String toString() {
            return uniqueId + " " + cardinality;
        }
    }

    /**
     * A constraint of the model: its span in the complete model, its text,
     * and its position as shown, the file and the position in it
     * ({@code file.cfr:3:5}), and the line in its file.
     */
    public record ConstraintInfo(SourceSpan pos, String syntax, String position, int fileLine) {

        @Override
        public String toString() {
            return syntax + " (" + position + ")";
        }

        /**
         * Whether the constraint spans the given line of its file.
         */
        public boolean isOnLine(long n) {
            return fileLine <= n && n <= fileLine + pos.endLine() - pos.line();
        }
    }

    /**
     * A constraint: a cardinality ({@code claferInfo}) or a constraint of the
     * model ({@code constraintInfo}).
     */
    public sealed interface Constraint {

        Range range();
    }

    public record ExactCardinalityConstraint(Range range, ClaferInfo claferInfo) implements Constraint {
    }

    public record LowerCardinalityConstraint(Range range, ClaferInfo claferInfo) implements Constraint {
    }

    public record UpperCardinalityConstraint(Range range, ClaferInfo claferInfo) implements Constraint {
    }

    public record UserConstraint(Range range, ConstraintInfo constraintInfo) implements Constraint {
    }

    /**
     * {@code parseConstraints}.
     *
     * @param clafers the name and cardinality of each clafer, by its Alloy
     * name
     */
    static List<Constraint> parseConstraints(ModelFiles files, Map<String, ClaferInfo> clafers,
            List<AlloyGenerator.Mapped> mapping) {
        List<Constraint> out = new ArrayList<>();
        List<String> text = text(files);
        for (AlloyGenerator.Mapped m : mapping) {
            Range r = new Range(m.line(), m.column(), m.endLine(), m.endColumn());
            switch (m.kind()) {
                case CONSTRAINT -> out.add(new UserConstraint(r, constraintInfoOf(files, text, m.span())));
                case LOWER_CARD -> out.add(new LowerCardinalityConstraint(r, info(clafers, m.clafer())));
                case UPPER_CARD -> out.add(new UpperCardinalityConstraint(r, info(clafers, m.clafer())));
                case EXACT_CARD -> out.add(new ExactCardinalityConstraint(r, info(clafers, m.clafer())));
            }
        }
        return out;
    }

    private static ClaferInfo info(Map<String, ClaferInfo> clafers, String clafer) {
        ClaferInfo i = clafers.get(clafer);
        if (i == null) {
            throw new IllegalStateException("Unknown clafer " + clafer);
        }
        return i;
    }

    // the columns count a tab up to the next multiple of 8, as the compiler does
    static List<String> text(ModelFiles files) {
        List<String> out = new ArrayList<>();
        String all = files.text();
        int start = 0;
        while (start < all.length()) {
            int end = all.indexOf('\n', start);
            if (end < 0) {
                end = all.length();
            }
            out.add(expandTabs(all.substring(start, end)));
            start = end + 1;
        }
        return out;
    }

    /**
     * The information of a constraint: its text and position in the files of
     * the model.
     */
    static ConstraintInfo constraintInfoOf(ModelFiles files, List<String> text, SourceSpan s) {
        ModelFiles.Position p = files.position(s.line());
        String position = (p.path().isEmpty() ? "" : p.path() + ":") + p.line() + ":" + s.column();
        String last = line(text, s.endLine());
        String rest = drop(last, s.endColumn() - 1);
        return new ConstraintInfo(s, closeParentheses(extract(text, s), rest), position, p.line());
    }

    private static String line(List<String> text, int l) {
        return l - 1 < text.size() ? text.get(l - 1) : "";
    }

    private static String drop(String s, int n) {
        return n >= s.length() ? "" : s.substring(Math.max(n, 0));
    }

    private static String take(String s, int n) {
        return n >= s.length() ? s : s.substring(0, Math.max(n, 0));
    }

    private static String extract(List<String> text, SourceSpan s) {
        if (s.line() == s.endLine()) {
            return drop(take(line(text, s.line()), s.endColumn() - 1), s.column() - 1);
        }
        StringBuilder sb = new StringBuilder(drop(line(text, s.line()), s.column() - 1)).append('\n');
        for (int l = s.line() + 1; l <= s.endLine() - 1; l++) {
            sb.append(line(text, l)).append('\n');
        }
        return sb.append(take(line(text, s.endLine()), s.endColumn() - 1)).append('\n').toString();
    }

    // an expression's span ends before the parentheses that close its last operand, e.g.,
    // `initially (a --> b` of `initially (a --> b)`, which are added
    private static String closeParentheses(String extracted, String rest) {
        while (true) {
            long open = extracted.chars().filter(c -> c == '(').count()
                    - extracted.chars().filter(c -> c == ')').count();
            int spaces = 0;
            while (spaces < rest.length() && rest.charAt(spaces) == ' ') {
                spaces++;
            }
            if (open > 0 && spaces < rest.length() && rest.charAt(spaces) == ')') {
                extracted = extracted + rest.substring(0, spaces) + ")";
                rest = rest.substring(spaces + 1);
            } else {
                return extracted;
            }
        }
    }

    // the line with its tabs as spaces, up to the next multiple of 8
    private static String expandTabs(String line) {
        StringBuilder sb = new StringBuilder();
        for (char ch : line.toCharArray()) {
            if (ch == '\t') {
                int n = 8 - sb.length() % 8;
                sb.append(" ".repeat(n));
            } else {
                sb.append(ch);
            }
        }
        return sb.toString();
    }
}
