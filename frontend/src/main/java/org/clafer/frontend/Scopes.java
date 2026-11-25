package org.clafer.frontend;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * The scopes that chocosolver solves an explicit model with, as the compiler
 * writes them to the explicit model's scope file ({@code clafer -m clafer
 * --meta-data}, {@code model.des.cfr-scope}).
 *
 * @param clafers the scope of every clafer, by UID, sorted, the enumerations
 * and their literals included
 * @param intLow the smallest integer, {@code --minint} or -128
 * @param intHigh the largest integer, {@code --maxint} or 127
 * @param stringLength the length of the longest string literal, with its
 * quotes, at least 16
 * @param decimalDigits the number of fractional digits of decimals, 0 if the
 * model has none and no {@code --decimal-digits}
 */
public record Scopes(Map<String, Long> clafers, long intLow, long intHigh, int stringLength, long decimalDigits) {

    /**
     * chocosolver's largest decimal, {@code Integer.MAX_VALUE / 100 - 1},
     * scaled.
     */
    private static final long MaxScaledDecimal = 21474835;

    /**
     * @return the scope file, as the compiler writes it
     * ({@code Explicit.explicitScopes})
     */
    public String scopeFile() {
        List<String> entries = new ArrayList<>();
        clafers.forEach((uid, scope) -> entries.add("{\"lpqName\": \"" + uid + "\", \"scope\": " + scope + "}"));
        entries.add("{\"defaultScope\": 1}");
        entries.add("{\"int\": [" + intLow + ", " + intHigh + "]}");
        entries.add("{\"mul\": [" + intLow + ", " + intHigh + "]}");
        entries.add("{\"stringLength\": " + stringLength + "}");
        entries.add("{\"chars\": [32, 126]}");
        entries.add("{\"decimalDigits\": " + decimalDigits + "}");
        entries.add("{\"decimal\": [" + unscaled(-MaxScaledDecimal) + ", " + unscaled(MaxScaledDecimal) + "]}");
        return "[\n" + String.join(",\n", entries) + "\n]\n";
    }

    /**
     * A scaled decimal as written, e.g., 214748.35 for 21474835 with 2
     * digits.
     */
    private String unscaled(long n) {
        String digits = Long.toString(Math.abs(n));
        int scale = (int) decimalDigits;
        String padded = "0".repeat(Math.max(0, scale + 1 - digits.length())) + digits;
        String whole = padded.substring(0, padded.length() - scale);
        String fraction = padded.substring(padded.length() - scale);
        return (n < 0 ? "-" : "") + whole + (scale > 0 ? "." + fraction : "");
    }
}
