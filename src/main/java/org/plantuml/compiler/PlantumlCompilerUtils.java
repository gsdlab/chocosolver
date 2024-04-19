package org.plantuml.compiler;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PlantumlCompilerUtils {
    public static String getPropertyId(String name) {
        Pattern pattern = Pattern.compile("(c[0-9]+_).*", Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(name);
        boolean matchFound = matcher.find();
        //matcher.group()
        if (matchFound) {
            String idxS = matcher.group(1);
            int idx = Integer.parseInt(idxS.substring(1, idxS.length() - 1));
            return name.substring(matcher.group(1).length()) + "_" + String.valueOf(idx);
        } else {
            return name;
        }
    }

    public static String getPropertyAlias(String name) {
        Pattern pattern = Pattern.compile("(c[0-9]+_).*", Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(name);
        boolean matchFound = matcher.find();
        if (matchFound) {
            return name.substring(matcher.group(1).length());
        } else {
            return name;
        }
    }
}