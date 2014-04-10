package org.clafer.test;

import java.lang.annotation.Annotation;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author jimmy
 */
public class Annotations {
    private Map<Class<? extends Annotation>, Annotation> map;

    public boolean hasAnnotation(Class<? extends Annotation> annotation) {
        return map != null && map.containsKey(annotation);
    }

    public <T extends Annotation> T getAnnotation(Class<T> annotation) {
        if (map == null) {
            return null;
        }
        @SuppressWarnings(value = "unchecked")
        T instance = (T) map.get(annotation);
        return instance;
    }

    public <T extends Annotation> void addAnnotation(T annotation) {
        if (map == null) {
            map = new HashMap<>(1);
        }
        map.put(annotation.annotationType(), annotation);
    }

    @SafeVarargs
    public final <T extends Annotation> void addAnnotations(T... annotations) {
        for (T annotation : annotations) {
            addAnnotation(annotation);
        }
    }

    @Override
    public String toString() {
        return map == null ? "{}" : map.toString();
    }

}
