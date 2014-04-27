package org.clafer.test;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 * @param <T> return type
 */
public class Procedure<T> {

    private final Method method;

    public Procedure(Method method) {
        this.method = Check.notNull(method);
    }

    public T invoke(Object object, Object... args) {
        try {
            method.setAccessible(true);
            @SuppressWarnings(value = "unchecked")
            T t = (T) method.invoke(object, args);
            return t;
        } catch (IllegalArgumentException e) {
            StringBuilder argsToString = new StringBuilder();
            for (int i = 0; i < args.length; i++) {
                if (i > 0) {
                    argsToString.append(", ");
                }
                argsToString.append(args[i].getClass());
            }
            throw new Error(method + " cannot be invoked with (" + argsToString + ")", e);
        } catch (IllegalAccessException e) {
            throw new Error(e);
        } catch (InvocationTargetException e) {
            try {
                throw e.getCause();
            } catch (RuntimeException | Error ex) {
                throw ex;
            } catch (Throwable ex) {
                throw new Error(ex);
            }
        }
    }

    public Class<?>[] getParameterTypes() {
        return method.getParameterTypes();
    }

    public Annotations[] getParameterAnnotations() {
        Annotation[][] parameterAnnotations = method.getParameterAnnotations();
        Annotations[] annotations = new Annotations[parameterAnnotations.length];
        for (int i = 0; i < annotations.length; i++) {
            annotations[i] = new Annotations();
            annotations[i].addAnnotations(parameterAnnotations[i]);
        }
        return annotations;
    }

    @Override
    public String toString() {
        return method.toString();
    }
}
