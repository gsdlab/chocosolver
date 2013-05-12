package solver.variables.delta.monitor;

import java.util.Arrays;

/**
 *
 * @author jimmy
 */
public class Sneak {

    public static void print(SetDeltaMonitor monitor) {
        System.out.println("First: " + Arrays.toString(monitor.first));
        System.out.println("Frozen First: " + Arrays.toString(monitor.frozenFirst));
        System.out.println("Last: " + Arrays.toString(monitor.last));
        System.out.println("Frozen Last: " + Arrays.toString(monitor.frozenLast));
        System.out.println("Delta: " + monitor.delta.getSize(0));
        System.out.println("Delta: " + monitor.delta.getSize(1));
    }
}
