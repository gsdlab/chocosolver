package org.clafer;

import choco.cp.solver.CPSolver;
import choco.kernel.solver.Solver;
import javax.script.ScriptEngine;
import java.io.File;
import java.io.IOException;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import org.clafer.generator.ChocoCompiler;
import org.clafer.generator.Printer;

/**
 *
 * @author jimmy
 */
public class ChocoSolver {

    public static void main(String[] args) throws Exception {
        run();
    }

    public static void run() throws IOException, ScriptException, NoSuchMethodException {
        ScriptEngine engine = new ScriptEngineManager().getEngineByMimeType("application/javascript");
        if (engine == null) {
            throw new IllegalStateException("Missing javascript engine.");
        }

        RhinoContext context = new RhinoContext();
        engine.put("rc", context);

        engine.put(ScriptEngine.FILENAME, "header.js");
        engine.eval(Util.readAll(ChocoSolver.class.getResourceAsStream("header.js")));
        engine.put(ScriptEngine.FILENAME, "Test.js");
        engine.eval(Util.readAll(new File("/home/jimmy/Programming/clafer/Test.js")));

        Solver solver = new CPSolver();

        if (context.hasObjective()) {
            ChocoCompiler compiler = ChocoCompiler.compiler(context.getModel(), context.getScope(), context.getObjective());

            Printer printer = compiler.compileTo(solver);
            if (solver.minimize(false)) {
                System.out.println(printer.printToString());
                System.out.println(solver.runtimeStatistics());
            }
        } else {
            ChocoCompiler compiler = ChocoCompiler.compiler(context.getModel(), context.getScope());

            Printer printer = compiler.compileTo(solver);
            int c = 0;
            if (solver.solve()) {
                do {
                    c++;
                    System.out.println(printer.printToString());
                    System.out.println(solver.runtimeStatistics());
                    if (c == 10) {
                        break;
                    }
                } while (solver.nextSolution());
            }
        }
    }
//    - Solution #1 found. 4843 Time (ms), 76 Nodes, 0 Backtracks, 0 Restarts.
//- Search completed
//  Solutions: 1
//  Time (ms): 4917
//  Nodes: 76
//  Backtracks: 0
//  Restarts: 0
//- Solution #2 found. 4917 Time (ms), 77 Nodes, 1 Backtracks, 0 Restarts.
//- Solution #3 found. 4917 Time (ms), 79 Nodes, 3 Backtracks, 0 Restarts.
//- Solution #4 found. 4917 Time (ms), 80 Nodes, 4 Backtracks, 0 Restarts.
//- Solution #5 found. 5043 Time (ms), 83 Nodes, 7 Backtracks, 0 Restarts.
//- Solution #6 found. 5043 Time (ms), 84 Nodes, 8 Backtracks, 0 Restarts.
//- Solution #7 found. 5043 Time (ms), 86 Nodes, 10 Backtracks, 0 Restarts.
//- Solution #8 found. 5043 Time (ms), 87 Nodes, 11 Backtracks, 0 Restarts.
//- Solution #9 found. 5043 Time (ms), 91 Nodes, 15 Backtracks, 0 Restarts.
//- Solution #10 found. 5043 Time (ms), 92 Nodes, 16 Backtracks, 0 Restarts.
//- Solution #11 found. 5043 Time (ms), 94 Nodes, 18 Backtracks, 0 Restarts.
//- Solution #12 found. 5043 Time (ms), 95 Nodes, 19 Backtracks, 0 Restarts.
//- Solution #13 found. 5043 Time (ms), 98 Nodes, 22 Backtracks, 0 Restarts.
//- Solution #14 found. 5043 Time (ms), 99 Nodes, 23 Backtracks, 0 Restarts.
//- Solution #15 found. 5043 Time (ms), 101 Nodes, 25 Backtracks, 0 Restarts.
//- Solution #16 found. 5247 Time (ms), 102 Nodes, 26 Backtracks, 0 Restarts.
//- Solution #17 found. 5247 Time (ms), 107 Nodes, 31 Backtracks, 0 Restarts.
//- Solution #18 found. 5247 Time (ms), 108 Nodes, 32 Backtracks, 0 Restarts.
//- Solution #19 found. 5247 Time (ms), 110 Nodes, 34 Backtracks, 0 Restarts.
//- Solution #20 found. 5247 Time (ms), 111 Nodes, 35 Backtracks, 0 Restarts.
//- Solution #21 found. 5247 Time (ms), 114 Nodes, 38 Backtracks, 0 Restarts.
//- Solution #22 found. 5247 Time (ms), 115 Nodes, 39 Backtracks, 0 Restarts.
//- Solution #23 found. 5247 Time (ms), 117 Nodes, 41 Backtracks, 0 Restarts.
//- Solution #24 found. 5247 Time (ms), 118 Nodes, 42 Backtracks, 0 Restarts.
//- Solution #25 found. 5247 Time (ms), 122 Nodes, 46 Backtracks, 0 Restarts.
//- Solution #26 found. 5447 Time (ms), 123 Nodes, 47 Backtracks, 0 Restarts.
//- Solution #27 found. 5447 Time (ms), 125 Nodes, 49 Backtracks, 0 Restarts.
//- Solution #28 found. 5447 Time (ms), 126 Nodes, 50 Backtracks, 0 Restarts.
//- Solution #29 found. 5447 Time (ms), 129 Nodes, 53 Backtracks, 0 Restarts.
//- Solution #30 found. 5447 Time (ms), 130 Nodes, 54 Backtracks, 0 Restarts.
//- Solution #31 found. 5447 Time (ms), 132 Nodes, 56 Backtracks, 0 Restarts.
//- Solution #32 found. 5447 Time (ms), 133 Nodes, 57 Backtracks, 0 Restarts.
//- Solution #33 found. 5447 Time (ms), 139 Nodes, 63 Backtracks, 0 Restarts.
//- Solution #34 found. 5447 Time (ms), 140 Nodes, 64 Backtracks, 0 Restarts.
//- Solution #35 found. 5447 Time (ms), 142 Nodes, 66 Backtracks, 0 Restarts.
//- Solution #36 found. 5647 Time (ms), 143 Nodes, 67 Backtracks, 0 Restarts.
//- Solution #37 found. 5647 Time (ms), 146 Nodes, 70 Backtracks, 0 Restarts.
//- Solution #38 found. 5647 Time (ms), 147 Nodes, 71 Backtracks, 0 Restarts.
//- Solution #39 found. 5647 Time (ms), 149 Nodes, 73 Backtracks, 0 Restarts.
//- Solution #40 found. 5647 Time (ms), 150 Nodes, 74 Backtracks, 0 Restarts.
//- Solution #41 found. 5647 Time (ms), 154 Nodes, 78 Backtracks, 0 Restarts.
//- Solution #42 found. 5647 Time (ms), 155 Nodes, 79 Backtracks, 0 Restarts.
//- Solution #43 found. 5647 Time (ms), 157 Nodes, 81 Backtracks, 0 Restarts.
//- Solution #44 found. 5647 Time (ms), 158 Nodes, 82 Backtracks, 0 Restarts.
//- Solution #45 found. 5647 Time (ms), 161 Nodes, 85 Backtracks, 0 Restarts.
//- Solution #46 found. 5848 Time (ms), 162 Nodes, 86 Backtracks, 0 Restarts.
//- Solution #47 found. 5848 Time (ms), 164 Nodes, 88 Backtracks, 0 Restarts.
//- Solution #48 found. 5848 Time (ms), 165 Nodes, 89 Backtracks, 0 Restarts.
//- Solution #49 found. 5848 Time (ms), 170 Nodes, 94 Backtracks, 0 Restarts.
//- Solution #50 found. 5848 Time (ms), 171 Nodes, 95 Backtracks, 0 Restarts.
//- Solution #51 found. 5848 Time (ms), 173 Nodes, 97 Backtracks, 0 Restarts.
//- Solution #52 found. 5848 Time (ms), 174 Nodes, 98 Backtracks, 0 Restarts.
//- Solution #53 found. 5848 Time (ms), 177 Nodes, 101 Backtracks, 0 Restarts.
//- Solution #54 found. 5848 Time (ms), 178 Nodes, 102 Backtracks, 0 Restarts.
//- Solution #55 found. 6048 Time (ms), 180 Nodes, 104 Backtracks,Solution #100 - 6664 Time (ms), 270 Nodes, 194 Backtracks, 0 Restarts - 
// 0 Restarts.
//- Solution #56 found. 6048 Time (ms), 181 Nodes, 105 Backtracks, 0 Restarts.
//- Solution #57 found. 6048 Time (ms), 185 Nodes, 109 Backtracks, 0 Restarts.
//- Solution #58 found. 6048 Time (ms), 186 Nodes, 110 Backtracks, 0 Restarts.
//- Solution #59 found. 6048 Time (ms), 188 Nodes, 112 Backtracks, 0 Restarts.
//- Solution #60 found. 6048 Time (ms), 189 Nodes, 113 Backtracks, 0 Restarts.
//- Solution #61 found. 6048 Time (ms), 192 Nodes, 116 Backtracks, 0 Restarts.
//- Solution #62 found. 6048 Time (ms), 193 Nodes, 117 Backtracks, 0 Restarts.
//- Solution #63 found. 6048 Time (ms), 195 Nodes, 119 Backtracks, 0 Restarts.
//- Solution #64 found. 6048 Time (ms), 196 Nodes, 120 Backtracks, 0 Restarts.
//- Solution #65 found. 6264 Time (ms), 203 Nodes, 127 Backtracks, 0 Restarts.
//- Solution #66 found. 6264 Time (ms), 204 Nodes, 128 Backtracks, 0 Restarts.
//- Solution #67 found. 6264 Time (ms), 206 Nodes, 130 Backtracks, 0 Restarts.
//- Solution #68 found. 6264 Time (ms), 207 Nodes, 131 Backtracks, 0 Restarts.
//- Solution #69 found. 6264 Time (ms), 210 Nodes, 134 Backtracks, 0 Restarts.
//- Solution #70 found. 6264 Time (ms), 211 Nodes, 135 Backtracks, 0 Restarts.
//- Solution #71 found. 6264 Time (ms), 213 Nodes, 137 Backtracks, 0 Restarts.
//- Solution #72 found. 6264 Time (ms), 214 Nodes, 138 Backtracks, 0 Restarts.
//- Solution #73 found. 6464 Time (ms), 218 Nodes, 142 Backtracks, 0 Restarts.
//- Solution #74 found. 6464 Time (ms), 219 Nodes, 143 Backtracks, 0 Restarts.
//- Solution #75 found. 6464 Time (ms), 221 Nodes, 145 Backtracks, 0 Restarts.
//- Solution #76 found. 6464 Time (ms), 222 Nodes, 146 Backtracks, 0 Restarts.
//- Solution #77 found. 6464 Time (ms), 225 Nodes, 149 Backtracks, 0 Restarts.
//- Solution #78 found. 6464 Time (ms), 226 Nodes, 150 Backtracks, 0 Restarts.
//- Solution #79 found. 6464 Time (ms), 228 Nodes, 152 Backtracks, 0 Restarts.
//- Solution #80 found. 6464 Time (ms), 229 Nodes, 153 Backtracks, 0 Restarts.
//- Solution #81 found. 6464 Time (ms), 234 Nodes, 158 Backtracks, 0 Restarts.
//- Solution #82 found. 6464 Time (ms), 235 Nodes, 159 Backtracks, 0 Restarts.
//- Solution #83 found. 6464 Time (ms), 237 Nodes, 161 Backtracks, 0 Restarts.
//- Solution #84 found. 6464 Time (ms), 238 Nodes, 162 Backtracks, 0 Restarts.
//- Solution #85 found. 6664 Time (ms), 241 Nodes, 165 Backtracks, 0 Restarts.
//- Solution #86 found. 6664 Time (ms), 242 Nodes, 166 Backtracks, 0 Restarts.
//- Solution #87 found. 6664 Time (ms), 244 Nodes, 168 Backtracks, 0 Restarts.
//- Solution #88 found. 6664 Time (ms), 245 Nodes, 169 Backtracks, 0 Restarts.
//- Solution #89 found. 6664 Time (ms), 249 Nodes, 173 Backtracks, 0 Restarts.
//- Solution #90 found. 6664 Time (ms), 250 Nodes, 174 Backtracks, 0 Restarts.
//- Solution #91 found. 6664 Time (ms), 252 Nodes, 176 Backtracks, 0 Restarts.
//- Solution #92 found. 6664 Time (ms), 253 Nodes, 177 Backtracks, 0 Restarts.
//- Solution #93 found. 6664 Time (ms), 256 Nodes, 180 Backtracks, 0 Restarts.
//- Solution #94 found. 6664 Time (ms), 257 Nodes, 181 Backtracks, 0 Restarts.
//- Solution #95 found. 6664 Time (ms), 259 Nodes, 183 Backtracks, 0 Restarts.
//- Solution #96 found. 6664 Time (ms), 260 Nodes, 184 Backtracks, 0 Restarts.
//- Solution #97 found. 6664 Time (ms), 266 Nodes, 190 Backtracks, 0 Restarts.
//- Solution #98 found. 6664 Time (ms), 267 Nodes, 191 Backtracks, 0 Restarts.
//- Solution #99 found. 6664 Time (ms), 269 Nodes, 193 Backtracks, 0 Restarts.
//- Solution #100 found. 6664 Time (ms), 270 Nodes, 194 Backtracks, 0 Restarts.
//- Solution #1 found. Upper-bound: 2147483647, 5408 Time (ms), 76 Nodes, 0 Backtracks, 0 Restarts.
//- Solution #2 found. Upper-bound: -134, 5408 Time (ms), 77 Nodes, 1 Backtracks, 0 Restarts.
//- Solution #3 found. Upper-bound: -136, 5408 Time (ms), 79 Nodes, 3 Backtracks, 0 Restarts.
//- Solution #4 found. Upper-bound: -143, 5408 Time (ms), 80 Nodes, 4 Backtracks, 0 Restarts.
//- Partial Search - Upper-bound: -145, 35831 Time (ms), 5000 Nodes, 9856 Backtracks, 0 Restarts.
//- Partial Search - Upper-bound: -145, 71069 Time (ms), 10000 Nodes, 19854 Backtracks, 0 Restarts.
//- Partial Search - Upper-bound: -145, 115710 Time (ms), 15000 Nodes, 29854 Backtracks, 0 Restarts.
//- Partial Search - Upper-bound: -145, 167344 Time (ms), 20000 Nodes, 39852 Backtracks, 0 Restarts.
}
