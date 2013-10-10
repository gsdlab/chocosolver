var load = rsc.load.bind(rsc);
var solve = rsc.solve.bind(rsc);
var maximize = rsc.maximize.bind(rsc);
var minimize = rsc.minimize.bind(rsc);
var minUnsat = rsc.minUnsat.bind(rsc);
var unsatCore = rsc.unsatCore.bind(rsc);
var stats = rsc.stats.bind(rsc);
var exit = rsc.exit.bind(rsc);

// Debugging
var __internals__ = rsc.internals.bind(rsc);