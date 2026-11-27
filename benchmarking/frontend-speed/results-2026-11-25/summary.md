### Summary by suite

| Suite | Models | Both compile | Haskell, median (ms) | Java warm, median (ms) | Ratio of the medians | Haskell, total (s) | Java warm, total (s) | Java in a cold JVM, median (ms) | Cold JVM run, median (ms) |
|---|---|---|---|---|---|---|---|---|---|
| ClaferCaseStudies | 95 | 94 | 70.0 | 3.4 | 21× | 85.9 | 0.73 | 172 | 391 |
| chocosolver | 9 | 9 | 16.0 | 0.7 | 25× | 0.2 | 0.03 | 113 | 316 |
| clafer-positive | 223 | 140 | 17.0 | 0.2 | 92× | 3.2 | 0.06 | 105 | 309 |
| all | 327 | 243 | 21.0 | 0.6 | 35× | 89.3 | 0.81 | 120 | 331 |

### The largest models

| Model | Lines | Haskell (ms) | Java warm (ms) | Ratio | Rhino reads the .js (ms) | Java cold run (ms) |
|---|---|---|---|---|---|---|
| ClaferCaseStudies/PlainClafer/Automotive/BodyDomain/DoorLocks/doorLocks.cfr | 885 | 7,741 | 68.8 | 113× | 3.3 | 712 |
| ClaferCaseStudies/PlainClafer/PreviousWork/Other/AADLToClafer/AADL_Complete.cfr | 621 | 31,995 | 79.5 | 402× | 1.6 | 697 |
| ClaferCaseStudies/PlainClafer/PreviousWork/MurashkinThesis/final_paper_version_two_window_opt.cfr | 600 | 1,507 | 24.6 | 61× | 2.0 | 565 |
| ClaferCaseStudies/PlainClafer/Automotive/BodyDomain/PowerWindow/powerWindow.cfr | 519 | 1,392 | 41.9 | 33× | 2.2 | 685 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/DoorLocks/classes_gen/DoorLocks/CentralDoorLocks/DoorLocksSys.cfr | 517 | 10,504 | 44.5 | 236× | 2.6 | 615 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/DoorLocks/source_gen/DoorLocks/CentralDoorLocks/DoorLocksSys.cfr | 517 | 10,309 | 36.9 | 279× | 2.0 | 624 |
| ClaferCaseStudies/PlainClafer/Automotive/BodyDomain/PowerWindow/driverPowerWindow.cfr | 435 | 473 | 29.4 | 16× | 1.9 | 565 |
| ClaferCaseStudies/PlainClafer/PreviousWork/MurashkinThesis/final_paper_version_driver_opt.cfr | 394 | 278 | 11.5 | 24× | 1.5 | 486 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/PowerWindow/classes_gen/PowerWindow/DriverDoor/DriverWinSys.cfr | 390 | 2,562 | 21.8 | 117× | 2.0 | 527 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/PowerWindow/source_gen/PowerWindow/DriverDoor/DriverWinSys.cfr | 390 | 2,585 | 20.9 | 124× | 1.8 | 534 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/DoorLocks/classes_gen/DoorLocks/CentralDoorLocks/DoorLocksFAA.cfr | 329 | 2,324 | 14.4 | 161× | 1.7 | 505 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/DoorLocks/source_gen/DoorLocks/CentralDoorLocks/DoorLocksFAA.cfr | 329 | 2,335 | 11.8 | 197× | 1.4 | 475 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/PowerWindow/classes_gen/PowerWindow/DriverDoor/DriverWinSysHA.cfr | 304 | 1,392 | 14.0 | 99× | 1.6 | 499 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/PowerWindow/source_gen/PowerWindow/DriverDoor/DriverWinSysHA.cfr | 304 | 1,395 | 13.4 | 104× | 1.5 | 519 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/DoorLocks/classes_gen/DoorLocks/CentralDoorLocks/PassiveKeyEntryFAA.cfr | 301 | 725 | 10.4 | 70× | 1.5 | 430 |

### The slowest models for the Haskell compiler

| Model | Lines | Haskell (ms) | Java warm (ms) | Ratio | Java cold run (ms) |
|---|---|---|---|---|---|
| ClaferCaseStudies/PlainClafer/PreviousWork/Other/AADLToClafer/AADL_Complete.cfr | 621 | 31,995 | 79.5 | 402× | 697 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/DoorLocks/classes_gen/DoorLocks/CentralDoorLocks/DoorLocksSys.cfr | 517 | 10,504 | 44.5 | 236× | 615 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/DoorLocks/source_gen/DoorLocks/CentralDoorLocks/DoorLocksSys.cfr | 517 | 10,309 | 36.9 | 279× | 624 |
| ClaferCaseStudies/PlainClafer/Automotive/BodyDomain/DoorLocks/doorLocks.cfr | 885 | 7,741 | 68.8 | 113× | 712 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/PowerWindow/source_gen/PowerWindow/DriverDoor/DriverWinSys.cfr | 390 | 2,585 | 20.9 | 124× | 534 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/PowerWindow/classes_gen/PowerWindow/DriverDoor/DriverWinSys.cfr | 390 | 2,562 | 21.8 | 117× | 527 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/DoorLocks/source_gen/DoorLocks/CentralDoorLocks/DoorLocksFAA.cfr | 329 | 2,335 | 11.8 | 197× | 475 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/DoorLocks/classes_gen/DoorLocks/CentralDoorLocks/DoorLocksFAA.cfr | 329 | 2,324 | 14.4 | 161× | 505 |
| ClaferCaseStudies/PlainClafer/PreviousWork/MurashkinThesis/final_paper_version_two_window_opt.cfr | 600 | 1,507 | 24.6 | 61× | 565 |
| ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/PowerWindow/source_gen/PowerWindow/DriverDoor/DriverWinSysHA.cfr | 304 | 1,395 | 13.4 | 104× | 519 |

Java warm slower than the Haskell compiler: 0 models

The Java frontend in a cold JVM (its first run, as in the CLI) slower than the Haskell compiler: 218 of 243 models
- the most: clafer-positive/gi49-setting-references-with-a-union.cfr (131 against 16.0 ms), ClaferCaseStudies/MPS/Automotive/BodyDomain/solutions/DoorLocks/classes_gen/DoorLocks/CentralDoorLocks/DoorLocksFM.cfr (205 against 26.0 ms), clafer-positive/if_then_else_with_references.cfr (125 against 16.0 ms), clafer-positive/maxMinArithmetic.cfr (123 against 16.0 ms), clafer-positive/implicit_and_explicit_this.cfr (98.9 against 13.0 ms)
- faster on 25: the models from 278 ms of the Haskell compiler up
- a whole cold JVM run (start, frontend, exit): median 331 ms, from 264 to 712 ms

### The models that fall back or that either side rejects

| Model | Java frontend | Haskell compiler | chocosolver |
|---|---|---|---|
| ClaferCaseStudies/PlainClafer/PreviousWork/Other/Drafts/constraints.cfr | rejected: ClaferCaseStudies/PlainClafer/PreviousWork/Other/Drafts/constraints.cfr:1:4: Name resolver: 'services' not ... | error: Compile error at constraints.cfr:1:4... | rejected |
| clafer-positive/escapes.cfr | escape | ok | falls back |
| clafer-positive/mergeExpAndSetExp.cfr | rejected: clafer-positive/mergeExpAndSetExp.cfr:12:24: Chocosolver does not solve real, nor does any backend | no-js: Choco output unavailable because the model contains: a real or double literal, a reference to a real or double.  | rejected |
| clafer-positive/primitiveTypesAndCoersions.cfr | rejected: clafer-positive/primitiveTypesAndCoersions.cfr:5:6: Chocosolver does not solve real, nor does any backend | no-js: Choco output unavailable because the model contains: a real or double literal, a reference to a real or double.  | rejected |
| clafer-positive/reals.cfr | rejected: clafer-positive/reals.cfr:1:9: Chocosolver does not solve real, nor does any backend | no-js: Choco output unavailable because the model contains: a reference to a real or double.  | rejected |
| the 79 temporal models of clafer-positive | Chocosolver does not solve temporal models, at the first temporal construct of each line | no Choco output: the model contains temporal operators | rejected |

### Whole chocosolver runs, cold (cli.sh: -n 1 --noprint, the same solving on both paths)

| Model | Lines | Java frontend (ms) | --haskell-frontend (ms) | Saved (ms) |
|---|---|---|---|---|
| clafer-positive/implicit_and_explicit_this.cfr | 6 | 229 | 324 | 95.0 |
| clafer-positive/ACCDemo_attributedFeatureModels.cfr | 54 | 341 | 434 | 93.0 |
| ClaferCaseStudies/PlainClafer/Automotive/BodyDomain/PowerWindow/powerWindow.cfr | 519 | 3,597 | 5,028 | 1,431 |
| ClaferCaseStudies/PlainClafer/Automotive/BodyDomain/DoorLocks/doorLocks.cfr | 885 | 8,679 | 16,606 | 7,927 |
| ClaferCaseStudies/PlainClafer/PreviousWork/Other/AADLToClafer/AADL_Complete.cfr | 621 | 15,383 | 48,393 | 33,010 |
