/**
 * This package provides the deductions of the IR optimizer: feasibility-based
 * bounds tightening ({@link org.clafer.ir.analysis.deduction.FBBT}), with one
 * deducer per kind of IR constraint, which narrows the domains of the
 * variables before the IR is compiled to Choco, removes constraints that hold,
 * and coalesces variables that must be equal.
 */
package org.clafer.ir.analysis.deduction;
