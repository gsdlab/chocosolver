/**
 * This package provides Clafer specific Choco constraints required for
 * efficient solving. Most constraints assume that the set envelope and kernel
 * are sorted. The Choco library does not guarantee this to be the case, the
 * compiler needs to guarantee it. The advantage is that some of the propagators
 * can be implemented more efficiently with this guarantee.
 *
 * @author jimmy
 */
package org.clafer.choco.constraint;
