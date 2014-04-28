package org.clafer.domain;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import org.clafer.collection.EmptyIntIterator;
import org.clafer.ir.IrException;

/**
 * A domain of size 0. Contains nothing.
 *
 * @author jimmy
 */
public class EmptyDomain implements Domain {

    @Override
    public boolean isBounded() {
        return false;
    }

    @Override
    public boolean contains(int value) {
        return false;
    }

    @Override
    public int getLowBound() {
        throw new IrException("Emtpy domain does not have a low bound.");
    }

    @Override
    public int getHighBound() {
        throw new IrException("Emtpy domain does not have a high bound.");
    }

    @Override
    public boolean isEmpty() {
        return true;
    }

    @Override
    public int size() {
        return 0;
    }

    @Override
    public boolean isSubsetOf(Domain superset) {
        return true;
    }

    @Override
    public boolean intersects(Domain other) {
        return false;
    }

    @Override
    public Domain insert(int value) {
        return Domains.constantDomain(value);
    }

    @Override
    public Domain remove(int value) {
        return this;
    }

    @Override
    public Domain boundLow(int low) {
        return this;
    }

    @Override
    public Domain boundHigh(int high) {
        return this;
    }

    @Override
    public Domain boundBetween(int low, int high) {
        return this;
    }

    @Override
    public Domain minus() {
        return this;
    }

    @Override
    public Domain difference(Domain other) {
        return this;
    }

    @Override
    public Domain intersection(Domain other) {
        return this;
    }

    @Override
    public Domain union(Domain other) {
        return other;
    }

    @Override
    public Domain offset(int c) {
        return this;
    }

    @Override
    public int[] getValues() {
        return new int[]{};
    }

    @Override
    public TIntIterator iterator() {
        return EmptyIntIterator.getIterator();
    }

    @Override
    public TIntIterator iterator(boolean increasing) {
        return EmptyIntIterator.getIterator();
    }

    @Override
    public void transferTo(TIntCollection collection) {
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof EmptyDomain;
    }

    @Override
    public int hashCode() {
        return 305419896;
    }

    @Override
    public String toString() {
        return "{}";
    }
}
