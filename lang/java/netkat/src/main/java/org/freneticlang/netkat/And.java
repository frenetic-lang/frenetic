package org.freneticlang.netkat;

public class And implements Predicate {
    private Predicate left, right;

    public And(Predicate left, Predicate Right) {
        this.left = left;
        this.right = right;
    }

    public String toString() {
        return ("(" + left + " and " + right + ")");
    }
}
