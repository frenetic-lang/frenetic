package org.freneticlang.netkat;

public class Or implements Predicate {
    private Predicate left, right;

    public Or(Predicate left, Predicate Right) {
        this.left = left;
        this.right = right;
    }

    public String toString() {
        return ("(" + left + " or " + right + ")");
    }
}
