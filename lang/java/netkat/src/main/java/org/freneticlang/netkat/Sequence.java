package org.freneticlang.netkat;

public class Sequence implements Policy {
    private Policy left, right;

    public Sequence(Policy left, Policy right) {
        this.left = left;
        this.right = right;
    }

    public String toString() {
        return ("(" + left + " ; " + right + ")");
    }
}
