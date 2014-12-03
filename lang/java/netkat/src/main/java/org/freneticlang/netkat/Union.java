package org.freneticlang.netkat;

public class Union implements Policy {
    private Policy left, right;

    public Union(Policy left, Policy Right) {
        this.left = left;
        this.right = right;
    }

    public String toString() {
        return ("(" + left + " | " + right + ")");
    }
}
