package org.freneticlang.netkat;

public class Star implements Predicate {
    private Predicate pred;

    public Star(Predicate pred) {
        this.pred = pred;
    }

    public String toString() {
        return ("(" + pred + "*" + ")");
    }
}
