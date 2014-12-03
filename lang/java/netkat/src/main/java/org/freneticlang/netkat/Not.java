package org.freneticlang.netkat;

public class Not implements Predicate {
    private Predicate pred;

    public Not(Predicate pred) {
        this.pred = pred;
    }

    public String toString() {
        return ("(" + "not " + pred + ")");
    }
}
