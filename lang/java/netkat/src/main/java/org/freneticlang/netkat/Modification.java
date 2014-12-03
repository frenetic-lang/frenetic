package org.freneticlang.netkat;

public class Modification implements Policy {
    private String field, value;

    public Modification(String field, String value) {
        this.field = field; 
        this.value = value;
    }

    public String toString() {
        return ("(" + field + " := " + value + ")");
    }
}
