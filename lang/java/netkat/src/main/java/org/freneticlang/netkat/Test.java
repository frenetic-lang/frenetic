package org.freneticlang.netkat;

public class Test implements Predicate {
    private String field, value;

    public Test(String field, String value) {
        this.field = field; 
        this.value = value;
    }

    public String toString() {
        return ("(" + field + " = " + value + ")");
    }
}
