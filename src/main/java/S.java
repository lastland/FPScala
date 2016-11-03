package javaexample;

class S implements Nat {
    private Nat p;

    public S(Nat n) {
        p = n;
    }

    public Nat pred() {
        return p;
    }

    public String toString() {
        return "(S " + p + ")";
    }
}
