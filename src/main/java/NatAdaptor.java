package javaexample;

public class NatAdaptor implements Comparable<NatAdaptor> {
    private Nat n;

    public NatAdaptor(Nat n) {
        this.n = n;
    }

    public Nat getN() {
        return n;
    }

    @Override
    public int compareTo(NatAdaptor x) {
        return compare(n, x.getN());
    }

    static private int compare(Nat x, Nat y) {
        if (x == NatFactory.Z()) {
            if (y == NatFactory.Z())
                return 0;
            return -1;
        }
        if (y == NatFactory.Z()) {
            return 1;
        }
        S x1 = (S)x;
        S y1 = (S)y;
        return compare(x1.pred(), y1.pred());
    }

    public String toString() {
        return n.toString();
    }
}
