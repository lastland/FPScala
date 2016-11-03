package javaexample;

public class NatFactory {
    private static Nat m_z = new Z();

    public static Nat Z() {
        return m_z;
    }

    public static Nat S(Nat n) {
        return new S(n);
    }
}
