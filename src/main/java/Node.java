package javaexample;

public class Node<A extends Comparable> implements Tree<A> {
    public Tree<A> left = null;
    public Tree<A> right = null;
    public A value;

    public Node(A v) {
        value = v;
    }

    public Node(Tree<A> l, A v, Tree<A> r) {
        left = l;
        right = r;
        value = v;
    }

    @Override
    public Tree<A> insert(A v) {
        if (v.compareTo(value) > 0) {
            right = insert(v, right);
            return this;
        } else if (v.compareTo(value) == 0) {
            return this;
        } else {
            left = insert(v, left);
            return this;
        }
    }

    private Tree<A> insert(A v, Tree<A> t) {
        if (t == null) {
            return new Node(v);
        } else {
            return t.insert(v);
        }
    }

    public String toString() {
        String l = null;
        if (left == null) l = "Empty";
        else l = left.toString();
        String r = null;
        if (right == null) r = "Empty";
        else r = right.toString();
        return "(" + l + ", " + value + ", " + r + ")";
    }
}
