import java.util.HashMap;
import java.util.Map;

// A simple class representing an Expression node.
// Crucially, it does NOT override equals() or hashCode().
class Expr {
    String name;

    Expr(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "Expr(\"" + name + "\")@" + Integer.toHexString(System.identityHashCode(this));
    }
}

public class SideTableDemo {
    public static void main(String[] args) {
        System.out.println("=== Java Reference Identity Demo ===\n");

        // 1. Create two distinct objects that represent the "same" variable 'a'
        // In a parser, these would be two different nodes in the syntax tree.
        Expr expr1 = new Expr("a");
        Expr expr2 = new Expr("a");

        System.out.println("Created two expressions:");
        System.out.println("1. " + expr1);
        System.out.println("2. " + expr2);
        System.out.println();

        // Verify they are distinct objects
        System.out.println("Are they the same reference? (expr1 == expr2): " + (expr1 == expr2));
        System.out.println("Are they equal? (expr1.equals(expr2)): " + expr1.equals(expr2));
        System.out.println("(Note: Object.equals() defaults to reference equality)\n");

        // 2. Create the "Side Table"
        // This map uses the object reference itself as the key.
        Map<Expr, Integer> locals = new HashMap<>();

        // 3. Store different resolution depths for them
        // This simulates one 'a' being global (depth null/undefined) or local at depth 1,
        // and another 'a' being local at depth 2.
        System.out.println("Storing resolution depths in the side table...");
        locals.put(expr1, 1);
        locals.put(expr2, 42);

        // 4. Retrieve values
        System.out.println("\nRetrieving values from side table:");
        System.out.println("Depth for expr1: " + locals.get(expr1)); // Should be 1
        System.out.println("Depth for expr2: " + locals.get(expr2)); // Should be 42

        // 5. Try with a new object that looks the same
        Expr expr3 = new Expr("a");
        System.out.println("\nDepth for a new 'a' node (expr3): " + locals.get(expr3)); // Should be null
    }
}
