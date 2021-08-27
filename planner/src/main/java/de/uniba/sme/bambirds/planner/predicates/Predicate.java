package de.uniba.sme.bambirds.planner.predicates;


import java.util.Arrays;
import java.util.Objects;

public class Predicate implements Comparable<Predicate> {
    private final String predicateName;
    private final String[] args;
    private final String rest;

    public Predicate(String predicateName) {
        this.predicateName = predicateName;
        this.args = null;
        this.rest = null;
    }

    public Predicate(String predicateName, String... args) {
        this.predicateName = predicateName;
        this.args = args;
        this.rest = null;
    }

    public Predicate(String predicateName, Object... args) {
        this.predicateName = predicateName;
        this.args = Arrays.stream(args)
                .map(Object::toString)
                .toArray(String[]::new);
        this.rest = null;
    }

    public Predicate(String predicateName, String[] args, String rest) {
        this.predicateName = predicateName;
        this.args = args;
        this.rest = rest;
    }

    public String getPredicateName() {
        return predicateName;
    }

    public String[] getArgs() {
        return args;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(predicateName);
        if (args != null) {
            sb.append('(');
            for (int i = 0; i < args.length; i++) {
                sb.append(args[i]);
                if(i < args.length - 1) {
                    sb.append(',');
                }
            }
            sb.append(')');
            if (rest != null) {
                sb.append(" :- ").append(rest);
            }
        }
        sb.append(".\n");
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Predicate predicate = (Predicate) o;
        return Objects.equals(predicateName, predicate.predicateName) && Arrays.equals(args, predicate.args) && Objects.equals(rest, predicate.rest);
    }

    @Override
    public int hashCode() {
        // Default hashCode implementation
        int result = Objects.hash(predicateName, rest);
        result = 31 * result + Arrays.hashCode(args);
        return result;
    }

    @Override
    public int compareTo(Predicate predicate) {
        if (this.equals(predicate)) return 0;
        if (predicate == null) return 1;

        int predicateNameComparison = predicateName.compareTo(predicate.predicateName);
        if (predicateNameComparison != 0){
            return predicateNameComparison;
        }

        if (args.length != predicate.args.length) {
            return args.length > predicate.args.length ? 1 : -1;
        }
        for (int i = 0; i < args.length; i++) {
            int argComparison = args[i].compareTo(predicate.args[i]);
            if (argComparison != 0){
                return argComparison;
            }
        }
        if (rest == null && predicate.rest == null) {
            return 0;
        }
        if (rest != null && predicate.rest == null) {
            return 1;
        }
        if (rest == null) {
            return -1;
        }
        return rest.compareTo(predicate.rest);
    }
}
