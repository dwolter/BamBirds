package de.uniba.sme.bambirds.planner.physicssimulation.exceptions;

/**
 * Throw ehen an error during the predicate generation process occurs
 */
public class PredicateGenerationException extends Exception {

    private static final long serialVersionUID = 1L;

    public PredicateGenerationException() {
    }

    public PredicateGenerationException(String message) {
        super(message);
    }

    public PredicateGenerationException(Throwable cause) {
        super(cause);
    }

    public PredicateGenerationException(String message, Throwable cause) {
        super(message, cause);
    }

    public PredicateGenerationException(String message, Throwable cause, boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
    
}