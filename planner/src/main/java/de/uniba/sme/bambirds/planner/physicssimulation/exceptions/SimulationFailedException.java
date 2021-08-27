package de.uniba.sme.bambirds.planner.physicssimulation.exceptions;

/**
 * Throw when the simulation failed.
 */
public class SimulationFailedException extends Exception {

    private static final long serialVersionUID = 1L;

    public SimulationFailedException() {
    }

    public SimulationFailedException(String message) {
        super(message);
    }

    public SimulationFailedException(Throwable cause) {
        super(cause);
    }

    public SimulationFailedException(String message, Throwable cause) {
        super(message, cause);
    }

    public SimulationFailedException(String message, Throwable cause, boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
    
    
}