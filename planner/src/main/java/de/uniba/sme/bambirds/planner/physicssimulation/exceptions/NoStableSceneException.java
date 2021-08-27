package de.uniba.sme.bambirds.planner.physicssimulation.exceptions;

/**
 * Throw when the scene cannot be stabilized.
 */
public class NoStableSceneException extends Exception {

    private static final long serialVersionUID = 1L;

    public NoStableSceneException() {
    }

    public NoStableSceneException(String arg0) {
        super(arg0);
    }

    public NoStableSceneException(Throwable arg0) {
        super(arg0);
    }

    public NoStableSceneException(String message, Throwable cause) {
        super(message, cause);
    }

    public NoStableSceneException(String message, Throwable cause, boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}