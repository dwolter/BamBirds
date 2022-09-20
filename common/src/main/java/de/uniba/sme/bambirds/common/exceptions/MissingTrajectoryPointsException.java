package de.uniba.sme.bambirds.common.exceptions;

public class MissingTrajectoryPointsException extends Exception {
	public MissingTrajectoryPointsException(final String message) {
		super(message);
	}

	public MissingTrajectoryPointsException(final String message, final Throwable t) {
		super(message, t);
	}
}
