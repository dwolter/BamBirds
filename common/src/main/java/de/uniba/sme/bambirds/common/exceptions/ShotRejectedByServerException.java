package de.uniba.sme.bambirds.common.exceptions;

public class ShotRejectedByServerException extends Exception {

	public ShotRejectedByServerException(final String message) {
		super(message);
	}

	public ShotRejectedByServerException(final String message, final Throwable t) {
		super(message, t);
	}
}
