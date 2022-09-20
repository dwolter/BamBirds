package de.uniba.sme.bambirds.common.exceptions;

public class InvalidParabolaException extends Exception {
	public InvalidParabolaException(final String message) {
		super(message);
	}

	public InvalidParabolaException(final String message, final Throwable t) {
		super(message, t);
	}
}
