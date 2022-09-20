package de.uniba.sme.bambirds.common.exceptions;

public class MissingSlingshotException extends Exception {
	public MissingSlingshotException(final String message) {
		super(message);
	}

	public MissingSlingshotException(final String message, final Throwable t) {
		super(message, t);
	}
}
