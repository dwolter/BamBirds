package de.uniba.sme.bambirds.common.exceptions;

public class ServerException extends Exception {
	/**
	 *
	 */
	private static final long serialVersionUID = 6539471423424464197L;


	public enum Reason {
		INVALID_RESPONSE, SHUTDOWN
	}

	private Reason reason;

	private static String messageForReason(final Reason reason) {
		switch (reason) {
			case INVALID_RESPONSE:
				return "The server answered with an unknown response";
			case SHUTDOWN:
				return "Server is shutting down. It will not respond to any requests";
			default:
				return "Undefined server exception";
		}
	}

	public ServerException(final Reason reason) {
		super(messageForReason(reason));
		this.reason = reason;
	}

	public ServerException(final String message) {
		super(message);
	}

	public ServerException(final String message, final Reason reason) {
		super(message);
		this.reason = reason;
	}

	public ServerException(final Reason reason, final Throwable t) {
		super(messageForReason(reason), t);
		this.reason = reason;
	}

	public ServerException(final String message, final Throwable t) {
		super(message, t);
	}

	public ServerException(final String message, final Reason reason, final Throwable t) {
		super(message, t);
		this.reason = reason;
	}

	public Reason getReason() {
		return reason;
	}
}
