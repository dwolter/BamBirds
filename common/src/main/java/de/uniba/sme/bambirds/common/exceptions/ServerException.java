package de.uniba.sme.bambirds.common.exceptions;

public class ServerException extends Exception {
	/**
	 *
	 */
	private static final long serialVersionUID = 6539471423424464197L;

	public enum Reason {
		INVALID_RESPONSE, SHUTDOWN
	}

	public Reason reason;

	static private String messageForReason(Reason reason) {
		switch (reason) {
			case INVALID_RESPONSE:
				return "The server answered with an unkown response";
			case SHUTDOWN:
				return "Server is shutting down. It will not respond to any requests";
			default:
				return "Undefined server exception";
		}
	}

	public ServerException(Reason reason) {
		super(messageForReason(reason));
		this.reason = reason;
	}

	public ServerException(String message) {
		super(message);
	}

	public ServerException(String message, Reason reason){
		super(message);
		this.reason = reason;
	}

	public ServerException(Reason reason, Throwable t) {
		super(messageForReason(reason), t);
		this.reason = reason;
	}

	public ServerException(String message, Throwable t) {
		super(message, t);
	}

	public ServerException(String message, Reason reason, Throwable t){
		super(message, t);
		this.reason = reason;
	}

}
