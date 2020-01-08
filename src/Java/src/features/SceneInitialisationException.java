package features;

public class SceneInitialisationException extends Exception {
	private static final long serialVersionUID = 7146267070968031811L;

	public enum Reason {
		NO_SLING_FOUND, NO_BIRDS_FOUND, NO_PIGS_FOUND
	}
	public Reason reason;

	static private String messageForReason(Reason reason) {
		switch (reason) {
		case NO_SLING_FOUND: return "Could not find slingshot.";
		case NO_BIRDS_FOUND: return "No birds found.";
		case NO_PIGS_FOUND:  return "No pigs found.";
		default:             return "Undefined scene initialisation exception";
		}
	}

	public SceneInitialisationException(Reason reason) {
		super(messageForReason(reason));
		this.reason = reason;
	}
}
