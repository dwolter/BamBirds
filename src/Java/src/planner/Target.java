package planner;

public class Target {

	private String targetId;
    private int angle; // between +90 and -90
	private String debugInfo;
	private double confidence;
	private ThinkerType thinker; // the "source" of the Target

	Target(String target, int angle, String debugInfo, double confidence, ThinkerType thinker) {
		this.targetId = target;
		this.debugInfo = debugInfo;
		this.confidence = confidence;
		this.thinker = thinker;
		this.angle = angle;
	}

	/**
	 * Gives the target to be hit by a shot.
	 * 
	 * @return the ID of the targeted object as a String
	 */
	public String getTargetId() {
		return targetId;
	}

    public int getAngle() {
        return angle;
    }

	/**
	 * @return a String indicating what strategy has been chosen or where the Target
	 *         originated from (e.g. PhysicsSim).
	 */
	public String getDebugInfo() {
		return debugInfo;
	}

	public double getConfidence() {
		return confidence;
	}

	public ThinkerType getThinker() {
		return thinker;
	}

	public String prettyPrint() {
		return String.format("Target: %7s @ %3s  DebugInfo: %-20s Confidence: %1.4f",
				targetId, angle, debugInfo, confidence);
	}

	public String toString() {
		return String.format("(Target %s:%s:%d:%1.4f)", debugInfo, targetId, angle, confidence);
	}
}
