package de.uniba.sme.bambirds.common.utils;

public class ConfigureResponse {

	public enum RoundType {
		/**
		 * First qualification round.
		 */
		QUALIFICATION_1,
		/**
		 * Second qualification round.
		 */
		QUALIFICATION_2,
		/**
		 * Semi finals.
		 */
		SEMI_FINAL,
		/**
		 * Finals.
		 */
		FINAL,
		/**
		 * Either got rejected or we are not in competition mode.
		 */
		UNKNOWN
	}

	private final RoundType round;
	private final int timeLimit;
	private final int numberOfLevels;

	public ConfigureResponse(final int roundIndex, final int timeLimit, final int numberOfLevels) {
		switch (roundIndex) {
			case 1:
				round = RoundType.QUALIFICATION_1;
				break;
			case 2:
				round = RoundType.QUALIFICATION_2;
				break;
			case 3:
				round = RoundType.SEMI_FINAL;
				break;
			case 4:
				round = RoundType.FINAL;
				break;
			default:
				round = RoundType.UNKNOWN;
		}
		this.timeLimit = timeLimit;
		this.numberOfLevels = numberOfLevels;
	}

	public ConfigureResponse(final RoundType round, final int timeLimit, final int numberOfLevels) {
		this.round = round;
		this.timeLimit = timeLimit;
		this.numberOfLevels = numberOfLevels;
	}

	public RoundType getRound() {
		return round;
	}

	public int getTimeLimit() {
		return timeLimit;
	}

	public int getNumberOfLevels() {
		return numberOfLevels;
	}

}
