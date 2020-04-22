package de.uniba.sme.bambirds.level_selection;

/**
 * Contains two numerical values that should represent two predictions
 * (ScorePrediction and WinProbability).
 *
 * @param <T> Data type of the first value
 * @param <E> Data type of the second value
 */
public class PredictionTuple<T extends Number, E extends Number> {

	private T score;
	private E winProbability;

	public PredictionTuple(T score, E winProbability) {
		this.score = score;
		this.winProbability = winProbability;
	}

	public T getScore() {
		return this.score;
	}

	public E getWinProbability() {
		return this.winProbability;
	}

	public void setScore(T value) {
		this.score = value;
	}

	public void setWinProbability(E value) {
		this.winProbability = value;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof PredictionTuple)) {
			return true;
		}

		PredictionTuple<?, ?> o = (PredictionTuple<?, ?>) obj;

		return this.score.equals(o.score) && this.winProbability.equals(o.winProbability);
	}

	@Override
	public int hashCode() {
		return (this.score == null ? 0 : this.score.hashCode())
				^ (this.winProbability == null ? 0 : this.winProbability.hashCode());
	}

	@Override
	public String toString() {
		return "(" + score + "," + winProbability + ")";
	}
}
