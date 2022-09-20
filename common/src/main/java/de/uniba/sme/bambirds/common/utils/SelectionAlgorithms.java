package de.uniba.sme.bambirds.common.utils;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.stream.Collectors;

public final class SelectionAlgorithms {

	private static final Random RANDOM = new Random();

	private SelectionAlgorithms() {
	}

	/**
	 * Select the option with the highest probability
	 *
	 * @param options map with the first element being the object to select and the second the probability of it's selection
	 * @param <K>     Type of the object to select
	 * @return the object with the highest probability
	 */
	public static <K> K greedy(final Map<K, Double> options) {
		if (options.size() < 1) {
			throw new IllegalArgumentException("Options can not be empty");
		}
		return options.entrySet().stream().max(Comparator.comparingDouble(Map.Entry::getValue)).map(Map.Entry::getKey).orElse(null);
	}

	/**
	 * Return a weighted random element of options with the probability of epsilon, otherwise use greedy selection
	 *
	 * @param options map with the first element being the object to select and the second the probability of it's selection
	 * @param epsilon probability of random selection
	 * @param <K>     Type of the object to select
	 * @return The selected object
	 */
	public static <K> K epsilonGreedy(final Map<K, Double> options, final double epsilon) {
		if (RANDOM.nextDouble() < epsilon) {
			return randomWeighted(options);
		} else {
			return greedy(options);
		}
	}

	/**
	 * Randomly select an element of the options based on their probability. Falls back to complete random selection if probabilities do not add up to 1
	 *
	 * @param options map with the first element being the object to select and the second the probability of its selection. Probabilities of all options should add up to 1
	 * @param <K>     Type of the object to select
	 * @return The randomly selected object
	 */
	public static <K> K randomWeighted(final Map<K, Double> options) {
		if (options.size() < 1) {
			throw new IllegalArgumentException("Options can not be empty");
		}
		double r = RANDOM.nextDouble();
		double countWeight = 0.0;
		for (Map.Entry<K, Double> option : options.entrySet()) {
			countWeight += option.getValue();
			if (countWeight >= r) {
				return option.getKey();
			}
		}
		// Should never get here if the probabilities of options add up to 1
		return random(options);
	}

	/**
	 * Completely random selection of an element from options
	 *
	 * @param options map with the first element being the object to select and the second the probability of it's selection
	 * @param <K>     Type of the object to select
	 * @return The randomly selected object
	 */
	public static <K> K random(final Map<K, Double> options) {
		if (options.size() < 1) {
			throw new IllegalArgumentException("Options can not be empty");
		}
		int randomElement = RANDOM.nextInt(options.size());
		return new ArrayList<>(options.entrySet()).get(randomElement).getKey();
	}

	/**
	 * Normalize weights of options, so they add up to 1 using the softmax algorithm
	 *
	 * @param options map with the first element being the object to select and the second the probability of it's selection
	 * @param <K>     Type of the object to select
	 * @return A new map with the elements weights adjusted
	 */
	public static <K> Map<K, Double> softmax(final Map<K, Double> options) {
		Map<K, Double> result = new HashMap<>();
		double meanWeight = options.entrySet().stream()
				.collect(Collectors.averagingDouble(x -> Math.abs(x.getValue())));
		double sum = 0;

		// Softmax = sum{i = 1 -> K}(exp(z.i)/sum{j = 1 -> K}(exp(z.j)))
		for (Map.Entry<K, Double> improvement : options.entrySet()) {
			double value = improvement.getValue() / meanWeight;
			value = Math.exp(value);
			sum += value;
			result.put(improvement.getKey(), value);
		}

		final double sumComputed = sum;
		result.replaceAll((key, value) -> value / sumComputed);
		return result;
	}

	/**
	 * My new method called Second Order Thompson Sampling with exponent k in [0,infinity)
	 * (you could also make k negative if you want 'negative exploitation', i.e. a higher probability of selecting lower evaluated actions)
	 * vary k to select a custom trade-off between exploration vs exploitation
	 * <ul>
	 * <li>k = 1 -> regular Thompson sampling - linear</li>
	 * <li>k > 1 -> approaches greedy selection as k approaches infinity (pure exploitation) - superlinear</li>
	 * <li>k < 1 -> approaches a uniform sample as k approaches 0 (pure exploration) - sublinear</li>
	 * </ul>
	 * @param options
	 * @param exponent
	 * @return
	 * @author Daniel Lutalo - Agent X
	 */
	public static <K> Map<K, Double> secondOrderThompsonSample(Map<K, Double> options, double exponent){
		// normalise scores into probabilities (first Thompson sample)
		double sum = options.values().stream().reduce(0.0, Double::sum);
		Map<K, Double> normalised = new HashMap<>();
		options.forEach((k,v) -> normalised.put(k,v/sum));

		// exponentiate the probabilities and renormalise (second Thompson sample)
		double exponentiatedSum = normalised.values().stream().reduce(0.0, (a,b) -> a + Math.pow(b, exponent));
		Map<K, Double> finalProbabilities = new HashMap<>();
		normalised.forEach((k,v) -> finalProbabilities.put(k,Math.pow(v, exponent)/exponentiatedSum));

		return finalProbabilities;
	}


}
