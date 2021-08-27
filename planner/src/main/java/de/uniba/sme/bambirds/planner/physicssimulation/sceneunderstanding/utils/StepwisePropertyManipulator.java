package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.utils;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.PropertyModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.PropertyState;

import java.math.BigDecimal;
import java.util.Random;

public class StepwisePropertyManipulator {

	private BigDecimal currentValue;
	private BigDecimal stepSize;
	private PropertyModifier modifier;
	private float minValue;
	private float maxValue;
	private float maxInitialRandomOffset;

	public StepwisePropertyManipulator(PropertyModifier modifier, float stepSize, float initialValue, float minValue,
			float maxValue, float maxInitialRandomOffset) {
		this.modifier = modifier;
		this.stepSize = toBigDecimal(stepSize);
		this.minValue = minValue;
		this.maxValue = maxValue;
		this.maxInitialRandomOffset = maxInitialRandomOffset;
		currentValue = toBigDecimal(initialValue);
	}

	public StepwisePropertyManipulator(PropertyModifier modifier, float stepSize, Simulation initialValueSim,
			float minValue, float maxValue, float maxInitialRandomOffset) {
		this.modifier = modifier;
		this.stepSize = toBigDecimal(stepSize);
		this.minValue = minValue;
		this.maxValue = maxValue;
		this.maxInitialRandomOffset = maxInitialRandomOffset;
		currentValue = toBigDecimal(modifier.getCurrentPropertyValue(initialValueSim));
	}

	public void setCurrentValueAccordingTo(Simulation refSim) {
		currentValue = toBigDecimal(modifier.getCurrentPropertyValue(refSim));
	}

	public boolean isAboveLimitRange() {
		return currentValue.floatValue() > maxValue;
	}

	public boolean isBelowLimitRange() {
		return currentValue.floatValue() < minValue;
	}

	public boolean isBetweenLimitRange() {
		return currentValue.floatValue() >= minValue && currentValue.floatValue() <= maxValue;
	}

	public void increment() {
		currentValue = currentValue.add(stepSize);
	}

	public void decrement() {
		currentValue = currentValue.subtract(stepSize);
	}

	public void incrementNtimes(int n) {
		//        currentValue += ((float) n * stepSize);
		currentValue = currentValue.add(stepSize.multiply(toBigDecimal(n)));
	}

	public void decrementNtimes(int n) {
		currentValue = currentValue.subtract(stepSize.multiply(toBigDecimal(n)));

	}

	public PropertyState getCurrentPropertyState() {
		return new PropertyState(modifier, currentValue.floatValue());
	}

	public void applyRandomOffset(float maxRandomOffset) {
		Random random = new Random();
		float newValue = currentValue.floatValue()  +(random.nextFloat() * 2f - 1) * maxInitialRandomOffset;
		if(newValue > maxValue){
			newValue = maxValue;
		}
		if(newValue < minValue){
			newValue = minValue;
		}
		currentValue  = toBigDecimal(newValue);
	}

	public StepwisePropertyManipulator getCopy() {
		return new StepwisePropertyManipulator(modifier, currentValue.floatValue(), stepSize.floatValue(), minValue,
				maxValue, maxInitialRandomOffset);
	}

	private BigDecimal toBigDecimal(float inputValue) {
		return new BigDecimal(String.valueOf(inputValue));
	}

	private BigDecimal toBigDecimal(int inputValue) {
		return new BigDecimal(String.valueOf(inputValue));
	}
}