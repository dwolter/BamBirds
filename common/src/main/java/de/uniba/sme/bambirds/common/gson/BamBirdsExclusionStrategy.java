package de.uniba.sme.bambirds.common.gson;

import com.google.gson.ExclusionStrategy;
import com.google.gson.FieldAttributes;

public class BamBirdsExclusionStrategy implements ExclusionStrategy {

	public boolean shouldSkipField(FieldAttributes f) {
		 return f.getAnnotation(Exclude.class) != null;
	}

	public boolean shouldSkipClass(Class<?> clazz) {
		 return clazz.getAnnotation(Exclude.class) != null;
	}

}