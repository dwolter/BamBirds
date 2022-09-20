package de.uniba.sme.bambirds.common.gson;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import de.uniba.sme.bambirds.common.objects.Plan;

import java.awt.Polygon;

public final class BamBirdsGson {

	private BamBirdsGson() {
	}

	public static Gson getGson() {
		final GsonBuilder gsonBuilder = new GsonBuilder();
		gsonBuilder.registerTypeAdapter(Polygon.class, new PolygonTypeAdapter());
		gsonBuilder.setExclusionStrategies(new BamBirdsExclusionStrategy());
		gsonBuilder.registerTypeAdapter(Plan.class, new JsonDeserializerWithRequired<Plan>());
		return gsonBuilder.create();
	}
}
