package de.uniba.sme.bambirds.common.gson;

import com.google.gson.JsonParseException;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

import java.awt.Polygon;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class PolygonTypeAdapter extends TypeAdapter<Polygon> {
	@Override
	public void write(final JsonWriter out, final Polygon value) throws IOException {
		out.beginObject();
		out.name("npoints").value(value.npoints);
		out.name("xpoints").beginArray();
		for (int xPoint : value.xpoints) {
			out.value(xPoint);
		}
		out.endArray();
		out.name("ypoints").beginArray();
		for (int yPoint : value.ypoints) {
			out.value(yPoint);
		}
		out.endArray();
		out.endObject();
	}

	@Override
	public Polygon read(final JsonReader in) throws IOException {
		int npoints = 0;
		int[] xpoints = null;
		int[] ypoints = null;
		in.beginObject();
		while (in.hasNext()) {
			switch (in.nextName()) {
				case "npoints":
					npoints = in.nextInt();
					break;
				case "xpoints":
					in.beginArray();
					final List<Integer> xpointsList = new ArrayList<>();
					while (in.hasNext()) {
						xpointsList.add(in.nextInt());
					}
					in.endArray();
					xpoints = new int[xpointsList.size()];
					for (int i = 0; i < xpointsList.size(); i++) {
						xpoints[i] = xpointsList.get(i);
					}
					break;
				case "ypoints":
					in.beginArray();
					final List<Integer> ypointsList = new ArrayList<>();
					while (in.hasNext()) {
						ypointsList.add(in.nextInt());
					}
					in.endArray();
					ypoints = new int[ypointsList.size()];
					for (int i = 0; i < ypointsList.size(); i++) {
						ypoints[i] = ypointsList.get(i);
					}
					break;
				default:
					break;
			}
		}
		in.endObject();
		if (Objects.isNull(xpoints) || Objects.isNull(ypoints)) {
			throw new JsonParseException("xpoints or ypoints missing");
		}

		return new Polygon(xpoints, ypoints, npoints);
	}
}
