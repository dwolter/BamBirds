package de.uniba.sme.bambirds.common.objects;


import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.LineSegment;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import de.uniba.sme.bambirds.common.objects.ab.shape.Poly;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;

import com.google.gson.Gson;
import org.junit.jupiter.api.Test;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class SerializationTest {

	@Test
	public void ABObjectSerialization(){
		Rectangle rect = new Rectangle(100,100,50,50);
		ABObject object = new ABObject(rect,ABType.Stone);
		object.globalID ="stone0";

		Gson gson = new Gson();
		String json = gson.toJson(object);
		System.out.println(json);
		assertTrue(json.contains("stone0"));
	}

	@Test
	public void RectSerialization(){
		ABObject object = new Rect(100,100,50,50,10,ABType.Wood);
		object.globalID ="wood0";

		Gson gson = new Gson();
		String json = gson.toJson(object);
		System.out.println(json);
		assertTrue(json.contains("wood0"));
	}

	@Test
	public void CircleSerialization(){
		ABObject object = new Circle(100,100,20,ABType.Ice);
		object.globalID ="ice0";

		Gson gson = new Gson();
		String json = gson.toJson(object);
		System.out.println(json);
		assertFalse(json.isEmpty());
		assertTrue(json.contains("ice0"));
	}

	@Test
	public void PolygonSerialization(){
		// Create Dummy LineSegments
		List<LineSegment> lineSegments = new ArrayList<LineSegment>();
		LineSegment seg = new LineSegment(new Point(50,50), 1);;
		lineSegments.add(seg);
		seg = new LineSegment(new Point(70,50), 1);
		lineSegments.add(seg);
		seg = new LineSegment(new Point(75,80), 1);
		lineSegments.add(seg);
		seg = new LineSegment(new Point(50,50), 1);
		lineSegments.add(seg);


		ABObject object = new Poly(lineSegments,0,0,ABType.Ice,70,55);
		object.globalID ="ice1";

		Gson gson = new Gson();
		String json = gson.toJson(object);
		System.out.println(json);
		assertTrue(json.contains("ice1"));
	}

}