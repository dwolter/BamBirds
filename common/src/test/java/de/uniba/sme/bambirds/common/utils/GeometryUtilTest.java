package de.uniba.sme.bambirds.common.utils;


import org.junit.jupiter.api.Test;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class GeometryUtilTest {

	@Test
	public void bresenhamHorizontal() {
		Point a = new Point(0, 0);
		Point b = new Point(0, 2);

		List<Point> expected = new ArrayList<>();
		expected.add(a);
		expected.add(new Point(0, 1));
		expected.add(b);

		List<Point> actual = GeometryUtil.bresenham(a, b);

		assertEquals(expected, actual);

	}

	@Test
	public void bresenhamVertical() {
		Point a = new Point(0, 0);
		Point b = new Point(2, 0);

		List<Point> expected = new ArrayList<>();
		expected.add(a);
		expected.add(new Point(1, 0));
		expected.add(b);

		List<Point> actual = GeometryUtil.bresenham(a, b);

		assertEquals(expected, actual);
	}


	@Test
	public void bresenhamDiagonal() {
		Point a = new Point(0, 0);
		Point b = new Point(2, 2);

		List<Point> expected = new ArrayList<>();
		expected.add(a);
		expected.add(new Point(1, 1));
		expected.add(b);

		List<Point> actual = GeometryUtil.bresenham(a, b);

		assertEquals(expected, actual);
	}
}
