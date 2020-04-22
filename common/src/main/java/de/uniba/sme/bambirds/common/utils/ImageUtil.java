package de.uniba.sme.bambirds.common.utils;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.awt.Point;
import java.awt.Image;
import java.awt.image.ColorModel;
import java.awt.image.WritableRaster;
import java.io.File;
import java.util.List;

import javax.imageio.ImageIO;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;

/**
 * ImageUtil
 */
public class ImageUtil {
	private static final Logger log = LogManager.getLogger(ImageUtil.class);

	/** @return Number of not identical pixel */
	public static int pixelDifference(BufferedImage imgA, BufferedImage imgB) {
		int height = Math.min(imgA.getHeight(), imgB.getHeight());
		int width = Math.min(imgA.getWidth(), imgB.getWidth());
		int diff = imgA.getWidth() * imgA.getHeight() + imgB.getWidth() * imgB.getHeight() - 2 * width * height;
		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {
				if (imgA.getRGB(x, y) != imgB.getRGB(x, y))
					++diff;
			}
		}
		return diff;
	}

	/**
	 * Masks the UI of Angry Birds
	 * 
	 * @param screenShot the screenshot to be masked
	 * @param cutoffX    Everything left of this value will be masked aswell.
	 *                   Default: 0 without masking
	 * @return a copy of the original Image with the UI masked
	 */
	public static BufferedImage removeABUI(BufferedImage screenShot, int cutoffX) {
		BufferedImage copy = deepCopy(screenShot);
		Graphics2D g2d = copy.createGraphics();
		g2d.setColor(Color.darkGray);
		g2d.fillRect(0, 0, 205, 70); // pause, retry, eagle button
		g2d.fillRect(630, 18, 205, 70); // high score
		if (cutoffX > 0)
			g2d.fillRect(0, 0, cutoffX, 480); // everything left of slingshot
		else
			g2d.fillRect(17, 374, 21, 84); // zoom buttons

		// special handling for right triangle menu
		int x[] = { 835, 806, 835 };
		int y[] = { 198, 241, 280 };
		g2d.fillPolygon(x, y, 3);
		return copy;
	}

	public static BufferedImage deepCopy(BufferedImage bi) {
		ColorModel cm = bi.getColorModel();
		boolean isAlphaPremultiplied = cm.isAlphaPremultiplied();
		WritableRaster raster = bi.copyData(null);
		return new BufferedImage(cm, raster, isAlphaPremultiplied, null);
	}

	public static BufferedImage drawPoint(BufferedImage img, int x, int y, int color) {
		Rectangle bounds = new Rectangle(img.getWidth(), img.getHeight());
		if (bounds.contains(x, y))
			img.setRGB(x, y, color);
		return img;
	}

	public static BufferedImage drawPoint(BufferedImage img, Point2D.Double p, int color) {
		return drawPoint(img, (int) Math.round(p.x), (int) Math.round(p.y), color);
	}

	public static BufferedImage drawPoints(BufferedImage img, List<Point2D.Double> ptList, int color) {
		for (Point2D.Double p : ptList)
			drawPoint(img, p, color);
		return img;
	}

	public static BufferedImage drawTarget(BufferedImage img, int x, int y, Color color) {
		Graphics2D g2d = img.createGraphics();
		g2d.setColor(color);
		g2d.drawLine(x - 5, y, x + 5, y);
		g2d.drawLine(x, y - 5, x, y + 5);
		return img;
	}

	public static BufferedImage drawTarget(BufferedImage img, Point2D.Double p, Color color) {
		return drawTarget(img, (int) Math.round(p.x), (int) Math.round(p.y), color);
	}

	public static BufferedImage drawBoundingBox(BufferedImage img, Rectangle rect, Color color) {
		Graphics2D g2d = img.createGraphics();
		g2d.setColor(color);
		g2d.drawRect(rect.x - 1, rect.y - 1, rect.width + 1, rect.height + 1);
		return img;
	}

	public static BufferedImage drawQuadraticWithOffset(BufferedImage img, double[] weights, Point2D.Double offset, int color) {
		Rectangle bounds = new Rectangle(img.getWidth(), img.getHeight());
		for (int x = 0; x < img.getWidth(); x++) {
			double y = weights[0] * x * x + weights[1] * x + weights[2];
			Point p = new Point((int) Math.round(x + offset.x), (int) Math.round(offset.y - y));
			if (bounds.contains(p))
				img.setRGB(p.x, p.y, color);
		}
		return img;
	}

	public static BufferedImage drawQuadratic(BufferedImage img, double[] weights, int color) {
		Rectangle bounds = new Rectangle(img.getWidth(), img.getHeight());
		for (int x = 0; x < img.getWidth(); x++) {
			int y = (int) (weights[0] * x * x + weights[1] * x + weights[2]);
			if (bounds.contains(x, y))
				img.setRGB(x, y, color);
		}
		return img;
	}

	public static void saveToFile(BufferedImage img, String filename) {
		try {
			File f = new File(String.format("debug/%s.png", filename));
			f.getParentFile().mkdirs();
			ImageIO.write(img, "png", f);
		} catch (Exception e) {
			log.error("Error while writing image: " + e.getMessage(), e);
		}
	}

	/**
	 * highlight regions with a given id
	 * @param img
	 * @param regions
	 * @param regionId
	 * @param fgColour
	 * @return
	 */ 
	public static BufferedImage highlightRegions(Image img, int[][] regions,
		int regionId, Color fgColour) {
		BufferedImage canvas = new BufferedImage(img.getWidth(null),
				img.getHeight(null), BufferedImage.TYPE_INT_ARGB);
		Graphics2D g2d = canvas.createGraphics();
		g2d.drawImage(img, 0, 0, null);
		g2d.setColor(fgColour);
		for (int y = 0; y < regions.length; y++) {
			for (int x = 0; x < regions[y].length; x++) {
				if (regions[y][x] == regionId) {
					g2d.drawRect(x, y, 1, 1);
				}
			}
		}

		return canvas;
	}

}