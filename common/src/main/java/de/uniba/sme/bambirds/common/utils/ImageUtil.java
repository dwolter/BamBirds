package de.uniba.sme.bambirds.common.utils;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

/**
 * Utilities for interacting with images.
 */
public final class ImageUtil {
	private static final Logger LOG = LogManager.getLogger(ImageUtil.class);

	private ImageUtil() {
	}

	/**
	 * @param imgA first image
	 * @param imgB second image
	 * @param pixelThreshold Threshold of difference between colors
	 * @return Number of not identical pixel in imgA and imgB
	 */
	public static int pixelDifference(BufferedImage imgA, BufferedImage imgB, int pixelThreshold) {
		return pixelDifference(imgA, imgB, null, pixelThreshold);
	}

	/**
	 * @param imgA first image
	 * @param imgB second image
	 * @param changedPixels number of overall changed pixels during shot
	 * @param pixelThreshold Threshold of difference between colors
	 * @return Number of not identical pixel in imgA and imgB
	 */
	public static int pixelDifference(final BufferedImage imgA, final BufferedImage imgB, final BufferedImage changedPixels, final int pixelThreshold) {
		Color colorBlack = new Color(0, 0, 0);
		int height = Math.min(imgA.getHeight(), imgB.getHeight());
		int width = Math.min(imgA.getWidth(), imgB.getWidth());
		int diff = imgA.getWidth() * imgA.getHeight() + imgB.getWidth() * imgB.getHeight() - 2 * width * height;
		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {
				if (imgA.getRGB(x, y) != imgB.getRGB(x, y) && diffExceedsValue(new Color(imgA.getRGB(x, y)), new Color(imgB.getRGB(x, y)), pixelThreshold)) {
					++diff;
					if (changedPixels != null) {
						//System.out.println("Pixel x: "+x+" and Pixel y: "+y+" are different");
						changedPixels.setRGB(x, y, colorBlack.getRGB());
					}
				}
			}
		}
		return diff;
	}

	/** @return Changed pixels buffered image without levt part completely black
	 * @param changedPixels Image before the correction */
	public static BufferedImage removeBlackPartOnSlingshot(BufferedImage changedPixels) {
		Color colorWhite = new Color(255, 255, 255);
		int height = Math.min(changedPixels.getHeight(), changedPixels.getHeight());
		int width = Math.max(changedPixels.getWidth(), changedPixels.getWidth());
		for (int y = 0; y < height; y++) {
			for (int x = 0; x < 300; x++) {
				changedPixels.setRGB(x, y, colorWhite.getRGB());
			}
		}
		return changedPixels;
	}

	/** @return whether total number of diff pixels exceeds threshold */
	private static boolean diffExceedsValue(Color imgA_RGB, Color imgB_RGB, int pixelThreshold) {
		int redDiff = Math.abs(imgA_RGB.getRed()-imgB_RGB.getRed());
		int greenDiff = Math.abs(imgA_RGB.getGreen()-imgB_RGB.getGreen());
		int blueDiff = Math.abs(imgA_RGB.getBlue()-imgB_RGB.getBlue());
		if ((redDiff + greenDiff + blueDiff) > pixelThreshold){
			//System.out.println("diff in red is "+redDiff+" diff in green is "
			//		+greenDiff+" diff in blue is "+blueDiff);
			return true;
		}
		else return false;
	}

	/** Performs erosion operation on black and white diff image */
	public static void erodeDiffImage (){
		//TODO implement or add opencv in python and remove
		/*String imagePath = args.length > 0 ? args[0] : "../data/LinuxLogo.jpg";
		matImgSrc = Imgcodecs.imread(imagePath);
		if (matImgSrc.empty()) {
			System.out.println("Empty image: " + imagePath);
			System.exit(0);
		}
		Mat element = Imgproc.getStructuringElement(elementType, new Size(2 * kernelSize + 1, 2 * kernelSize + 1),
				new Point(kernelSize, kernelSize));
		Imgproc.erode(matImgSrc, matImgDst, element);*/

	}

	/**
	 * Masks the UI of Angry Birds
	 *
	 * @param screenShot the screenshot to be masked
	 * @param cutoffX    Everything left of this value will be masked as well.
	 *                   Default: 0 without masking
	 * @return a copy of the original Image with the UI masked
	 */
	public static BufferedImage removeABUI(final BufferedImage screenShot, final int cutoffX) {
		BufferedImage copy = deepCopy(screenShot);
		Graphics2D g2d = copy.createGraphics();
		g2d.setColor(Color.darkGray);
		g2d.fillRect(0, 0, 205, 70); // pause, retry, eagle button
		g2d.fillRect(630, 18, 205, 70); // high score
		if (cutoffX > 0) {
			g2d.fillRect(0, 0, cutoffX, Settings.IMAGE_HEIGHT); // everything left of slingshot
		} else {
			g2d.fillRect(17, 374, 21, 84); // zoom buttons
		}
		// special handling for right triangle menu
		int[] x = {835, 806, 835};
		int[] y = {198, 241, 280};
		g2d.fillPolygon(x, y, 3);
		return copy;
	}

	/**
	 * Clips the image with the given values and removes UI.
	 * @param image image to clip
	 * @param cutoff xLeft, xRight, yTop, yBottom in respective x and y values
	 * @return Clipped image without UI.
	 */
	public static BufferedImage removeABUIAndClip(BufferedImage image, int[] cutoff) {
		if(cutoff[0] < 0 || cutoff[0] > image.getWidth()){
			cutoff[0] = 0;
		}
		if(cutoff[1] < 0 || cutoff[1] > image.getWidth()){
			cutoff[1] = image.getWidth();
		}
		if(cutoff[2] < 0 || cutoff[2] > image.getHeight()){
			cutoff[2] = 0;
		}
		if(cutoff[3] < 0 || cutoff[3] > image.getHeight()){
			cutoff[3] = image.getHeight();
		}
		int width = cutoff[1]-cutoff[0];
		int height = cutoff[3]-cutoff[2];
		if(width == image.getWidth() && height == image.getHeight()){
			return deepCopy(image);
		}
		BufferedImage imageCopy = image.getSubimage(cutoff[0],cutoff[2],width,height);
		Graphics2D g2d = imageCopy.createGraphics();
		g2d.setColor(Color.darkGray);
		g2d.fillRect(0-cutoff[0], 0-cutoff[2], 205, 70); // pause, retry, eagle button
		g2d.fillRect(630-cutoff[0], 18-cutoff[2], 205, 70); // high score
		g2d.fillRect(17-cutoff[0], 374-cutoff[2], 21, 84); // zoom buttons
		// special handling for right triangle menu
		int x[] = { 835-cutoff[0], 806-cutoff[0], 835-cutoff[0]};
		int y[] = { 198-cutoff[2], 241-cutoff[2], 280-cutoff[2]};
		g2d.fillPolygon(x, y, 3);
		g2d.dispose();
		BufferedImage newImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
		Graphics2D graphic = newImage.createGraphics();
		graphic.drawImage(imageCopy, 0,0,null);
		graphic.dispose();
		return newImage;
	}

	public static BufferedImage deepCopy(final BufferedImage bi) {
		ColorModel cm = bi.getColorModel();
		boolean isAlphaPremultiplied = cm.isAlphaPremultiplied();
		WritableRaster raster = bi.copyData(null);
		return new BufferedImage(cm, raster, isAlphaPremultiplied, null);
	}

	public static void drawPoint(final BufferedImage img, final int x, final int y, final int color) {
		Rectangle bounds = new Rectangle(img.getWidth(), img.getHeight());
		if (bounds.contains(x, y)) {
			img.setRGB(x, y, color);
		}
	}

	public static void drawPoint(final BufferedImage img, final Point2D.Double p, final int color) {
		drawPoint(img, (int) Math.round(p.x), (int) Math.round(p.y), color);
	}

	public static void drawPoints(final BufferedImage img, final List<Point2D.Double> ptList, final int color) {
		for (Point2D.Double p : ptList) {
			drawPoint(img, p, color);
		}
	}

	public static void drawTarget(final BufferedImage img, final int x, final int y, final Color color) {
		Graphics2D g2d = img.createGraphics();
		g2d.setColor(color);
		g2d.drawLine(x - 5, y, x + 5, y);
		g2d.drawLine(x, y - 5, x, y + 5);
	}

	public static void drawTarget(final BufferedImage img, final Point2D.Double p, final Color color) {
		drawTarget(img, (int) Math.round(p.x), (int) Math.round(p.y), color);
	}

	public static void drawBoundingBox(final BufferedImage img, final Rectangle rect, final Color color) {
		Graphics2D g2d = img.createGraphics();
		g2d.setColor(color);
		g2d.drawRect(rect.x - 1, rect.y - 1, rect.width + 1, rect.height + 1);
	}

	public static void drawQuadraticWithOffset(final BufferedImage img, final double[] weights, final Point2D.Double offset, final int color) {
		Rectangle bounds = new Rectangle(img.getWidth(), img.getHeight());
		for (int x = 0; x < img.getWidth(); x++) {
			double y = weights[0] * x * x + weights[1] * x + weights[2];
			Point p = new Point((int) Math.round(x + offset.x), (int) Math.round(offset.y - y));
			if (bounds.contains(p)) {
				img.setRGB(p.x, p.y, color);
			}
		}
	}

	public static void drawQuadratic(final BufferedImage img, final double[] weights, final int color) {
		Rectangle bounds = new Rectangle(img.getWidth(), img.getHeight());
		for (int x = 0; x < img.getWidth(); x++) {
			int y = (int) (weights[0] * x * x + weights[1] * x + weights[2]);
			if (bounds.contains(x, y)) {
				img.setRGB(x, y, color);
			}
		}
	}

	/**
	 * Save an image to a file.
	 *
	 * @param img        a buffered Image to save
	 * @param filename   the name of the file
	 * @param folderPath path to the file without a trailing slash
	 * @return the file object where the file is saved or null if it didn't succeed
	 */
	public static File saveToFile(final BufferedImage img, final String filename, final Path folderPath) {
		try {
			File f = folderPath.resolve(filename + ".png").toFile();
			if (!f.getParentFile().mkdirs() && !f.getParentFile().exists()) {
				throw new IOException("failed to create parent directories to " + f.getPath());
			}
			if (ImageIO.write(img, "png", f)) {
				return f;
			}
		} catch (Exception e) {
			LOG.error("Error while writing image: " + e.getMessage(), e);
		}
		return null;
	}

	/**
	 * Save an image to the default debug folder.
	 *
	 * @param img      a buffered Image to save
	 * @param filename the name of the file
	 * @return the file object where the file is saved or null if it didn't succeed
	 */
	public static File saveToDebugFile(final BufferedImage img, final String filename) {
		return saveToFile(img, filename, Settings.DEBUG_DIR);
	}

	/**
	 * Save an image to the default temp folder.
	 *
	 * @param img      a buffered Image to save
	 * @param filename the name of the file
	 * @return the file object where the file is saved or null if it didn't succeed
	 */
	public static File saveToTempFile(final BufferedImage img, final String filename) {
		return saveToFile(img, filename, Settings.TEMP_DIR);
	}

	/**
	 * highlight regions with a given id.
	 *
	 * @param img      Image to highlight
	 * @param regions  the regions to highlight
	 * @param regionId the id of the region
	 * @param fgColour the colour for highlighting
	 * @return the Image with highlighted regions
	 */
	public static BufferedImage highlightRegions(final Image img, final int[][] regions,
																							 final int regionId, final Color fgColour) {
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
