package de.uniba.sme.bambirds.vision;

import de.uniba.sme.bambirds.common.database.ScreenScale;
import de.uniba.sme.bambirds.common.utils.VisualDebugger;

import java.awt.Point;
import java.awt.Color;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.Comparator;
import java.util.LinkedList;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class VisionSling {
	private static final Logger log = LogManager.getLogger(VisionSling.class);
	private static VisualDebugger DBG = new VisualDebugger("VisionSling");
	{ DBG.enableDebug(false, false); }

	private static final boolean IGNORE_SV_VALUES = false;

	private BufferedImage image;
	private boolean convertedToHSV = false;
	private int _nHeight; // height of the scene
	private int _nWidth; // width of the scene
	private Rectangle sling= null;

	public String identifier = "lvl";

	/**
	 * Custom image processing to convert the RGB color space into HSV color space.
	 * Currently this class supports Slingshot extraction {@link #findSlingshot()}
	 * @param img Screenshot of the scene
	 */
	public VisionSling(final BufferedImage img) {
		_nHeight = img.getHeight();
		_nWidth = img.getWidth();
		image = img;
		convertToHSV();
	}

	private void convertToHSV() {
		if (!convertedToHSV) {
			BufferedImage tmp = new BufferedImage(_nWidth, _nHeight, BufferedImage.TYPE_INT_RGB);
			for (int y = 0; y < _nHeight; y++) {
				for (int x = 0; x < _nWidth; x++) {
					tmp.setRGB(x, y, rgb2hsv(image.getRGB(x, y)));
				}
			}
			image = tmp;
			convertedToHSV = true;
		}
	}

	/**
	 * Runs image processing on the image to find the slingshot.
	 * @return Bounding box of sling
	 */
	public Rectangle findSlingshot() {

		if(sling != null) {
			return sling;
		}

		convertToHSV();
		// coarse search
		Rectangle fullscreen = new Rectangle(0, 0, _nWidth - _nWidth / 3, _nHeight);
		LinkedList<Rectangle> coarse = findSlingshot(fullscreen, 18, 60, 0.4f);
		log.debug(coarse.toString());
		Rectangle clip = null;
		Rectangle nextClip = null;
		while (coarse.size() > 0) {
			nextClip = coarse.pop();
			if (clip ==null) {
				clip = nextClip;
			}
			if (nextClip.x > 20 && nextClip.x < clip.x) clip = nextClip;
			else if (clip.x < 20) clip = null;
		}
		if (clip == null)
			return null;
		// fine search, keep bottom fixed for new bounds
		clip.translate(-10, -2);
		clip.setSize(10 + Math.max(clip.height / 2, clip.width * 3), clip.height + 2);
		LinkedList<Rectangle> fine = findSlingshot(clip, 60, 130, 0.6f);
		if (fine.size() < 1)
			return null;

		sling = fine.getFirst();

		// replace height if ratio doesnt match
		double h = ScreenScale.SLINGSHOT.ratioH7W() * sling.width;
		if (Math.abs(sling.height - h) > 7) {
			log.debug("previous slingshot height was " + sling.height
					+ " now setting it to " + (int) Math.round(h));
			sling.height = (int) Math.round(h);
		}
		return sling;
	}

	/**
	 * Internal method to apply a two level hue threshold and pixel grouping.
	 * This method is called twice (two level). Use public function {@link #findSlingshot()} instead.
	 * @param crop Don't use complete image but a clipped segment
	 * @param threshold Hue value threshold (colors darken then threshold)
	 * @param minPoints Number of points to be at least in one bounding box
	 * @param pxPercentage Pixel to bounding box fill ration. (Sling has roughly half of it's px set)
	 * @return Bounding box of slingshot
	 */
	private LinkedList<Rectangle> findSlingshot(Rectangle crop, int threshold, int minPoints, float pxPercentage) {

		DBG.visualOutputEnabled = (threshold == 60);
		DBG.setBlack();

		LinkedList<Rectangle> objList = new LinkedList<>();
		VisionHelper.loopFillObjects(image, crop, new VisionHelper.FillProcessing() {
			@Override public boolean threshold(int x, int y, int h, int s, int v) {
				return (h < threshold && h > 10);
			}
			@Override public boolean shouldEvaluatePixel(Point px, Point seed, Point offset) {
				if (px.x < (seed.x - 10)) {
					msg = FillStatus.HILL_BENEATH_SLINGSHOT; // contour of hill beneath sling
					return false;
				}
				DBG.drawPoint(px.x + offset.x, px.y + offset.y, 0xFFFFFF);
				return true;
			}
			@Override public void result(Rectangle rect, LinkedList<Point> pointCloud) {
				float pxFillRatio = pointCloud.size() / (float) ((rect.width + 1) * (rect.height + 1));
				if (pointCloud.size() > minPoints && pxFillRatio < pxPercentage
						&& rect.height > 2 * rect.width) {
					// FIXME: once the trajectory improvement is implemented, remove 'h > 2 * w' contraint (robustness)
					// can be only done with a scale measurement other than the sling
					if (msg == FillStatus.HILL_BENEATH_SLINGSHOT) {
						rect.height -= 1; // hills have one black contour line at the top
					}
					objList.add(rect);
					DBG.drawBoundingBox(rect, Color.green);
				}
			}
		});

		DBG.drawBoundingBox(crop, Color.blue);
		DBG.saveToFile(String.format("%s_slingshot_%d", identifier, threshold));

		objList.sort(Comparator.comparingInt(rect -> -rect.height));
		return objList;
	}

	/**
	 * Color conversion RGB -> HSV
	 * @param rgb Integer value with last 24 bits containing all three colors
	 * @return 24 bit integer value with hue, saturation and value
	 */
	private int rgb2hsv(int rgb) {
		int red   = ((rgb & 0x00ff0000) >> 16);
		int green = ((rgb & 0x0000ff00) >> 8);
		int blue  =  (rgb & 0x000000ff);

		float r_prime = red / 255.0f;
		float g_prime = green / 255.0f;
		float b_prime = blue / 255.0f;
		float cmax = Math.max(Math.max(r_prime, g_prime), b_prime);
		float cmin = Math.min(Math.min(r_prime, g_prime), b_prime);
		float delta = cmax - cmin;

		float h = 0;
		if (delta < 0.00001f)
			h = 0;
		else if (cmax == r_prime)
			h = 60 * (((g_prime - b_prime) / delta) % 6);
		else if (cmax == g_prime)
			h = 60 * (((b_prime - r_prime) / delta) + 2);
		else if (cmax == b_prime)
			h = 60 * (((r_prime - g_prime) / delta) + 4);

		if( h < 0.0f )
			h += 360.0f;
		int c = (int)(h / 360.0f * 255);

		if (IGNORE_SV_VALUES) {
			return c << 16 | c << 8 | c;
		} else {
			int s = 0;
			if (cmax > 0)
				s = (int)((delta / cmax) * 255);
			int v = (int)(cmax * 255);

			return c << 16 | s << 8 | v;
		}
	}
}
