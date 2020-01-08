package helper;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.LinkedList;
import java.util.List;

import ab.vision.ABObject;
import database.Slingshot;
import features.Scene;
import features.SceneInitialisationException;
import features.VisualDebugger;
import meta.ActionRobot;

public class VisionHelper {
	/** Used to loop over an RGB / HSV image */
	public interface HSVFilter {
		/** x and y values are absolute image coordinates, not clipped rect */
		void process(int x, int y, int r, int g, int b);
	}

	static private VisualDebugger DBG = new VisualDebugger("ShotExecutor");
	static{ DBG.enableDebug(false, false); }

	static private int birdsXMax = 0;
	static private int birdsWidthMax = 0;

	/** User function for flood fill algorithm */
	static public abstract class FillProcessing {
		public enum FillStatus {
			HILL_BENEATH_SLINGSHOT
		}

		public FillStatus msg;

		/** Return false to ignore and this pixel */
		public boolean shouldEvaluatePixel(Point px, Point seed, Point offset) {
			return true;
		};

		/** Return true to include pixel in bitmask */
		public boolean threshold(int x, int y, int r, int g, int b) {
			return true;
		};

		/** Params are final object (rect) and all contained pixels (pointCloud) */
		public abstract void result(Rectangle rect, LinkedList<Point> pointCloud);
	}

	/**
	 * Loop over the whole image (or part of it) and call user function on HSV
	 * values.
	 * 
	 * @param crop   Don't use complete image but a clipped segment
	 * @param filter User defined function for processing
	 */
	static public void imageFilter(BufferedImage image, Rectangle crop, HSVFilter filter) {
		for (int y = crop.y; y < crop.y + crop.height; y++) {
			for (int x = crop.x; x < crop.x + crop.width; x++) {
				final int hsv = image.getRGB(x, y);
				final int h = ((hsv & 0x00ff0000) >> 16);
				final int s = ((hsv & 0x0000ff00) >> 8);
				final int v = (hsv & 0x000000ff);
				filter.process(x, y, h, s, v);
			}
		}
	}

	/**
	 * Loop over image and create seeds for flood fill algorithm
	 * 
	 * @param crop      Don't use complete image but a clipped segment
	 * @param processor User defined function
	 */
	static public void loopFillObjects(BufferedImage img, Rectangle crop, FillProcessing processor) {
		// BufferedImage tmp = new BufferedImage(_nWidth, _nHeight,
		// BufferedImage.TYPE_INT_RGB);
		boolean ignorePixel[][] = new boolean[crop.height][crop.width];
		boolean bitmask[][] = new boolean[crop.height][crop.width];
		imageFilter(img, crop, (int x, int y, int r, int g, int b) -> {
			boolean flag = processor.threshold(x, y, r, g, b);
			bitmask[y - crop.y][x - crop.x] = flag;
			ignorePixel[y - crop.y][x - crop.x] = !flag;
			// tmp.setRGB(x, y, (flag ? 0xffffff : 0));
		});
		// DBG.setImage(tmp);
		for (int i = 0; i < crop.height; i++) {
			for (int j = 0; j < crop.width; j++) {
				if (ignorePixel[i][j])
					continue;

				fill(new Point(j, i), crop.getLocation(), ignorePixel, bitmask, processor);
			}
		}
	}

	/**
	 * Start flood fill on single pixel.
	 * 
	 * @param seed        Start point of flood fill
	 * @param offset      Apply offset if using a clipping rect
	 * @param ignorePixel Bitmask to skip certain pixels
	 * @param bitmask     Bitmask to indicate relevant areas
	 * @param proc        User defined function
	 */
	static public void fill(Point seed, Point offset, boolean[][] ignorePixel, boolean[][] bitmask,
			FillProcessing proc) {
		int bHeight = ignorePixel.length;
		int bWidth = ignorePixel[0].length;
		LinkedList<Point> queue = new LinkedList<>();
		LinkedList<Point> pointsinRec = new LinkedList<>();
		Rectangle obj = new Rectangle(offset.x + seed.x, offset.y + seed.y, 0, 0);

		queue.add(seed);
		ignorePixel[seed.y][seed.x] = true;

		while (!queue.isEmpty()) {
			Point p = queue.pop();
			if (!proc.shouldEvaluatePixel(p, seed, offset))
				continue;

			if (bitmask[p.y][p.x]) {
				obj.add(offset.x + p.x, offset.y + p.y);
				pointsinRec.add(new Point(offset.x + p.x, offset.y + p.y));

				// add surrounding points to queue, perform fill
				for (int dy = Math.max(0, p.y - 1); dy < Math.min(bHeight, p.y + 2); dy++) {
					for (int dx = Math.max(0, p.x - 1); dx < Math.min(bWidth, p.x + 2); dx++) {
						if (!ignorePixel[dy][dx]) {
							ignorePixel[dy][dx] = true;
							queue.add(new Point(dx, dy));
						}
					}
				}
			}
		}
		proc.result(obj, pointsinRec);
	}

	static public void waitTillSceneIsStable() {
		long shotTime = System.currentTimeMillis();
		long now = shotTime;
		long isOver = now + 16000; // wait max 16s
		boolean isMoving = true;
		BufferedImage curScr = screenshotWithoutMovingParts();
		BufferedImage prevScr;

		int countZeroChangeFrames = 0;
		while (now < isOver && isMoving) {
			// single loop body takes ~100ms to execute, so no need to further delay using
			// sleep()
			prevScr = curScr;
			curScr = screenshotWithoutMovingParts();
			int dif = ActionRobot.get().pixelDifference(prevScr, curScr);
			now = System.currentTimeMillis();
			if (dif > 100) {
				countZeroChangeFrames = 0; // not stable yet
			} else {
				++countZeroChangeFrames;
				if (countZeroChangeFrames >= 2)
					isMoving = false;
			}
			CustomLogger.info(String.format("[ShotExecutor] now: %1.2fs, pixel difference: %d", 
			(now-shotTime)/1000.0, dif));
		}
		birdsXMax = 0;
		birdsWidthMax = 0;
	}

	static private BufferedImage screenshotWithoutMovingParts() {
		Scene scene;
		try {
			scene = new Scene(ActionRobot.get().doScreenShot(), null, 1.005);
		} catch (SceneInitialisationException e) {
			return ActionRobot.get().screenshotWithoutUI(0);
		}
		Slingshot slingshot = scene.slingshot;
		BufferedImage screenShot = ActionRobot.get().screenshotWithoutUI(slingshot.bounds.x + slingshot.bounds.width);
		Graphics2D g2d = screenShot.createGraphics();
		g2d.setColor(Color.darkGray);
		g2d.fillRect(800, 190, 40, 100); // right wiggling triangle
		try {
			List<ABObject> pigs = scene.getPigs();
			if (pigs != null) {
				for (ABObject pig : pigs) {
					Rectangle r = new Rectangle(pig.getCenter());
					// the eyes of grandpa and helmet are too far apart, thats why 0.9
					r.grow((int)(pig.width * 0.9), (int)(pig.height * 0.85));
					g2d.fillRect(r.x, r.y, r.width, r.height); // because pigs are blinking
				}
			}
		} catch (Exception e){
			CustomLogger.severe("[Level] error masking pigs, returning regular screenshot...");
		}
		try {
			List<ABObject> birds = scene.getBirds();
			if (birds != null) {
				for (ABObject bird : birds) {
					birdsXMax = Math.max(birdsXMax, bird.x);
					birdsWidthMax = Math.max(birdsWidthMax, bird.width);
				}
			}
			if(birdsXMax > slingshot.bounds.x){
				Rectangle r = new Rectangle(birdsXMax, screenShot.getHeight());
				// the eyes of grandpa and helmet are too far apart, thats why 0.9
				r.grow((int)(birdsWidthMax*3), (int)(screenShot.getHeight()));
				g2d.fillRect(r.x, r.y, r.width, r.height); // because pigs are blinking
			}
		} catch (Exception e){
			CustomLogger.severe("[Level] error masking birds, returning regular screenshot...");
		}
		DBG.saveToFileDirectly("preshot-stable-" + DBG.incrementCounter(), screenShot);
		return screenShot;
	}
}
