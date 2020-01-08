package features;

import ab.demo.other.ActionRobot;
import ab.demo.other.ClientActionRobot;
import ab.vision.ABObject;
import ab.vision.ABType;
import ab.vision.Vision;
import ab.vision.VisionUtils;
import ab.vision.real.shape.Circle;
import ab.vision.real.shape.Rect;
import database.ScreenScale;
import database.Slingshot;
import helper.CustomLogger;
import shot.SavedShot;
import shot.ShotPlanner;

import java.awt.Color;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.lang.Math;

import static features.SceneInitialisationException.Reason.*;

/**
 * The {@code Scene} class is basically a simple wrapper around the {@link Vision} class.
 * It provides a few simple methods to suit the needs of the <i>BamBird</i> agent and
 * hides the many {@code Vision} methods we don't care about.
 * <p>
 * When you instantiate a {@code Scene} object, make sure you catch the {@code NullPointerException}
 * it may throw if the given {@code BufferedImage} does not contain some critical objects.
 */
public class Scene {
	public Slingshot slingshot;
	public double scalingFactor = 1.005;
	private List<ABObject> pigs;
	private List<ABObject> birds;
	private List<ABObject> hills;
	private List<ABObject> blocks;
	private List<ABObject> tnts;
	private List<ABObject> _allObjects;
	private List<SavedShot> savedShots;
	private int groundPlaneY = -1;

	static private VisualDebugger DBG = new VisualDebugger("Scene");
	{ DBG.enableDebug(false, true); }

	/**
	 * Stores all objects in {@code img} that are recognized by the {@code find...} methods of the {@link Vision} class.
	 *
	 * @param img A screenshot from {@link ClientActionRobot#doScreenShot}
	 * @param sling If {@code null} invoke {@code Vision} to determine
	 * @param scalingFactor If {@code 0.0} use default scaling factor
	 * @throws NullPointerException if {@code Vision} cannot find a sling in {@code img}
	 * @see Vision#findSlingshotMBR
	 */
	public Scene(BufferedImage img, Slingshot sling, double scalingFactor) throws SceneInitialisationException {
		this.slingshot = sling;
		if (this.scalingFactor != 0.0)
			this.scalingFactor = scalingFactor;
		Vision vision = new Vision(img);
		if (this.slingshot == null) {
			CustomLogger.info("[Scene] Using VisionMBR to detect slingshot.");
			Rectangle slingRect = vision.findSlingshotMBR();
			if (slingRect == null) throw new SceneInitialisationException(NO_SLING_FOUND);
			this.slingshot = new Slingshot(slingRect, null);
			CustomLogger.info("[Scene] found: " + this.slingshot);
		}

		this.pigs = vision.findPigsRealShape();
		if (pigs == null || this.pigs.size() == 0) throw new SceneInitialisationException(NO_PIGS_FOUND);

		this.birds = vision.findBirdsRealShape();
		if (birds == null || this.birds.size() == 0) throw new SceneInitialisationException(NO_BIRDS_FOUND);

		birds.sort(new Comparator<ABObject>() {
            @Override
            public int compare(ABObject bird1, ABObject bird2) {
                double dist1 = bird1.getCenter().distance(slingshot.bounds.getLocation());
                double dist2 = bird2.getCenter().distance(slingshot.bounds.getLocation());
                return (int)(dist1-dist2);
            }
        });  // bird on sling (one with shortest distance to sling)
		this.hills = vision.findHills();
		this.blocks = vision.findBlocksRealShape();
		this.tnts = vision.findTNTs();
        
        this.groundPlaneY = vision.findGroundPlane((int)Math.round(this.slingshot.getPivotPoint().getY()));

		generateIds();
		_allObjects = mergeAllObjectsInOneList();
	}

	/**
	 * Returns a semi-sorted list of birds. The first bird is the one on the sling.
	 * The rest of the birds are <i>not</i> ordered. <p>
	 * The same sorting logic as in {@link ActionRobot#getBirdTypeOnSling()} is used to sort this list.
	 */
	public List<ABObject> getBirds() { return new ArrayList<>(birds); }
	public List<ABObject> getPigs() { return new ArrayList<>(pigs); }
	public List<ABObject> getHills() { return new ArrayList<>(hills); }
	public List<ABObject> getBlocks() { return new ArrayList<>(blocks); }
	public List<ABObject> getTnts() { return new ArrayList<>(tnts); }
	public List<ABObject> getAllObjects() { return new ArrayList<>(_allObjects); }
    public int getGroundPlane() { return this.groundPlaneY; }

	private void generateIds() {
		generateIDsPerList(birds);
		generateIDsPerList(blocks);
		generateIDsPerList(hills);
		generateIDsPerList(pigs);
		generateIDsPerList(tnts);
	}

	private void generateIDsPerList(List<ABObject> list) {
		int counter = 0;
		for (ABObject obj : list)
			obj.globalID = obj.getType().toString().toLowerCase() + counter++;
	}

	private List<ABObject> mergeAllObjectsInOneList() {
		List<ABObject> allBlocks = new ArrayList<>();
		try { allBlocks.addAll(birds); } catch (Exception ignored) {}
		try { allBlocks.addAll(sortedTargetsList()); } catch (Exception ignored) {}
		return allBlocks;
	}

	public List<ABObject> sortedTargetsList() {
		List<ABObject> allBlocks = new ArrayList<>();
		try { allBlocks.addAll(pigs); } catch (Exception ignored) {}
		try { allBlocks.addAll(hills); } catch (Exception ignored) {}
		try { allBlocks.addAll(blocks); } catch (Exception ignored) {}
		try { allBlocks.addAll(tnts); } catch (Exception ignored) {}
		allBlocks.sort(Comparator.comparingInt(o -> o.x));
		return allBlocks;
	}

	public int estimateMaximalPointsWithoutBirds() {
		int points = 0;
		for (ABObject o : _allObjects) {
			switch (o.getType()) {
				case Pig:   points += 5300; break;
				case Wood:  points += 700; break;
				case Ice:   points += 650; break;
				case Stone: points += 800; break;
				default: break;
			}
		}
		return points;// + birds.size() * 10000;
	}

	private double estimateScreenScale() {
		double sum = 0;
		int count = 0;
		for (ABObject b : birds) {
			if (!(b instanceof Circle)) continue;
			sum += ScreenScale.birds[b.type.id].radius / ((Circle) b).r;
			count++;
		}
		return sum / count;
	}

	/**
	 * Search for an ID in all objects in the scene an return the matching object.
	 * @param id the ID of an object (e.g. {@code pig2})
	 * @return an object with ID {@code id} or {@code null} if no matching object is found
	 */
	public ABObject findObjectWithID(String id) {
		for (ABObject o : _allObjects)
			if (o.globalID.equals(id)) return o;
		return null;
	}

	public int getCountSavedShots() {
		return this.savedShots.size();
	}

	public List<SavedShot> getReachableTargets() {
		if (savedShots == null)
			return new ArrayList<>();
		return new ArrayList<>(savedShots);
	}

	public void setReachabilityForAllBlocks(ShotPlanner planner) {
		if (birds == null || birds.isEmpty())
			return;
		double padding = birds.get(0).getHeight() / 2;
		List<ABObject> allBlocks = sortedTargetsList();
		savedShots = new ArrayList<>();
//		DBG.doScreenshot();
		double screenScale = estimateScreenScale();
		for (ABObject obj : allBlocks) {
			List<Point> targetPoints = new LinkedList<>();
			targetPoints.add(obj.getCenter());

			if (obj instanceof Rect) {
				Rect r = ((Rect) obj);
				double len = r.getpLength() / 2;
				double dy = len * Math.sin(r.angle);
				double dx = len * Math.cos(r.angle);
				if (2 * len * screenScale > 50) {
					targetPoints.add(new Point((int)(r.centerX - dx), (int)(r.centerY - dy)));
					targetPoints.add(new Point((int)(r.centerX + dx), (int)(r.centerY + dy)));
					if (2 * len * screenScale > 100) {
						targetPoints.add(new Point((int)(r.centerX - dx/2), (int)(r.centerY - dy/2)));
						targetPoints.add(new Point((int)(r.centerX + dx/2), (int)(r.centerY + dy/2)));
					}
				}
			}
			for (Point tp : targetPoints) {
//				DBG.drawTarget(tp.x, tp.y, Color.RED);
				savedShots.addAll(planner.savedShotsForTarget(new Point2D.Double(tp.x, tp.y), obj, allBlocks, padding));
			}
		}
	}

	public String sceneDescription() {
		String birds = "";
		int miny = 1000;
		String birdInSling = "-";

		for (ABObject bird : this.birds) {
			String thisBird = "";
			switch (bird.getType()) {
				case RedBird:    thisBird = "R"; break;
				case YellowBird: thisBird = "Y"; break;
				case BlueBird:   thisBird = "B"; break;
				case BlackBird:  thisBird = "K"; break;
				case WhiteBird:  thisBird = "W"; break;
				default: break;
			}
			birds += thisBird;
			if (bird.getY() < miny) {
				miny = (int) bird.getY();
				birdInSling = thisBird;
			}
		}
		return "Sl:" + birdInSling + " B:" + birds + " P:" + this.pigs.size()
				+ " I:" + getNumberOfSpecBlocks(ABType.Ice) + " W:" + getNumberOfSpecBlocks(ABType.Wood)
				+ " S:" + getNumberOfSpecBlocks(ABType.Stone) + " T:" + this.tnts.size();
	}

	private long getNumberOfSpecBlocks(ABType typeOfBlock) {
		return this.blocks.stream().filter(o -> o.getType() == typeOfBlock).count();
	}

	public void writeScreenshotWithIDs(String filename) {
		if (DBG.canSaveFile()) {
			writeScreenshotWithIDs(filename, pigs, "pigs", Color.RED);
			writeScreenshotWithIDs(filename, blocks, "blocks", Color.MAGENTA);
			writeScreenshotWithIDs(filename, hills, "hills", Color.ORANGE);
			List<ABObject> slingshotList = new ArrayList<>();
			slingshotList.add(new ABObject(slingshot.bounds, ABType.Sling));
			writeScreenshotWithIDs(filename, slingshotList, "slingshot", Color.BLUE);
			writeScreenshotWithIDs(filename, birds, "birds", Color.BLUE);
		}
	}

	private void writeScreenshotWithIDs(String filename, List<ABObject> objects, String type, Color color) {
		BufferedImage tmp = meta.ActionRobot.get().doScreenShot();
		VisionUtils.drawBoundingBoxesWithID(tmp, objects, color);
		DBG.saveToFileDirectly(filename + "-" + type, tmp);
	}

	@Override
	public String toString() {
		return sceneDescription();
	}
}
