package de.uniba.sme.bambirds.common.objects;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.awt.image.BufferedImage;
import java.awt.geom.Point2D;

import de.uniba.sme.bambirds.common.database.ScreenScale;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.Slingshot;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;
import de.uniba.sme.bambirds.common.utils.ImageUtil;

/**
 * Abstract Scene is a Class for defining the general behaviour of scenes. It
 * does not by itself analyse the given Image.
 */
public abstract class AbstractScene {

	protected Slingshot slingshot;
	public double scalingFactor = 1.005;
	protected List<ABObject> pigs;
	protected List<ABObject> birds;
	protected List<ABObject> hills;
	protected List<ABObject> blocks;
	protected List<ABObject> tnts;
	protected List<ABObject> _allObjects;
	protected List<SavedShot> savedShots;
	protected int groundPlaneY = -1;

	protected BufferedImage image;

	public AbstractScene(BufferedImage image) {
		super();
		this.image = image;
		pigs = new ArrayList<>();
		birds = new ArrayList<>();
		hills = new ArrayList<>();
		blocks = new ArrayList<>();
		tnts = new ArrayList<>();
		_allObjects = new ArrayList<>();
		savedShots = new ArrayList<>();
	}

	protected void generateIds() {
		generateIDsPerList(birds);
		generateIDsPerList(blocks);
		generateIDsPerList(hills);
		generateIDsPerList(pigs);
		generateIDsPerList(tnts);
	}

	protected void generateIDsPerList(List<ABObject> list) {
		int counter = 0;
		for (ABObject obj : list)
			obj.globalID = obj.getType().toString().toLowerCase() + counter++;
	}

	public Slingshot getSlingshot() {
		return slingshot;
	}

	public void setSlingshot(Slingshot slingshot) {
		this.slingshot = slingshot;
	}

	/**
	 * Returns a semi-sorted list of birds. The first bird is the one on the sling.
	 * The rest of the birds are <i>not</i> ordered.
	 */
	public List<ABObject> getBirds() {
		return new ArrayList<>(birds);
	}

	public List<ABObject> getPigs() {
		return new ArrayList<>(pigs);
	}

	public List<ABObject> getHills() {
		return new ArrayList<>(hills);
	}

	public List<ABObject> getBlocks() {
		return new ArrayList<>(blocks);
	}

	public List<ABObject> getTnts() {
		return new ArrayList<>(tnts);
	}

	public List<ABObject> getAllObjects() {
		return new ArrayList<>(_allObjects);
	}

	public int getGroundPlane() {
		return this.groundPlaneY;
	}

	protected List<ABObject> mergeAllObjectsInOneList() {
		List<ABObject> allBlocks = new ArrayList<>();
		try {
			allBlocks.addAll(birds);
		} catch (Exception ignored) {
		}
		try {
			allBlocks.addAll(sortedTargetsList());
		} catch (Exception ignored) {
		}
		return allBlocks;
	}

	public List<ABObject> sortedTargetsList() {
		List<ABObject> allBlocks = new ArrayList<>();
		try {
			allBlocks.addAll(pigs);
		} catch (Exception ignored) {
		}
		try {
			allBlocks.addAll(hills);
		} catch (Exception ignored) {
		}
		try {
			allBlocks.addAll(blocks);
		} catch (Exception ignored) {
		}
		try {
			allBlocks.addAll(tnts);
		} catch (Exception ignored) {
		}
		allBlocks.sort(Comparator.comparingInt(o -> o.x));
		return allBlocks;
	}

	public int estimateMaximalPointsWithoutBirds() {
		int points = 0;
		for (ABObject o : _allObjects) {
			switch (o.getType()) {
				case Pig:
					points += 5300;
					break;
				case Wood:
					points += 700;
					break;
				case Ice:
					points += 650;
					break;
				case Stone:
					points += 800;
					break;
				default:
					break;
			}
		}
		return points;// + birds.size() * 10000;
	}

	public List<TargetPoint> getTargetPoints() {
		if (birds == null || birds.isEmpty())
			return new ArrayList<>();
		List<ABObject> allBlocks = sortedTargetsList();
		double screenScale = estimateScreenScale();
		List<TargetPoint> targetPoints = new ArrayList<>();
		for (ABObject obj : allBlocks) {
			targetPoints.add(new TargetPoint(obj, obj.getCenter()));

			if (obj instanceof Rect) {
				Rect r = ((Rect) obj);
				double len = r.getpLength() / 2;
				double dy = len * Math.sin(r.angle);
				double dx = len * Math.cos(r.angle);
				if (2 * len * screenScale > 50) {
					targetPoints.add(new TargetPoint(obj,new Point2D.Double(r.centerX - dx, r.centerY - dy)));
					targetPoints.add(new TargetPoint(obj,new Point2D.Double(r.centerX + dx, r.centerY + dy)));
					if (2 * len * screenScale > 100) {
						targetPoints.add(new TargetPoint(obj,new Point2D.Double(r.centerX - dx/2, r.centerY - dy/2)));
						targetPoints.add(new TargetPoint(obj,new Point2D.Double(r.centerX + dx/2, r.centerY + dy/2)));
					}
				}
			}
		}
		return targetPoints;
	}

	protected double estimateScreenScale() {
		double sum = 0;
		int count = 0;
		for (ABObject b : birds) {
			if (!(b instanceof Circle))
				continue;
			sum += ScreenScale.birds[b.type.id()].radius / ((Circle) b).r;
			count++;
		}
		return sum / count;
	}

	/**
	 * Search for an ID in all objects in the scene an return the matching object.
	 * 
	 * @param id the ID of an object (e.g. {@code pig2})
	 * @return an object with ID {@code id} or {@code null} if no matching object is
	 *         found
	 */
	public ABObject findObjectWithID(String id) {
		for (ABObject o : _allObjects)
			if (o.globalID.equals(id))
				return o;
		return null;
	}

	public int getCountSavedShots() {
		return this.savedShots.size();
	}

	public List<SavedShot> getPossibleShots() {
		if (savedShots == null)
			return new ArrayList<>();
		return new ArrayList<>(savedShots);
	}

	public void setPossibleShots(List<SavedShot> shots) {
		if (savedShots == null){
			savedShots = new ArrayList<>();
		} 
		if (shots != null){
			savedShots.clear();
			savedShots.addAll(shots);
		}
	}

	public void addPossibleShots(List<SavedShot> shots) {
		if (savedShots == null){
			savedShots = new ArrayList<>();
		} 
		if (shots != null){
			savedShots.addAll(shots);
		}
	}

	// public void setReachabilityForAllBlocks(ShotPlanner planner) {}

	public String sceneDescription() {
		String birds = "";
		int miny = 1000;
		String birdInSling = "-";

		for (ABObject bird : this.birds) {
			String thisBird = "";
			switch (bird.getType()) {
				case RedBird:
					thisBird = "R";
					break;
				case YellowBird:
					thisBird = "Y";
					break;
				case BlueBird:
					thisBird = "B";
					break;
				case BlackBird:
					thisBird = "K";
					break;
				case WhiteBird:
					thisBird = "W";
					break;
				default:
					break;
			}
			birds += thisBird;
			if (bird.getY() < miny) {
				miny = (int) bird.getY();
				birdInSling = thisBird;
			}
		}
		return "Sl:" + birdInSling + " B:" + birds + " P:" + this.pigs.size() + " I:" + getNumberOfSpecBlocks(ABType.Ice)
				+ " W:" + getNumberOfSpecBlocks(ABType.Wood) + " S:" + getNumberOfSpecBlocks(ABType.Stone) + " T:"
				+ this.tnts.size();
	}

	protected long getNumberOfSpecBlocks(ABType typeOfBlock) {
		return this.blocks.stream().filter(o -> o.getType() == typeOfBlock).count();
	}

	public ABType getBirdTypeOnSling() {
		try {
			return birds.get(0).getType();
		} catch (Exception e) {
			return ABType.RedBird;
		}
	}

	@Override
	public String toString() {
		return sceneDescription();
	}

	/**
	 * 
	 * @return a copy of the stored Image
	 */
	public BufferedImage getImage() {
		return ImageUtil.deepCopy(image);
	}

}