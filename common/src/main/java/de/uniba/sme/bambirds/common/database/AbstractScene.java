package de.uniba.sme.bambirds.common.database;

import com.google.gson.Gson;
import de.uniba.sme.bambirds.common.gson.BamBirdsGson;
import de.uniba.sme.bambirds.common.gson.Exclude;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.Slingshot;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import de.uniba.sme.bambirds.common.utils.ImageUtil;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

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

	@Exclude
	protected List<ABObject> allObjects;
	protected int groundPlaneY = -1;

	@Exclude
	protected final BufferedImage image;

	public AbstractScene(final BufferedImage image) {
		super();
		this.image = image;
		pigs = new ArrayList<>();
		birds = new ArrayList<>();
		hills = new ArrayList<>();
		blocks = new ArrayList<>();
		tnts = new ArrayList<>();
		allObjects = new ArrayList<>();
	}

	protected void generateIds() {
		generateIDsPerList(birds);
		generateIDsPerList(blocks);
		generateIDsPerList(hills);
		generateIDsPerList(pigs);
		generateIDsPerList(tnts);
	}

	protected void generateIDsPerList(final List<ABObject> list) {
		int counter = 0;
		for (ABObject obj : list) {
			obj.setGlobalID(obj.getType().toString().toLowerCase() + counter++);
		}
	}

	public Slingshot getSlingshot() {
		return slingshot;
	}

	public void setSlingshot(final Slingshot slingshot) {
		this.slingshot = slingshot;
	}

	/**
	 * Returns a semi-sorted list of birds. The first bird is the one on the sling.
	 * The rest of the birds are <i>not</i> ordered.
	 *
	 * @return The list of birds
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
		return new ArrayList<>(allObjects);
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
		for (ABObject o : allObjects) {
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
		return points; // + birds.size() * 10000;
	}

	protected double estimateScreenScale() {
		double sum = 0;
		int count = 0;
		for (ABObject b : birds) {
			if (!(b instanceof Circle)) {
				continue;
			}
			sum += ScreenScale.BIRDS[b.getType().id()].getRadius() / ((Circle) b).getRadius();
			count++;
		}
		return sum / count;
	}

	/**
	 * Search for an ID in all objects in the scene and return the matching object.
	 *
	 * @param id the ID of an object (e.g. {@code pig2})
	 * @return an object with ID {@code id} or {@code null} if no matching object is
	 * found
	 */
	public ABObject findObjectWithID(final String id) {
		for (ABObject o : allObjects) {
			if (o.getGlobalID().equals(id)) {
				return o;
			}
		}
		return null;
	}

	public String sceneDescription() {
		StringBuilder birds = new StringBuilder();
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
			birds.append(thisBird);
			if (bird.getY() < miny) {
				miny = (int) bird.getY();
				birdInSling = thisBird;
			}
		}
		return "Sl:" + birdInSling + " B:" + birds + " P:" + this.pigs.size() + " I:" + getNumberOfSpecBlocks(ABType.Ice)
				+ " W:" + getNumberOfSpecBlocks(ABType.Wood) + " S:" + getNumberOfSpecBlocks(ABType.Stone) + " T:"
				+ this.tnts.size();
	}

	protected long getNumberOfSpecBlocks(final ABType typeOfBlock) {
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
	 * @return a copy of the stored Image
	 */
	public BufferedImage getImage() {
		return ImageUtil.deepCopy(image);
	}

	/**
	 * @return a json representation of the scene
	 */
	public String toJSON() {
		Gson g = BamBirdsGson.getGson();
		return g.toJson(this);
	}

	public int getObjectCount() {
		return allObjects.size();
	}

	/**
	 * Compare this scene to another one.
	 *
	 * @param scene Another scene
	 * @return True if the scenes match with only small differences
	 */
	public boolean compareTo(final AbstractScene scene) {
		// Pig and object number must be the same
		if (scene == null) {
			return false;
		}
		if (pigs.size() != scene.pigs.size()
				|| blocks.size() != scene.blocks.size()
				|| tnts.size() != scene.tnts.size()) {
			return false;
		}
		// Otherwise, check if the objects have moved
		List<ABObject> thisObjects = sortedTargetsList();
		List<ABObject> otherSceneObjects = sortedTargetsList();
		int min = Math.min(thisObjects.size(), otherSceneObjects.size());
		for (int i = 0; i < min; i++) {
			if (thisObjects.get(i).getCenter().distance(otherSceneObjects.get(i).getCenter()) > 5) {
				return false;
			}
		}
		return true;
	}
}
