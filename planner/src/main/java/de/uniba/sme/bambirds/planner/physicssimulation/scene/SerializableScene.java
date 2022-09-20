package de.uniba.sme.bambirds.planner.physicssimulation.scene;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import de.uniba.sme.bambirds.common.database.ScreenScale;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.Slingshot;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;

import java.io.Serializable;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties.EntityPropertiesLUT;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;



/**A SerializableScene-object is a serializable version of the AbstractScene and can be stored locally to increase the development iteration speed - such that the vision module's output can be reused during development without the need to run the agent again and again. */
public class SerializableScene  implements Serializable {
	private static final long serialVersionUID = 1L;
	
    private static final Logger log = LogManager.getLogger(SerializableScene.class);

	public EntityPropertiesLUT getEntityPropertiesLUT() {
		return entityPropertiesLUT;
	}

	public void setEntityPropertiesLUT(EntityPropertiesLUT entityPropertiesLUT) {
		this.entityPropertiesLUT = entityPropertiesLUT;
	}

	private EntityPropertiesLUT entityPropertiesLUT = new EntityPropertiesLUT();

	
	// default-constructor necessary for serialization
	public SerializableScene(){
		pigs = new ArrayList<>();
		birds = new ArrayList<>();
		hills = new ArrayList<>();
		blocks = new ArrayList<>();
		tnts = new ArrayList<>();
		_allObjects = new ArrayList<>();

	} 
	public SerializableScene(AbstractScene abstractScene){
		pigs = abstractScene.getPigs();
		birds = abstractScene.getBirds();
		hills = abstractScene.getHills();
		blocks = abstractScene.getBlocks();
		tnts = abstractScene.getTnts();
		_allObjects = abstractScene.getAllObjects();
		groundPlaneY = abstractScene.getGroundPlane();
	}


	public Slingshot slingshot;
	public double scalingFactor = 1.005;
	public List<ABObject> pigs;
	public List<ABObject> birds;
	public List<ABObject> hills;
	public List<ABObject> blocks;
	public List<ABObject> tnts;
	public List<ABObject> _allObjects;
	public int groundPlaneY = -1;
	

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
			obj.setGlobalID(obj.getType().toString().toLowerCase() + counter++);
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

	protected double estimateScreenScale() {
		double sum = 0;
		int count = 0;
		for (ABObject b : birds) {
			if (!(b instanceof Circle))
				continue;
			sum += ScreenScale.BIRDS[b.getType().id()].getRadius() / ((Circle) b).getRadius();
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
			if (o.getGlobalID().equals(id))
				return o;
		return null;
	}


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


}