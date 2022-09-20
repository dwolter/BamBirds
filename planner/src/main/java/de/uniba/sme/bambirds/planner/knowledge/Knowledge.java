package de.uniba.sme.bambirds.planner.knowledge;

import java.awt.Point;
import java.awt.Polygon;
import java.awt.geom.Area;
import java.awt.geom.Point2D;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.*;
import java.util.stream.Collectors;

import de.uniba.sme.bambirds.common.database.Level;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;
import de.uniba.sme.bambirds.planner.predicates.IPredicateGenerator;
import de.uniba.sme.bambirds.planner.predicates.Predicate;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABShape;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.shape.Body;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import de.uniba.sme.bambirds.common.objects.ab.shape.Poly;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;

import static java.lang.Math.abs;
import static java.lang.Math.max;

/**
 * Creates a Knowledge Model for Prolog
 */
public class Knowledge implements IPredicateGenerator {
	private static final Logger log = LogManager.getLogger(Knowledge.class);
	private List<ABObject> allBlocks;
	private final Level level;
	private final AbstractScene scene;

	private static final String NONE = "none";

	/**
	 * Threshold for how many pixels blocks can be apart from each other to be realted to each other
	 */
	private static final int RELATION_THRESHOLD = 5;
	private static final String RELATION_IS_IN= "isIn";
	private static final String RELATION_IS_ON = "isOn";
	private static final String RELATION_IS_BELOW = "isBelow";
	private static final String RELATION_IS_RIGHT = "isRight";
	private static final String RELATION_IS_LEFT = "isLeft";

	// predicate templates for string formatting
	private static final DecimalFormat doubleFormatter = new DecimalFormat("###.########");

	private static final Set<ABType> BIRD_TYPES = new HashSet<>(
			Arrays.asList(ABType.RedBird, ABType.YellowBird, ABType.BlueBird, ABType.BlackBird, ABType.WhiteBird));

	private int bar = 2000;

	public Knowledge(Level level) {
		this.level = level;
		this.scene = level.currentScene;
		DecimalFormatSymbols dotFormatSymbol = new DecimalFormatSymbols();
		dotFormatSymbol.setDecimalSeparator('.');
		doubleFormatter.setDecimalFormatSymbols(dotFormatSymbol);
	}

	@Override
	public List<Predicate> call() throws PredicateGenerationException {
		long startTime = System.currentTimeMillis();
		if (scene == null)
			throw new PredicateGenerationException("Level object does not contain current scene");

		// List of resulting predicate strings
		List<ABObject> birds = scene.getBirds();
		List<ABObject> pigs = scene.getPigs();

		if (pigs == null || pigs.size() == 0) {
			throw new PredicateGenerationException("Can't build model without pigs.");
		} else if (birds == null || birds.size() == 0) {
			throw new PredicateGenerationException("Can't build model without birds.");
		}

		List<Predicate> results = new ArrayList<>();
		List<ABObject> hills = scene.getHills();
		List<ABObject> objects = scene.getAllObjects();
		this.allBlocks = scene.getAllObjects();

		Point2D.Double pt = level.getSlingshot().getPivot();
		results.add(new Predicate("slingshotPivot", formatTwoDouble(pt.x, pt.y)));
		results.add(new Predicate("scene_scale",
						formatTwoDouble(scene.getSlingshot().getSceneScale(), level.getScalingFactor())));
		results.add(new Predicate("ground_plane", scene.getGroundPlane()));

		for (ABObject ob : objects) {
			boolean evaluateWhatsAbove = false;
			boolean storeOrientation = false;

			results.add(new Predicate("shape", ob.getGlobalID(), describeShape(ob)));
			switch (ob.getType()) {
				case Ground:
					results.add(new Predicate("ground", ob.getGlobalID()));
					break;
				case Hill:
					results.add(new Predicate( "hill", ob.getGlobalID(), formatCoordinates(ob)));
					break;
				case Sling:
					break;
				case RedBird:
					results.add(new Predicate("bird", ob.getGlobalID()));
					results.add(new Predicate("hasColor", ob.getGlobalID(), "red"));
					break;
				case YellowBird:
					results.add(new Predicate("bird", ob.getGlobalID()));
					results.add(new Predicate("hasColor", ob.getGlobalID(), "yellow"));
					break;
				case BlueBird:
					results.add(new Predicate("bird", ob.getGlobalID()));
					results.add(new Predicate("hasColor", ob.getGlobalID(), "blue"));
					break;
				case BlackBird:
					results.add(new Predicate("bird", ob.getGlobalID()));
					results.add(new Predicate("hasColor", ob.getGlobalID(), "black"));
					break;
				case WhiteBird:
					results.add(new Predicate("bird", ob.getGlobalID()));
					results.add(new Predicate("hasColor", ob.getGlobalID(), "white"));
					break;
				case Pig:
					results.addAll(getRelations(ob, allBlocks));
					results.add(new Predicate("hasMaterial", ob.getGlobalID(), "pork", formatCoordinates(ob)));
					results.add(new Predicate("pig", ob.getGlobalID(), formatCoordinates(ob)));
					evaluateWhatsAbove = true;
					break;
				case Ice:
					results.addAll(getRelations(ob, allBlocks));
					results.add(new Predicate("hasMaterial", ob.getGlobalID(), "ice", formatCoordinates(ob)));
					storeOrientation = true;
					results.add(new Predicate("hasForm", ob.getGlobalID(), getForm(ob)));
					break;
				case Wood:
					results.addAll(getRelations(ob, allBlocks));
					results.add(new Predicate("hasMaterial", ob.getGlobalID(), "wood", formatCoordinates(ob)));
					storeOrientation = true;
					evaluateWhatsAbove = getForm(ob).equals("ball");
					results.add(new Predicate("hasForm", ob.getGlobalID(), getForm(ob)));
					break;
				case Stone:
					results.addAll(getRelations(ob, allBlocks));
					results.add(new Predicate("hasMaterial", ob.getGlobalID(), "stone", formatCoordinates(ob)));
					storeOrientation = true;
					evaluateWhatsAbove = getForm(ob).equals("ball");
					results.add(new Predicate("hasForm", ob.getGlobalID(), getForm(ob)));
					break;
				case TNT:
					results.addAll(getRelations(ob, allBlocks));
					results.add(new Predicate("hasMaterial", ob.getGlobalID(), "tnt", formatCoordinates(ob)));
					evaluateWhatsAbove = true;
					List<ABObject> explodes = getExplodables(ob, allBlocks);
					for (ABObject ex : explodes) {
						results.add(new Predicate("canExplode", ob.getGlobalID(), ex.getGlobalID()));
					}
					break;
				default:
					// log.info("These aren't the blocks you're looking for.");
					// results.addAll(getRelations(ob,allBlocks));
					// results.add(new Predicate("object", ob.globalID));
					break;
			}

			if (evaluateWhatsAbove) {
				List<ABObject> aboveList = whatsAbove(ob, allBlocks);
				for (ABObject objectAbove : aboveList) {
					results.add(new Predicate("isOver", objectAbove.getGlobalID(), ob.getGlobalID()));
				}
			}
			if (storeOrientation) {
				if (ob.width >= ob.height) {
					results.add(new Predicate("hasOrientation", ob.getGlobalID(), "horizontal"));
				} else {
					results.add(new Predicate("hasOrientation", ob.getGlobalID(), "vertical"));
				}
			}
		}

		List<List<ABObject>> structures = getStructures(allBlocks);

		// write structures to predicate list
		results.addAll(getStructurePredicates(structures, pigs));
		log.debug("Structures found and written.");

		setOnHillPredicate(results, hills);

		// write size predicates
		for (ABObject block : allBlocks) {
			results.add(new Predicate("hasSize", block.getGlobalID(), getSize(block)));
		}
		log.debug("Size Predicates checked and written.");

		// write birds
		for (int i = 0; i < birds.size(); i++) {
			String birdID = birds.get(i).getGlobalID();
			results.add(new Predicate("birdOrder", birdID, i));
		}

		// remove duplicates and sort results
		Set<Predicate> hs = new HashSet<>(results);
		results.clear();
		results.addAll(hs);
		log.debug("generated {} predicates in {} milliseconds", results.size(),
				System.currentTimeMillis() - startTime);

		return results;
	}

	// UGLY HACK to check for hills
	// TODO: There has to be a better way!!!
	// But why, though?
	private void setOnHillPredicate(List<Predicate> predicates, List<ABObject> hills) {
		// FIXME -- disabled function due to infinite loop if predicate is added --
		if (true || hills.isEmpty()) {
			log.debug("No hills.");
			return;
		}

		log.info("Looking for objects on hills.");

		for (int i = 0; i < predicates.size(); i++) {
			Predicate predicate = predicates.get(i);
			if (predicate.getPredicateName().equals("isOn")) {
				List<String> args = Arrays.asList(predicate.getArgs().clone());
				int index = args.indexOf("ground");
				if (index == -1) {
					continue;
				}

				// find ABObject with regexed globalID
				for (ABObject x : allBlocks) {
					if (x.getGlobalID().equals(args.get(0))) {
						args.set(index, onHill(x, hills));
						predicates.set(i, new Predicate(predicate.getPredicateName(), args));// FIXME: adding causes infinite loop
						// TODO: WIP Slope detection
					}
				}
			}
		}
	}

	private String formatCoordinates(ABObject object) {
		int x = (int) object.getX();
		int width = (int) object.getWidth();
		int y = (int) object.getY();
		int height = (int) object.getHeight();
		return String.format("%d,%d,%d,%d", x, y, width, height);
	}

	private String formatTwoDouble(double a, double b) {
		return String.format("%s,%s", doubleFormatter.format(a), doubleFormatter.format(b));
	}

	private String describeShape(ABObject ob) {
		float area = ob.getArea();
		if (ob.isHollow())
			area /= 2; // guess in the wild

		String shape = "";
		// the following dispatcher is a ugly in OOP, but in order not to modify
		// source code from the AB packackage this seems to be an acceptable approach
		// to simulate dispatch
		if (ob instanceof Rect) {
			Rect r = (Rect) ob;
			shape = String.format("rect, %s,%s,%s,[%s,%s,%s]", doubleFormatter.format(r.getCenterX()),
					doubleFormatter.format(r.getCenterY()), doubleFormatter.format(area), doubleFormatter.format(r.getPWidth()),
					doubleFormatter.format(r.getPLength()), doubleFormatter.format(r.getAngle()));
		} else if (ob instanceof Circle) {
			Circle c = (Circle) ob;
			shape = String.format("ball, %s,%s,%s,[%s]", doubleFormatter.format(c.getCenterX()), doubleFormatter.format(c.getCenterY()),
					doubleFormatter.format(area), doubleFormatter.format(c.getRadius()));
		} else if (ob instanceof Poly) {
			Polygon p = ((Poly) ob).getPolygon();
			Body b = (Body) ob;
			shape = "poly";
			shape = String.format("poly, %s,%s,%s,[%d", doubleFormatter.format(b.getCenterX()), doubleFormatter.format(b.getCenterY()),
					doubleFormatter.format(area), p.npoints);
			for (int i = 0; i < p.npoints; i++) {
				shape += String.format(",[%d,%d]", p.xpoints[i], p.ypoints[i]);
			}
			shape += "]";
		} else if (ob.getType() == ABType.TNT) {
			shape = String.format("rect, %s,%s,%s,[%s,%s,%s]", doubleFormatter.format(ob.getCenterX()),
					doubleFormatter.format(ob.getCenterY()), doubleFormatter.format(ob.getArea()), doubleFormatter.format(ob.getHeight()),
					doubleFormatter.format(ob.getWidth()), doubleFormatter.format(ob.getAngle()));
		} else {
			log.warn("Whoops: unhandled shape!");
			shape = String.format("unknown, %s,%s,%s,[]", doubleFormatter.format(ob.getCenterX()),
					doubleFormatter.format(ob.getCenterY()), doubleFormatter.format(ob.getArea()));
		}
		return shape;
	}

	private String getForm(ABObject ob) {
		String shape = NONE;

		switch (ob.getShape()) {
			case Circle:
				shape = "ball";
				break;
			default:
				if (ob.height == ob.width) {
					shape = "cube";
				} else if (ob.height > (2 * ob.width) || ob.width > (2 * ob.height)) {
					shape = "bar";
					if (max(ob.height, ob.width) < bar) {
						bar = max(ob.height, ob.width);
					}
				} else {
					shape = "block";
				}
				break;
		}

		return shape;
	}

	private String getSize(ABObject block) {
		String size = "medium";

		switch (getForm(block)) {
			case "cube":
				if (block.height < 2 * bar) {
					size = "small";
				} else if (block.height > 2 * bar) {
					size = "big";
				}
				/*
				 * The size of a cube is only "medium" if it is exactly twice as high as bar.
				 * TODO: Just compare >= or <= ?
				 */
				break;
			case "bar":
				/*
				 * How does this make any sense? For example, if height/width is less than bar
				 * it defaults to "medium", but it is "small" if the larger of the two equals
				 * bar. TODO: Something that is not this.
				 */
				if (max(block.height, block.width) == bar) {
					size = "small";
				} else if (max(block.height, block.width) > 2 * bar) {
					size = "big";
				}
				break;
			default:
				return size;
		}

		return size;
	}

	/**
	 * @param objectToCheck an object
	 * @param blocks        list of objects
	 * @return The object above {@code objectToCheck} or {@code objectToCheck} if
	 *         there is no such object
	 */
	private List<ABObject> whatsAbove(ABObject objectToCheck, List<ABObject> blocks) {
		blocks.sort(Collections.reverseOrder(new YComparator()));
		List<ABObject> aboveList = new ArrayList<>();

		int bottomRightX = objectToCheck.x + objectToCheck.width;
		int gap = 5;

		for (ABObject objectAbove : blocks) {
			int ex_ob = objectAbove.x + objectAbove.width;
			if (objectToCheck.y > objectAbove.y && !(objectToCheck.x - ex_ob > gap || objectAbove.x - bottomRightX > gap)) {
				aboveList.add(objectAbove);
			}
		}

		return aboveList;
	}

	/**
	 * Takes an {@code ABObject} and checks for every element in a list of
	 * {@code ABObject} if there is a direct relation between the given object and
	 * the element.
	 *
	 * Note: The {@code onGround} variable is (hopefully) just a temporary
	 * workaround.
	 *
	 * @param o2   ABObject
	 * @param list List<ABObject>
	 * @return A list of predicates/relations (e.g. "isLeft(block2_id, block1_id)")
	 */
	private List<Predicate> getRelations(ABObject o2, List<ABObject> list) {
		List<Predicate> rel = new ArrayList<>();
		boolean onGround = true;
		for (ABObject o1 : list) {
			// Skip for identical objects
			if (o2.getGlobalID().equals(o1.getGlobalID())) {
				continue;
			}
			// If hasRelation returns "isOn" o2 lies on o1
			List<String> relations = hasRelations(o2, o1);
			for (String relation : relations) {
				rel.add(new Predicate(relation, o2.getGlobalID(), o1.getGlobalID()));
				if (relation.equals(RELATION_IS_ON)) {
					onGround = false;
				}
			}
		}

		if (onGround) {
			rel.add(new Predicate(RELATION_IS_ON, o2.getGlobalID(), "ground"));
		}

		return rel;
	}

	/**
	 * @param block ABObject
	 * @param hills Poly
	 * @return A {@code String} containing the {@code globalID} of the hill on which
	 *         {@code block} is or {@code "ground"} if {@code block} is not on any
	 *         hill
	 */
	private String onHill(ABObject block, List<ABObject> hills) {
		int tolerance = 5;
		for (ABObject hill : hills) {
			Poly poly = (Poly) hill;

			if (poly.getPolygon().intersects(block.x, block.y, block.width, block.height + tolerance)) {
				return hill.getGlobalID();
			}
			// //TODO: Finish layered if-clause, maybe replace by switch-case
			// boolean collisionLeft = false;
			// // Check left edge
			// if (poly.polygon.intersects(block.x, block.y, block.width / 4, block.height +
			// tolerance)){
			// collisionLeft = true;
			// }
			// //Check right edge
			// if (poly.polygon.intersects(block.x+block.width *3 / 4, block.y, block.width
			// / 4, block.height + tolerance)){
			// //If left edge also collides
			// if(collisionLeft = true){
			// //return hill
			// return hill.globalID;
			// }else{
			// //If no left collision
			// //return hill + slope LEFT
			// return hill.globalID;
			// }
			// }else{
			// //If collision right edge is false,
			// // do the whole thing again
			// // If left edge does collide:
			// if(collisionLeft = true){
			// //return hill + Slope RIGHT
			// return hill.globalID;
			// }else{
			// //If no left collision
			// //no collision at all --> Ground
			// return "ground";
			// }
			// }

		}
		return "ground";
	}

	private List<String> hasRelations(ABObject object, ABObject otherObject) {
		List<String> relations = new ArrayList<>();

		Area otherArea = new Area(otherObject.getPolygon());
		if (otherArea.contains(object)) {
			relations.add(RELATION_IS_IN);
		} else {
			// Note: [x=0,y=0] is the left upper corner
			// For example: o2 is on o1 if a ${threshold} high rectangle below o2 intersects
			// with o1
			// (o2.y + o2.height is the *lower* edge of o2)
			if (otherArea.intersects(object.x, object.y + object.height, object.width, RELATION_THRESHOLD)) {
				relations.add(RELATION_IS_ON);
			}
			if (otherArea.intersects(object.x, object.y - RELATION_THRESHOLD, object.width, RELATION_THRESHOLD)) {
				relations.add(RELATION_IS_BELOW);
			}
			if (otherArea.intersects(object.x + object.width, object.y, RELATION_THRESHOLD, object.height)) {
				relations.add(RELATION_IS_LEFT);
			}
			if (otherArea.intersects(object.x - RELATION_THRESHOLD, object.y, RELATION_THRESHOLD, object.height)) {
				relations.add(RELATION_IS_RIGHT);
			}
			
		}

		return relations;
	}

	/**
	 * @param struct1   (for now) structure with smaller x-value
	 * @param structID1 ID of {@code struct1}
	 * @param struct2   (for now) structure with higher x-value
	 * @param structID2 ID of {@code struct2}
	 * @return A list of predicates (e.g. "canCollapse(structID1, structID2).")
	 */
	private List<Predicate> orderStructures(List<ABObject> struct1, String structID1, List<ABObject> struct2,
			String structID2) {
		List<Predicate> predicates = new ArrayList<>();

		// direction of structures
		predicates.add(new Predicate("collapsesInDirection", structID1, structID2, "away"));
		predicates.add(new Predicate("collapsesInDirection", structID2, structID1, "towards"));

		struct1.sort(new YComparator());
		struct2.sort(new YComparator());

		ABObject top1 = struct1.get(0);
		ABObject bottom1 = struct1.get(struct1.size() - 1);

		ABObject top2 = struct2.get(0);
		ABObject bottom2 = struct2.get(struct2.size() - 1);

		int height1 = (bottom1.y + bottom1.height) - top1.y;
		int height2 = (bottom2.y + bottom2.height) - top2.y;

		struct1.sort(new XComparator());
		struct2.sort(new XComparator());

		ABObject right1 = struct1.get(struct1.size() - 1);
		ABObject left1 = struct1.get(0);

		ABObject right2 = struct2.get(struct2.size() - 1);
		ABObject left2 = struct2.get(0);

		int center1 = (right1.x + right1.width + left1.x) / 2;
		int center2 = (right2.x + right2.width + left2.x) / 2;

		// difference in elevation between two structures
		int elevation = (bottom1.y + bottom1.height) - (bottom2.y + bottom2.height);

		if (elevation < 0) { // struct1 is at higher elevation than struct2
			height1 += abs(elevation);
		} else {
			height2 += elevation;
		}

		if (center1 + (height1 * range(top1)) > left2.x) {
			predicates.add(new Predicate("canCollapse", structID1, structID2));
		}

		if (center2 - (height2 * range(top2)) < (right1.x + right1.width)) {
			predicates.add(new Predicate("canCollapse", structID2, structID1));
		}

		return predicates;
	}

	/**
	 * @param block ABObject to decide range for
	 * @return Range of {@code block} depending on its shape
	 */
	private double range(ABObject block) {
		if (block.getShape() == ABShape.Circle) {
			return 2.0;
		} else {
			return 1.0;
		}
	}

	private List<ABObject> getExplodables(ABObject tnt, List<ABObject> candidates) {
		List<ABObject> explodables = new ArrayList<>();

		Point center = tnt.getCenter();
		int multiplier = 3;
		int width = tnt.width;

		for (ABObject block : candidates) {
			Point centerb = block.getCenter();
			double dist = Math.hypot((center.x - centerb.x), (center.y - centerb.y));
			if (dist <= width * multiplier) {
				explodables.add(block);
			}
		}

		return explodables;
	}

	/**
	 * Search candidates for blocks neighboring target.
	 * 
	 * @param target     ABObject
	 * @param candidates List of ABObjects to search for neighbors
	 * @return A list of neighbors of target
	 */
	private List<ABObject> getNeighbors(ABObject target, List<ABObject> candidates) {
		List<ABObject> neighbors = new ArrayList<>();

		for (ABObject x : candidates) {
			if (!hasRelations(target, x).isEmpty()) {
				neighbors.add(x);
			}
		}
		return neighbors;
	}

	/**
	 * Recursively searches candidates for neighboring blocks.
	 *
	 * @param target     ABObject to start search from
	 * @param candidates List of possible candidates
	 * @param visited    List of already visited blocks
	 * @return List of closely neighboring blocks including the target
	 */
	private List<ABObject> floodFill(ABObject target, List<ABObject> candidates, List<ABObject> visited) {
		if (visited.contains(target)) {
			return visited;
		} else {
			visited.add(target);
			List<ABObject> neighbors = getNeighbors(target, candidates);

			for (ABObject neighbor : neighbors) {
				visited = floodFill(neighbor, candidates, visited);
			}

			return visited;
		}
	}

	/**
	 * Find groups of neighboring blocks.
	 *
	 * @param blocklist List of ABObjects to search for grouped blocks.
	 * @return List of lists of grouped blocks. Each List contains at least one element.
	 */
	public List<List<ABObject>> getStructures(List<ABObject> blocklist) {
		// candidates are all ABObjects except Hills and Birds
		List<ABObject> candidates = blocklist.stream()
				.filter(b -> b.getType() != ABType.Hill && !BIRD_TYPES.contains(b.getType())).collect(Collectors.toList());
		List<List<ABObject>> structures = new ArrayList<>();

		while (!candidates.isEmpty()) {
			// Execute flood fill with the first Object in the candidates
			List<ABObject> structure = floodFill(candidates.get(0), candidates, new ArrayList<>());
			// Remove the resulting structure from the candidates and add it to the result
			candidates.removeAll(structure);
			structures.add(structure);
		}
		return structures;
	}

	/**
	 * Build structural Prolog predicates from a list of lists of {@code ABObject}s.
	 *
	 * @param structures List of lists of grouped {@code ABObject}s
	 * @return List of {@code Strings} with Prolog predicates.
	 */
	public List<Predicate> getStructurePredicates(List<List<ABObject>> structures, List<ABObject> pigs) {
		List<Predicate> predicates = new ArrayList<>();
		structures.sort(new sortStructuresByX());

		for (int i = 0; i < structures.size(); i++) {
			List<ABObject> struct = structures.get(i);

			String structID = "struct" + i;
			predicates.add(new Predicate("structure", structID));

			// sort struct for finding leftmost object
			struct.sort(new XComparator()); // Should not be necessary as already sorted by sortStructuresByX() above
			ABObject leftmostObject = struct.get(0);
			struct.sort(new XRightComparator()); // Seems unnecessary but may be no way around
			ABObject rightmostObject = struct.get(0);

			struct.sort(new YComparator());
			ABObject topmostObject = struct.get(0);
			struct.sort(new YBottomComparator()); // Seems unnecessary but may be no way around
			ABObject bottommostObject = struct.get(0);

			predicates.add(new Predicate("isAnchorPointFor", leftmostObject.getGlobalID(), structID));

			if (isStructTower(leftmostObject, rightmostObject, topmostObject, bottommostObject)) {
				predicates.add(new Predicate("isTower", structID));
			}

			for (ABObject obj : struct) {
				predicates.add(new Predicate("belongsTo", obj.getGlobalID(), structID));
			}

			// make structure collapsable.
			predicates.add(new Predicate("isCollapsable", structID));

			if (i != structures.size() - 1) {
				// check which structures can collapse each other
				predicates.addAll(orderStructures(struct, structID, structures.get(i + 1), "struct" + (i + 1)));
			}

			// check if structure protects a pig
            for (ABObject pig : pigs) {
                if ( !whatsAbove(pig, struct).isEmpty() ) {
                    predicates.add(new Predicate("protects", structID, pig.getGlobalID()));
                }
            }
        }

		return predicates;
	}

	private boolean isStructTower(ABObject leftmostObject, ABObject rightmostObject, ABObject topmostObject,
			ABObject bottommostObject) {
		double widthOfStruct = rightmostObject.getMaxX() - leftmostObject.getX();
		double heightOfStruct = bottommostObject.getMaxY() - topmostObject.getY();

		if (widthOfStruct >= heightOfStruct) {
			return false;
		} else {
			return true;
		}
	}

	/*
	 * TODO: - onGround has a temporary workaround (every object that has no "isOn"
	 * relation) - size - separate method for pigs, and more resilient - balls as
	 * important objects: only big circles? more than just "above" ?
	 */

	@Override
	public void addSimulationsToVisualSimulationDebugger(String testbedNamePrefix) { }
}
