package de.uniba.sme.bambirds.planner.knowledge;

import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.BamBirdModule;
import de.uniba.sme.bambirds.common.objects.AbstractScene;
import de.uniba.sme.bambirds.common.objects.Level;
import de.uniba.sme.bambirds.common.objects.SavedShot;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABShape;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.shape.Body;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import de.uniba.sme.bambirds.common.objects.ab.shape.Poly;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;

import static java.lang.Math.abs;
import static java.lang.Math.max;
import static de.uniba.sme.bambirds.common.utils.Settings.PROLOG_FILE_EXTENSION;;

public class Knowledge extends BamBirdModule {
	private static final Logger log = LogManager.getLogger(Knowledge.class);
	private List<ABObject> allBlocks;

	// predicate templates for string formatting
	private static final String PRED_1 = "%s(%s).%n";
	private static final String PRED_1_C = "%s(%s,%s).%n";
	private static final String PRED_2 = "%s(%s,%s).%n";
	private static final String PRED_2_C = "%s(%s,%s,%s).%n";
	private static final String PRED_3 = "%s(%s,%s,%s).%n";
	private static final String PRED_3_C = "%s(%s,%s,%s,%s).%n";
	private static final String PRED_4 = "%s(%s,%s,%s,%s).%n";
	private static final DecimalFormat doubleFormatter = new DecimalFormat("###.########");

	private static final Set<ABType> BIRD_TYPES = new HashSet<>(
			Arrays.asList(ABType.RedBird, ABType.YellowBird, ABType.BlueBird, ABType.BlackBird, ABType.WhiteBird));

	private int bar = 2000;

	public Knowledge() {
		DecimalFormatSymbols dotFormatSymbol = new DecimalFormatSymbols();
		dotFormatSymbol.setDecimalSeparator('.');
		doubleFormatter.setDecimalFormatSymbols(dotFormatSymbol);
	}

	/**
	 * Builds a model (a Prolog knowledge base) of {@code scene} and writes it to a
	 * file.
	 * <p>
	 * <i>Note:</i> Tiny differences in the objects' coordinates in {@code scene}
	 * (originally the screenshot from which {@code scene} was created) can result
	 * in different predicates in the knowledge representations.
	 *
	 * @param level    A level object with current scene containing the objects in
	 *                 the level
	 * @param filename A basename for the Prolog output file (without extension; for
	 *                 example {@code "situation1"})
	 * @return A path to a Prolog file
	 * @throws IllegalArgumentException If objects within {@code scene} are missing
	 */
	public Path buildModel(Level level, String filename) throws IllegalArgumentException {
		AbstractScene scene = level.currentScene;
		if (scene == null)
			throw new IllegalArgumentException("Level object does not contain current scene");

		// List of resulting predicate strings
		List<ABObject> birds = scene.getBirds();
		List<ABObject> pigs = scene.getPigs();

		if (pigs == null || pigs.size() == 0) {
			throw new IllegalArgumentException("Can't build model without pigs.");
		} else if (birds == null || birds.size() == 0) {
			throw new IllegalArgumentException("Can't build model without birds.");
		}

		List<String> results = new ArrayList<>();
		List<ABObject> hills = scene.getHills();
		List<ABObject> objects = scene.getAllObjects();
		this.allBlocks = scene.getAllObjects();

		Point2D.Double pt = level.getSlingshot().pivot;
		results.add(String.format(PRED_1, "slingshotPivot", formatTwoDouble(pt.x, pt.y)));
		results.add(String.format(PRED_1, "scene_scale",
				formatTwoDouble(level.getSlingshot().getSceneScale(), level.getScalingFactor())));
		results.add(String.format(PRED_1, "ground_plane", scene.getGroundPlane()));

		for (ABObject ob : objects) {
			boolean evaluateWhatsAbove = false;
			boolean storeOrientation = false;

			results.add(String.format(PRED_2, "shape", ob.globalID, describeShape(ob)));
			switch (ob.getType()) {
				case Ground:
					results.add(String.format(PRED_1, "ground", ob.globalID));
					break;
				case Hill:
					results.add(String.format(PRED_1_C, "hill", ob.globalID, formatCoordinates(ob)));
					break;
				case Sling:
					break;
				case RedBird:
					results.add(String.format(PRED_1, "bird", ob.globalID));
					results.add(String.format(PRED_2, "hasColor", ob.globalID, "red"));
					break;
				case YellowBird:
					results.add(String.format(PRED_1, "bird", ob.globalID));
					results.add(String.format(PRED_2, "hasColor", ob.globalID, "yellow"));
					break;
				case BlueBird:
					results.add(String.format(PRED_1, "bird", ob.globalID));
					results.add(String.format(PRED_2, "hasColor", ob.globalID, "blue"));
					break;
				case BlackBird:
					results.add(String.format(PRED_1, "bird", ob.globalID));
					results.add(String.format(PRED_2, "hasColor", ob.globalID, "black"));
					break;
				case WhiteBird:
					results.add(String.format(PRED_1, "bird", ob.globalID));
					results.add(String.format(PRED_2, "hasColor", ob.globalID, "white"));
					break;
				case Pig:
					results.addAll(getRelations(ob, allBlocks));
					results.add(String.format(PRED_1_C, "pig", ob.globalID, formatCoordinates(ob)));
					evaluateWhatsAbove = true;
					break;
				case Ice:
					results.addAll(getRelations(ob, allBlocks));
					results.add(String.format(PRED_2_C, "hasMaterial", ob.globalID, "ice", formatCoordinates(ob)));
					storeOrientation = true;
					results.add(String.format(PRED_2, "hasForm", ob.globalID, getForm(ob)));
					break;
				case Wood:
					results.addAll(getRelations(ob, allBlocks));
					results.add(String.format(PRED_2_C, "hasMaterial", ob.globalID, "wood", formatCoordinates(ob)));
					storeOrientation = true;
					evaluateWhatsAbove = getForm(ob).equals("ball");
					results.add(String.format(PRED_2, "hasForm", ob.globalID, getForm(ob)));
					break;
				case Stone:
					results.addAll(getRelations(ob, allBlocks));
					results.add(String.format(PRED_2_C, "hasMaterial", ob.globalID, "stone", formatCoordinates(ob)));
					storeOrientation = true;
					evaluateWhatsAbove = getForm(ob).equals("ball");
					results.add(String.format(PRED_2, "hasForm", ob.globalID, getForm(ob)));
					break;
				case TNT:
					results.addAll(getRelations(ob, allBlocks));
					results.add(String.format(PRED_2_C, "hasMaterial", ob.globalID, "tnt", formatCoordinates(ob)));
					evaluateWhatsAbove = true;
					List<ABObject> explodes = getExplodables(ob, allBlocks);
					for (ABObject ex : explodes) {
						results.add(String.format(PRED_2, "canExplode", ob.globalID, ex.globalID));
					}
					break;
				default:
					// log.info("These aren't the blocks you're looking for.");
					// results.addAll(getRelations(ob,allBlocks));
					// results.add(String.format(PRED_1, "object", ob.globalID));
					break;
			}

			if (evaluateWhatsAbove) {
				List<ABObject> aboveList = whatsAbove(ob, allBlocks);
				for (ABObject objectAbove : aboveList) {
					results.add(String.format(PRED_2, "isOver", objectAbove.globalID, ob.globalID));
				}
			}
			if (storeOrientation) {
				if (ob.width >= ob.height) {
					results.add(String.format(PRED_2, "hasOrientation", ob.globalID, "horizontal"));
				} else {
					results.add(String.format(PRED_2, "hasOrientation", ob.globalID, "vertical"));
				}
			}
		}

		// Save all possible shots as parabolas to prolog
		// There isHittable will be calculated
		for (SavedShot obj : scene.getPossibleShots()) {
			if (!Double.isNaN(obj.parabola[0]) && !Double.isNaN(obj.parabola[1])) {
				results.add(String.format(PRED_3, "parabola", obj.targetID, obj.impactAngle,
						formatTwoDouble(obj.parabola[0], obj.parabola[1])));
			}
		}

		List<List<ABObject>> structures = getStructures(allBlocks);

		// write structures to predicate list
		results.addAll(getStructurePredicates(structures, pigs));
		log.info("Structures found and written.");

		setOnHillPredicate(results, hills);

		// write size predicates
		for (ABObject block : allBlocks) {
			results.add(String.format(PRED_2, "hasSize", block.globalID, getSize(block)));
		}
		log.info("Size Predicates checked and written.");

		// write birds
		for (int i = 0; i < birds.size(); i++) {
			String birdID = birds.get(i).globalID;
			results.add(String.format(PRED_2, "birdOrder", birdID, i));
		}

		// add some inference rules
		results.add("object(X) :- hasMaterial(X,_,_,_,_,_).\n");
		results.add("object(X) :- pig(X,_,_,_,_).\n");
		results.add("hasMaterial(P,pork,X,Y,W,H) :- pig(P,X,Y,W,H).\n");
		if (!hills.isEmpty()) {
			results.add("object(X) :- hill(X,_,_,_,_).\n");
		}

		log.info("Checking Defaults...");
		results = addDefaultPredicates(results);

		// remove duplicates and sort results
		Set<String> hs = new HashSet<>();
		hs.addAll(results);
		results.clear();
		results.addAll(hs);

		Collections.sort(results);

		return writeProlog(results, filename);
	}

	// UGLY HACK to check for hills
	// TODO: There has to be a better way!!!
	// But why, though?
	private void setOnHillPredicate(List<String> predicates, List<ABObject> hills) {
		// FIXME -- disabled function due to infinite loop if predicate is added --
		if (true || hills.isEmpty()) {
			log.info("No hills.");
			return;
		}

		log.info("Looking for objects on hills.");
		Pattern p = Pattern.compile("isOn\\(\\w+,ground\\)\\..*\\n");

		for (int i = 0; i < predicates.size(); i++) {
			String predicate = predicates.get(i);
			if (p.matcher(predicate).matches()) {
				String[] foo = predicate.split("[(,)]");

				// find ABObject with regexed globalID
				for (ABObject x : allBlocks) {
					if (x.globalID.equals(foo[1])) {
						predicates.set(i, predicate.replace("ground", onHill(x, hills)));
						// TODO: WIP Slope detection
						predicates.add(i, predicate.replace("ground", onHill(x, hills))); // FIXME: adding causes infinite loop
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
		float area = ob.area;
		if (ob.hollow)
			area /= 2; // guess in the wild

		String shape = "";
		// the following dispatcher is a ugly in OOP, but in order not to modify
		// source code from the AB packackage this seems to be an acceptable approach
		// to simulate dispatch
		if (ob instanceof Rect) {
			Rect r = (Rect) ob;
			shape = String.format("rect, %s,%s,%s,[%s,%s,%s]", doubleFormatter.format(r.centerX),
					doubleFormatter.format(r.centerY), doubleFormatter.format(area), doubleFormatter.format(r.getpWidth()),
					doubleFormatter.format(r.getpLength()), doubleFormatter.format(r.angle));
		} else if (ob instanceof Circle) {
			Circle c = (Circle) ob;
			shape = String.format("ball, %s,%s,%s,[%s]", doubleFormatter.format(c.centerX), doubleFormatter.format(c.centerY),
					doubleFormatter.format(area), doubleFormatter.format(c.r));
		} else if (ob instanceof Poly) {
			Polygon p = ((Poly) ob).polygon;
			Body b = (Body) ob;
			shape = "poly";
			shape = String.format("poly, %s,%s,%s,[%d", doubleFormatter.format(b.centerX), doubleFormatter.format(b.centerY),
					doubleFormatter.format(area), p.npoints);
			for (int i = 0; i < p.npoints; i++) {
				shape += String.format(",[%d,%d]", p.xpoints[i], p.ypoints[i]);
			}
			shape += "]";
		} else {
			log.warn("Whoops: unhandled shape!");
			shape = String.format("unknown, %s,%s,%s,[]", doubleFormatter.format(ob.getCenterX()),
					doubleFormatter.format(ob.getCenterY()), doubleFormatter.format(ob.area));
		}
		return shape;
	}

	private String getForm(ABObject ob) {
		String shape = "none";

		switch (ob.shape) {
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
	private List<String> getRelations(ABObject o2, List<ABObject> list) {
		List<String> rel = new ArrayList<>();
		boolean onGround = true;
		for (ABObject o1 : list) {
			// If hasRelation returns "isOn" o2 lies on o1
			String relation = hasRelation(o2, o1);
			switch (relation) {
				case "isOn":
					rel.add(String.format(PRED_2, relation, o2.globalID, o1.globalID));
					onGround = false;
					break;
				case "isBelow":
					rel.add(String.format(PRED_2, relation, o2.globalID, o1.globalID));
					break;
				case "isRight":
					rel.add(String.format(PRED_2, relation, o2.globalID, o1.globalID));
					if (o2.getType() != ABType.Pig && o2.angle > 0) {
						rel.add(String.format(PRED_2, "supports", o2.globalID, o1.globalID));
					}
					break;
				case "isLeft":
					rel.add(String.format(PRED_2, relation, o2.globalID, o1.globalID));
					if (o2.getType() != ABType.Pig && o2.angle > 0) {
						rel.add(String.format(PRED_2, "supports", o2.globalID, o1.globalID));
					}
					break;
				default:
					break;
			}
		}

		if (onGround) {
			rel.add(String.format(PRED_2, "isOn", o2.globalID, "ground"));
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

			if (poly.polygon.intersects(block.x, block.y, block.width, block.height + tolerance)) {
				return hill.globalID;
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

	private String hasRelation(ABObject o2, ABObject o1) {
		int threshold = 5;
		String relation = "none";

		if (o2.contains(o1) && o1.contains(o2)) {
			return relation;
		}

		// Note: [x=0,y=0] is the left upper corner
		// For example: o2 is on o1 if a ${threshold} high rectangle below o2 intersects
		// with o1
		// (o2.y + o2.height is the *lower* edge of o2)
		if ((new Rectangle(o2.x, o2.y + o2.height, o2.width, threshold).intersects(o1))) {
			return "isOn";
		}
		if ((new Rectangle(o2.x, o2.y - threshold, o2.width, threshold).intersects(o1))) {
			return "isBelow";
		}
		if ((new Rectangle(o2.x + o2.width, o2.y, threshold, o2.height)).intersects(o1)) {
			return "isLeft";
		}
		// if ((new Rectangle(o2.x - threshold, o2.y, threshold,
		// o2.height)).intersects(o1)) {
		if ((new Rectangle(o1.x + o1.width, o1.y, threshold, o1.height)).intersects(o2)) {
			return "isRight";
		}

		return relation;
	}

	/**
	 * @param struct1   (for now) structure with smaller x-value
	 * @param structID1 ID of {@code struct1}
	 * @param struct2   (for now) structure with higher x-value
	 * @param structID2 ID of {@code struct2}
	 * @return A list of predicates (e.g. "canCollapse(structID1, structID2).")
	 */
	private List<String> orderStructures(List<ABObject> struct1, String structID1, List<ABObject> struct2,
			String structID2) {
		List<String> predicates = new ArrayList<>();

		// direction of structures
		predicates.add(String.format(PRED_3, "collapsesInDirection", structID1, structID2, "away"));
		predicates.add(String.format(PRED_3, "collapsesInDirection", structID2, structID1, "towards"));

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
			predicates.add(String.format(PRED_2, "canCollapse", structID1, structID2));
		}

		if (center2 - (height2 * range(top2)) < (right1.x + right1.width)) {
			predicates.add(String.format(PRED_2, "canCollapse", structID2, structID1));
		}

		return predicates;
	}

	/**
	 * @param block ABObject to decide range for
	 * @return Range of {@code block} depending on its shape
	 */
	private double range(ABObject block) {
		if (block.shape == ABShape.Circle) {
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
	 * Writes the given List into a new Prolog file.
	 *
	 * @param results List of results
	 * @return Path to Prolog file
	 */
	private Path writeProlog(List<String> results, String filename) {
		Path filepath = Paths.get(filename + PROLOG_FILE_EXTENSION).toAbsolutePath().normalize();

		try (BufferedWriter bw = Files.newBufferedWriter(filepath)) {
			bw.write(":- dynamic(isHittable/2).\n");
			for (String result : results) {
				bw.write(result);
			}
		} catch (IOException e) {
			log.error("Couldn't write Prolog: " + e.getMessage(), e);
		}

		return filepath;
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
			if (!"none".equals(hasRelation(target, x))) {
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
	 * @return List of closely neighboring blocks
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
	 * @return List of lists of grouped blocks.
	 */
	public List<List<ABObject>> getStructures(List<ABObject> blocklist) {
		List<ABObject> candidates = blocklist.stream()
				.filter(b -> b.getType() != ABType.Hill && !BIRD_TYPES.contains(b.getType())).collect(Collectors.toList());
		List<List<ABObject>> structures = new ArrayList<>();

		while (!candidates.isEmpty()) {
			List<ABObject> structure = floodFill(candidates.get(0), candidates, new ArrayList<>());
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
	public List<String> getStructurePredicates(List<List<ABObject>> structures, List<ABObject> pigs) {
		List<String> predicates = new ArrayList<>();
		structures.sort(new sortStructuresByX());

		for (int i = 0; i < structures.size(); i++) {
			List<ABObject> struct = structures.get(i);

			String structID = "struct" + i;
			predicates.add(String.format(PRED_1, "structure", structID));

			// sort struct for finding leftmost object
			struct.sort(new XComparator());
			ABObject leftmostObject = struct.get(0);
			struct.sort(new XRightComparator());
			ABObject rightmostObject = struct.get(0);

			struct.sort(new YComparator());
			ABObject topmostObject = struct.get(0);
			struct.sort(new YBottomComparator());
			ABObject bottommostObject = struct.get(0);

			predicates.add(String.format(PRED_2, "isAnchorPointFor", leftmostObject.globalID, structID));

			if (isStructTower(leftmostObject, rightmostObject, topmostObject, bottommostObject)) {
				predicates.add(String.format(PRED_1, "isTower", structID));
			}

			for (ABObject obj : struct) {
				predicates.add(String.format(PRED_2, "belongsTo", obj.globalID, structID));
			}

			// make structure collapsable.
			predicates.add(String.format(PRED_1, "isCollapsable", structID));

			if (i != structures.size() - 1) {
				// check which structures can collapse each other
				predicates.addAll(orderStructures(struct, structID, structures.get(i + 1), "struct" + (i + 1)));
			}

			// check if structure protects a pig
			for (ABObject pig : pigs) {
				if (!struct.contains(pig) && whatsAbove(pig, struct) != pig) {
					predicates.add(String.format(PRED_2, "protects", structID, pig.globalID));
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

	/**
	 * @param predicates a list of Prolog predicates
	 * @return {@code predicates} with default predicates added
	 */
	private List<String> addDefaultPredicates(List<String> predicates) {
		// FIXME: doing string search is not sufficient since predicates may appear on
		// the right hand side only!!
		String[] defaults1 = { "isCollapsable", "object", "isTower" };
		String[] defaults2 = { "protects", "belongsTo", "isAnchorPointFor", "canCollapse", "isOn", "isBelow", "isLeft",
				"isRight", "supports", "hasSize", "isHittable", "hasForm", "canExplode", "hasOrientation", "isOver" };
		String[] defaults3 = { "collapsesInDirection" };
		String[] defaults4 = { "parabola" };
		String[] defaults5 = { "hill", "shape" };
		String[] defaults6 = { "hasMaterial" };

		List<List<String>> all = new ArrayList<>();
		all.add(Arrays.asList(defaults1));
		all.add(Arrays.asList(defaults2));
		all.add(Arrays.asList(defaults3));
		all.add(Arrays.asList(defaults4));
		all.add(Arrays.asList(defaults5));
		all.add(Arrays.asList(defaults6));

		int pred_i = 0;

		for (List<String> defaults : all) {
			pred_i += 1;
			log.info("Now checking defaults/" + pred_i);

			for (String def : defaults) {
				boolean missing = true;

				for (String pred : predicates) {
					if (pred.contains(def)) {
						missing = false;
						break;
					}
				}

				if (missing) {
					switch (pred_i) {
						case 1:
							predicates.add(String.format(PRED_1, def, "dummyObject"));
							log.info("Wrote default predicate: " + def);
							break;
						case 2:
							predicates.add(String.format(PRED_2, def, "dummyObject", "dummyObject"));
							log.info("Wrote default predicate: " + def);
							break;
						case 3:
							predicates.add(String.format(PRED_3, def, "dummyObject", "dummyObject", "dummyObject"));
							log.info("Wrote default predicate: " + def);
							break;
						case 4:
							predicates.add(String.format(PRED_4, def, "dummyObject", "dummyObject", "dummyObject", "dummyObject"));
							log.info("Wrote default predicate: " + def);
							break;
						case 5:
							predicates.add(String.format(PRED_1, def, "dummyObject, 0,0,1,1"));
							log.info("Wrote default predicate: " + def);
							break;
						case 6:
							predicates.add(String.format(PRED_2, def, "dummyObject", "dummyObject, 0,0,1,1"));
							log.info("Wrote default predicate: " + def);
							break;

					}
				}
			}
		}

		return predicates;
	}

	/*
	 * TODO: - onGround has a temporary workaround (every object that has no "isOn"
	 * relation) - size - separate method for pigs, and more resilient - balls as
	 * important objects: only big circles? more than just "above" ?
	 */

	@Override
	public void shutdown() {
		// TODO Auto-generated method stub
	}

}
