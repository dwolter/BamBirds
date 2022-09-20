package de.uniba.sme.bambirds.planner.shot;

import de.uniba.sme.bambirds.common.database.DecisionTree;
import de.uniba.sme.bambirds.common.database.Level;
import de.uniba.sme.bambirds.common.database.Node;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.ThinkerType;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.knowledge.Knowledge;


import java.awt.Point;
import java.awt.Rectangle;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.Stack;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author uni
 */
public class FeedbackShotSelection implements MetaShotSelection {
    private static final Logger log = LogManager.getLogger(FeedbackShotSelection.class);

    private Level currentLevel;
    private int executedShots;
    private Knowledge knowledge;

    private Random random = new Random();
    private Plan chosenTarget = null;
    private List<Node> possibleNodes = new ArrayList<>();
    private boolean fallbackToDemoShot = false;

    public FeedbackShotSelection(Level currentLevel, int shotNumber) {
        this.currentLevel = currentLevel;
        this.executedShots = shotNumber;
        this.knowledge = new Knowledge(currentLevel);
    }

    @Override
    public Plan getChosenTarget() {
        // Target for prolog?? or for ABServer??
        return this.chosenTarget;
    }

    @Override
    public boolean generateAndEvaluateShotOptions(List<Plan> plans) {

        if (plans == null || plans.isEmpty()) {
            return false;
        }

        fallbackToDemoShot = false;
        currentLevel.tree.getCurrentNode().createChildNodesFromTargetList(plans);

        return generateAndEvaluateShotOptions();
    }

    @Override
    public boolean generateAndEvaluateShotOptions () {

        try {

            DecisionTree tree = this.currentLevel.tree;
            List<ABObject> allObjects = this.currentLevel.currentScene.getAllObjects();
            Node currentNode = tree.getCurrentNode();

            currentNode.setResultingABObjects(allObjects);

            possibleNodes = currentNode.getChildren().stream()
                    .sorted(Comparator.comparingDouble(n -> -n.getConfidence())).limit(3) // CHange to intelligent
                                                                                          // subset of targets
                    .sorted(Comparator.comparingInt(Node::getTried)).collect(Collectors.toList());

            if (possibleNodes.isEmpty()) {
                throw new NullPointerException("All nodes lead to failure");
            }

            List<Node> triedPaths = possibleNodes.stream().filter(Node::wasTried).collect(Collectors.toList());

            // At least two path can be compared here
            if (triedPaths.size() > 1 || true) {
                findKillers(possibleNodes);
            }

            /**
             * Bewertung anhand ganzem baum - kommutative pfade erkennen - Confidence aus
             * prolog - global schlechte schusse finden und lokal gewichten
             */
            // possibleNodes.sort(Comparator.comparingDouble(n -> -n.getConfidence()));
            exportTree();
            // Nach Graphviz exprortieren am ende von langer testzeit eines levels
            // bsp level: reihenfolgenabhaengiges wettbewerlevel (Schwein durch holz
            // verdeckt, steine oben)
            // Aauch: viele Tuerme und viele schweine, sehr confus
            return true;
        } catch (Exception e) {
            fallbackToDemoShot = true;
            e.printStackTrace();
            log.info("Error generating Targets: " + e.getMessage() + ", falling back to demo shot.");
        }
        return false;

    }

    /**
     * Finds Node that will always lead to failure
     *
     * @param triedPaths
     */
    private void findKillers(List<Node> triedPaths) {
        triedPaths.forEach((node) -> {
            log.info("Confidentiallity for " + node.getPlan().getTargetObject() + " : " + node.getConfidence());
        });
    }

    public void exportTree() {
        if (currentLevel == null || currentLevel.tree == null || currentLevel.tree.getCurrentNode() == null) {
            log.info("Tree is not available for printing.");
            return;
        }
        Node currentRoot = this.currentLevel.tree.getCurrentNode();
        while (currentRoot.parent != null) {
            currentRoot = currentRoot.parent;
        }

        StringBuilder treeExport = new StringBuilder();
        StringBuilder nodeExport = new StringBuilder();

        Stack<Node> nodes = new Stack<>();
        nodes.add(currentRoot);

        while (!nodes.isEmpty()) {
            Node popedNode = nodes.pop();

            String targetID = (popedNode.getPlan() == null) ? "NONE" : popedNode.getPlan().getTargetObject();

            nodeExport.append(System.lineSeparator()).append(";").append(popedNode.hashCode())
                    .append(System.lineSeparator()).append("TargetID: ").append(targetID).append(System.lineSeparator())
                    .append("Confidence: ").append(popedNode.getConfidence()).append(System.lineSeparator())
                    .append("Affected Objects: ").append(popedNode.getAffectedObj()).append(System.lineSeparator())
                    .append("Objects: ").append(System.lineSeparator());

            popedNode.getABObjects().forEach((obj) -> {
                nodeExport.append("        {").append("GlobalID: ").append(obj.getGlobalID()).append(", ").append("X: ")
                        .append(obj.x).append(", ").append("Y: ").append(obj.y).append(", ").append("Width: ")
                        .append(obj.width).append(", ").append("Height: ").append(obj.height).append(", ")
                        .append("Shape: ").append(obj.getShape()).append(", ").append("Type: ").append(obj.getType()).append(", ")
                        .append("Angle: ").append(obj.getAngle()).append("}").append(System.lineSeparator());
            });

            nodeExport.append(";");

            popedNode.getChildren().stream().forEach(child -> {
                treeExport.append("\"").append(popedNode.getPlan() != null ? popedNode.getPlan().getTargetObject() : "Null")
                        .append(" ").append(popedNode.hashCode()).append("\"").append(" -> ").append("\"")
                        .append(child.getPlan() != null ? child.getPlan().getTargetObject(): "Null").append(" ")
                        .append(child.hashCode()).append("\"").append(System.lineSeparator());
                nodes.push(child);
            });
        }

        try (PrintWriter out = new PrintWriter("debug/tree.DOT")) {
            out.write(treeExport.toString());
            out.flush();
        } catch (FileNotFoundException ex) {
            log.info(ex.getMessage());
        }
        try (PrintWriter out = new PrintWriter("debug/nodes.txt")) {
            out.write(nodeExport.toString());
            out.flush();
        } catch (FileNotFoundException ex) {
            log.info(ex.getMessage());
        }

    }

    public List<Node> getAvailableTargets() {
        if (this.possibleNodes == null) {
            return new ArrayList<Node>();
        }
        return this.possibleNodes;
    }

    public void printAvailableTargets() {
        log.info("");
        log.info("Available targets: ");
        if (possibleNodes == null || possibleNodes.isEmpty()) {
            log.info("-- none --");
        } else {
            log.info(Integer.toString(possibleNodes.size()));
            for (Node n : possibleNodes) {
                if (n.getPlan() != null) {
                    log.info(n.getPlan().prettyPrint());
                }
            }
        }
    }

    @Override
    public Node mostPromisingNode() {
        Optional<Node> mostPromisingNode = this.possibleNodes.stream().findFirst();
		
        if (mostPromisingNode.isPresent()) {
            Node highestConfNode = mostPromisingNode.get();
            log.info("Most Promising Target: " + highestConfNode.getPlan().getTargetObject() + " tries "
                    + highestConfNode.tries);
            this.chosenTarget = highestConfNode.getPlan();
            currentLevel.tree.setCurrentNode(highestConfNode);
            highestConfNode.incrementTryCounter();
            return highestConfNode;
        } else {
            Plan demoPlan = getDemoPlan();
			if (demoPlan == null) {
				return null;
			}
			Node demoNode = new Node(demoPlan);
			currentLevel.tree.getCurrentNode().addChild(demoNode);
            currentLevel.tree.setCurrentNode(demoNode);
            return demoNode;
        }
        // TODO: ChosenTarget??? Wird im SHotselector nur !demoshot gesetzt
    }

    private Plan getDemoPlan() {
        log.info("[FeedbackShotSelection] DEMOSHOT");

        TrajectoryPlanner tp = new TrajectoryPlanner();
        Rectangle sling = currentLevel.currentScene.getSlingshot().getBounds();
        ABType birdOnSling = currentLevel.currentScene.getBirds().get(0).getType();
        List<ABObject> pigs = currentLevel.currentScene.getPigs();
        if (sling != null) {

            if (!pigs.isEmpty()) {

                Point releasePoint = null;
                int dx, dy;
                {
                    ABObject pig = pigs.get(random.nextInt(pigs.size()));

                    Point _tpt = pig.getCenter();
                    ArrayList<Point> pts = tp.estimateLaunchPoint(sling, _tpt);

                    if (pts.size() == 1) {
                        releasePoint = pts.get(0);
                    } else if (pts.size() == 2) {
                        if (random.nextInt(6) == 0) {
                            releasePoint = pts.get(1);
                        } else {
                            releasePoint = pts.get(0);
                        }
                    } else if (pts.isEmpty()) {
                        log.warn("No release point found for the target");
                        log.info("Try a shot with 45 degree");
                        releasePoint = TrajectoryPlanner.findReleasePoint(sling, Math.PI / 4);
                    }
                    Point refPoint = TrajectoryPlanner.getReferencePoint(sling);
                    if (releasePoint != null) {
                        double releaseAngle = tp.getReleaseAngle(sling, releasePoint);
                        log.info("Release Point: " + releasePoint);
                        log.info("Release Angle: " + Math.toDegrees(releaseAngle));
                        int tapInterval;
                        switch (birdOnSling) {
                            case RedBird:
                                tapInterval = 0;
                                break;
                            case YellowBird:
                                tapInterval = 65 /* + random.nextInt(25) */;
                                break;
                            case WhiteBird:
                                tapInterval = 70 /* + random.nextInt(20) */;
                                break;
                            case BlackBird:
                                tapInterval = 70 /* + random.nextInt(20) */;
                                break;
                            case BlueBird:
                                tapInterval = 65 /* + random.nextInt(20) */;
                                break;
                            default:
                                tapInterval = 60;
                        }

                        int tapTime = tp.getTapTime(sling, releasePoint, _tpt, tapInterval);
                        dx = (int) releasePoint.getX() - refPoint.x;
                        dy = (int) releasePoint.getY() - refPoint.y;
                        return new Plan(pig.getGlobalID(), 42, "demo", 0,new Shot(refPoint.x, refPoint.y, dx, dy, _tpt.x, _tpt.y, 0, tapTime),ThinkerType.DEMO);
                    } else {
                        log.info("No Release Point Found");
                        return null;
                    }
                }
            }
        }
        return null;
    }

    public Level getCurrentLevel() {
        return currentLevel;
    }

    public void setCurrentLevel(Level currentLevel) {
        this.currentLevel = currentLevel;
    }

    public int getExecutedShots() {
        return executedShots;
    }

    public void setExecutedShots(int executedShots) {
        this.executedShots = executedShots;
    }

    public Knowledge getKnowledge() {
        return knowledge;
    }

    public void setKnowledge(Knowledge knowledge) {
        this.knowledge = knowledge;
    }

    public List<Node> getPossibleNodes() {
        return possibleNodes;
    }

    public void setPossibleNodes(List<Node> possibleNodes) {
        this.possibleNodes = possibleNodes;
    }

    public boolean isFallbackToDemoShot() {
        return fallbackToDemoShot;
    }

    public void setFallbackToDemoShot(boolean fallbackToDemoShot) {
        this.fallbackToDemoShot = fallbackToDemoShot;
    }

}
