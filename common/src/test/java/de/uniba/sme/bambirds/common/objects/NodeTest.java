package de.uniba.sme.bambirds.common.objects;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.Slingshot;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import de.uniba.sme.bambirds.common.utils.ShotHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D;

public class NodeTest {

    @Test
    public void adaptShotBecauseOfNewScalingFactor() {
        // Given
        double initialScalingFactor = 1.005;
        Point2D.Double target = new Point2D.Double(336, 219);
        Slingshot sling = new Slingshot(new Rectangle(117, 327, 20, 55), new Point2D.Double(128.2, 337.8));
        ABObject bird = new Circle(117,327,8,ABType.RedBird);
        ExampleScene scene = new ExampleScene(sling, bird);

        ShotHelper.setProperties(initialScalingFactor, ABType.RedBird);
        Point releasePoint = getReleasePoint(sling, target);

        Plan plan = new Plan("redbird0","pig0", 42, "targetPig", 1.0, new String[]{}, new Shot(sling.x, sling.y, releasePoint.x, releasePoint.y, target.x, target.y, 0, 1300), ThinkerType.NEWPLANNER);
        Node node = new Node(plan, scene);

        // When
        double newScalingFactor = 1.023267;
        node.adaptNodesToNewScalingFactor(initialScalingFactor, newScalingFactor);

        // Then
        ShotHelper.setProperties(newScalingFactor, ABType.RedBird);
        Point newReleasePoint = getReleasePoint(sling, target);

        Assertions.assertEquals(newReleasePoint.x,node.getShot().getDragX(), "X value of the nodes ReleasePoint should equal the new releasePoint after scene scale change");
        Assertions.assertEquals(newReleasePoint.y,node.getShot().getDragY(), "Y value of the nodes ReleasePoint should equal the new releasePoint after scene scale change");
    }

    private Point getReleasePoint(Slingshot sling, Point2D.Double target) {
        double highAngle = ShotHelper.estimateLaunchPoint(sling, target)[1];
        return ShotHelper.angleToReleasePoint(highAngle, sling);
    }

    private static class ExampleScene extends AbstractScene {

        public ExampleScene(Slingshot slingshot, ABObject bird) {
            super(null);
            this.birds.add(bird);
            this.slingshot = slingshot;
        }
    }
}
