package de.uniba.sme.bambirds.vision;

import static de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException.Reason.NO_BIRDS_FOUND;
import static de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException.Reason.NO_PIGS_FOUND;
import static de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException.Reason.NO_SLING_FOUND;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.Comparator;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException;
import de.uniba.sme.bambirds.common.objects.AbstractScene;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.Slingshot;
import de.uniba.sme.bambirds.common.utils.ActionRobot;

/**
 * The {@code Scene} class is basically a simple wrapper around the
 * {@link Vision} class. It provides a few simple methods to suit the needs of
 * the <i>BamBird</i> agent and hides the many {@code Vision} methods we don't
 * care about.
 * <p>
 * When you instantiate a {@code Scene} object, make sure you catch the
 * {@code NullPointerException} it may throw if the given {@code BufferedImage}
 * does not contain some critical objects.
 */
public class Scene extends AbstractScene {
	private static final Logger log = LogManager.getLogger(Scene.class);

	/**
	 * Stores all objects in {@code img} that are recognized by the {@code find...}
	 * methods of the {@link Vision} class.
	 * @param img A screenshot from {@link ActionRobot#doScreenShot}
	 * @throws SceneInitialisationException if {@code Vision} cannot find a sling, pigs or birds in {@code img}
	 */
	public Scene(BufferedImage img) throws SceneInitialisationException {
		this(img, null,0);
	}

	/**
	 * Stores all objects in {@code img} that are recognized by the {@code find...}
	 * methods of the {@link Vision} class.
	 * @param img A screenshot from {@link ActionRobot#doScreenShot}
	 * @param scalingFactor If {@code 0} use default scaling factor
	 * @throws SceneInitialisationException if {@code Vision} cannot find a sling, pigs or birds in {@code img}
	 */
	public Scene(BufferedImage img, double scalingFactor) throws SceneInitialisationException {
		this(img, null,scalingFactor);
	}

	/**
	 * Stores all objects in {@code img} that are recognized by the {@code find...}
	 * methods of the {@link Vision} class.
	 * @param img A screenshot from {@link ActionRobot#doScreenShot}
	 * @param sling If {@code null} invoke {@code Vision} to determine
	 * @throws SceneInitialisationException if {@code Vision} cannot find a sling, pigs or birds in {@code img}
	 */
	public Scene(BufferedImage img, Slingshot sling) throws SceneInitialisationException {
		this(img, sling,0);
	}

	/**
	 * Stores all objects in {@code img} that are recognized by the {@code find...}
	 * methods of the {@link Vision} class.
	 *
	 * @param img           A screenshot from {@link ActionRobot#doScreenShot}
	 * @param sling         If {@code null} invoke {@code Vision} to determine
	 * @param scalingFactor If {@code 0} use default scaling factor
	 * @throws SceneInitialisationException if {@code Vision} cannot find a sling, pigs or birds in {@code img}
	 * @see Vision#findSlingshotMBR
	 */
	public Scene(BufferedImage img, Slingshot sling, double scalingFactor) throws SceneInitialisationException {
		super(img);
		this.slingshot = sling;
		if (Double.compare(scalingFactor, 0) != 0)
			this.scalingFactor = scalingFactor;
		Vision vision = new Vision(img);
		if (this.slingshot == null) {
			log.debug("Using VisionMBR to detect slingshot.");
			Rectangle slingRect = vision.findSlingshotMBR();
			if (slingRect == null)
				throw new SceneInitialisationException(NO_SLING_FOUND);
			this.slingshot = new Slingshot(slingRect, null);
			log.debug("found: " + this.slingshot);
		}

		this.pigs = vision.findPigsRealShape();
		if (pigs == null || this.pigs.size() == 0)
			throw new SceneInitialisationException(NO_PIGS_FOUND);

		this.birds = vision.findBirdsRealShape();
		if (birds == null || this.birds.size() == 0)
			throw new SceneInitialisationException(NO_BIRDS_FOUND);

		birds.sort(new Comparator<ABObject>() {
			@Override
			public int compare(ABObject bird1, ABObject bird2) {
				double dist1 = bird1.getCenter().distance(slingshot.getLocation());
				double dist2 = bird2.getCenter().distance(slingshot.getLocation());
				return (int) (dist1 - dist2);
			}
		}); // bird on sling (one with shortest distance to sling)
		this.hills = vision.findHills();
		this.blocks = vision.findBlocksRealShape();
		this.tnts = vision.findTNTs();

		this.groundPlaneY = vision.findGroundPlane((int) Math.round(this.slingshot.getPivotPoint().getY()));

		generateIds();
		_allObjects = mergeAllObjectsInOneList();
	}

}
