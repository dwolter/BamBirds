package de.uniba.sme.bambirds.feedback;

import java.util.concurrent.Callable;
import java.awt.image.BufferedImage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.objects.Timer;

/**
 * Callable to concurrently run instances of {@link de.uniba.sme.bambirds.feedback.FeedbackScene} and return them when called as 
 * a {@link java.util.concurrent.Future}.
 * @see de.uniba.sme.bambirds.feedback.FeedbackScene
 * @see java.util.concurrent.Future
 */
public class SceneCalculator implements Callable<FeedbackScene> {

    private static final Logger log = LogManager.getLogger(SceneCalculator.class);
    private final BufferedImage fullImage;
    private BufferedImage image;
    private int[] cutoff;
    private int id;
    public SceneCalculator(BufferedImage fullImage, BufferedImage image, int[] cutoff, int id){
        this.fullImage = fullImage;
        this.image = image;
        this.cutoff = cutoff;
        this.id = id;
    }

    @Override
    public FeedbackScene call() throws Exception {
        Timer calculationTime = new Timer();
        FeedbackScene scene = new FeedbackScene(fullImage, image, cutoff, id);
        log.info("Took " + calculationTime.getElapsedTime() + "ms to calculate");
        return scene;
    }

}