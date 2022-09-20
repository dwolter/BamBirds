package de.uniba.sme.bambirds.feedback;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ConnectedComponent;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;
import de.uniba.sme.bambirds.common.utils.ImageUtil;
import de.uniba.sme.bambirds.vision.VisionMBR;
import de.uniba.sme.bambirds.vision.VisionUtils;
import de.uniba.sme.bambirds.vision.real.ImageSegmenter;

import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;

/**
 * This class is used for wrapping the class {@link de.uniba.sme.bambirds.vision.VisionRealShape} and thereby storing relevant
 * information for evaluation in {@link de.uniba.sme.bambirds.feedback.SequenceCalculator}.
 *
 * @see de.uniba.sme.bambirds.vision.VisionRealShape
 * @see de.uniba.sme.bambirds.feedback.SequenceCalculator
 * @author uni (original idea), pascal (redesign & implementation)
 */
public class FeedbackScene{
    private static final Logger LOG = LogManager.getLogger();

    private final List<ABObject> objects;
    private final int id;
    private final BufferedImage image;
    private final long imageTime;
    private final ImageSegmenter segmenter;
    private final List<ConnectedComponent> componentList;
    private final Rectangle slingshot;
    private int[] cutoff;

    public FeedbackScene(BufferedImage fullImage, BufferedImage image, int[] cutoff, int id) {
        this.id = id;
        this.image = image;
        if (cutoff != null && cutoff.length == 4) {
            this.cutoff = cutoff;
        } else {
            throw new IllegalArgumentException("cutoff needs to be an array of length 4");
        }
        this.imageTime = System.currentTimeMillis();
        this.segmenter = new ImageSegmenter(image);
        Rectangle slingshot = (new VisionMBR(ImageUtil.removeABUI(fullImage, 0))).findSlingshotMBR();
        if (slingshot != null) {
            this.slingshot = slingshot;
        } else {
            LOG.warn("Failed to find slingshot, coordinates will not be relative to it");
            this.slingshot = new Rectangle();
        }
        this.componentList = segmenter.findComponents();
        this.objects = findObjects();
    }

    private boolean isValid(ABObject object){
        if(object == null){
            return false;
        } else if(!(object instanceof Rect) && !(object instanceof Circle)){
            /* if not Rect or Circle: high chance of error in vision */
            return false;
        } else {
            return object.getId() != -1;
        }
    }
    /**
     * All possibly moving objects exlcuding birds that can be evaluated in {@link de.uniba.sme.bambirds.feedback.SequenceCalculator}. 
     * Birds not included on purpose: Not necessary, does slow down process, most of them not in picture: Birds require to 
     * calculate sling first, else it throws Exceptions.
     * @return List of objects.
     */
    public List<ABObject> getObjects() {
        return new ArrayList<>(objects);
    }

    public int getId() {
        return id;
    }

    public BufferedImage getImage() {
        return ImageUtil.deepCopy(image);
    }
    /**
     * Draws the ids of all found objects in a scene on the image of that scene.
     * @return Image of the scene with drawn ids.
     */
    public BufferedImage drawImage(boolean drawGrey, boolean drawIds, Color objectBorder) {
        BufferedImage newImage = ImageUtil.deepCopy(image);
        Graphics2D graphic = newImage.createGraphics();
        int fontSize = 8;
        if(drawGrey){
            graphic.drawImage(VisionUtils.convert2grey(newImage), 0, 0, null); 
        }else{
            graphic.drawImage(newImage, 0, 0, null); 
        }
        graphic.setFont(new Font("TimesRoman", Font.PLAIN, fontSize)); 
        for(ABObject object : objects){
            object.translate(-getTranslateX(), -getTranslateY());
            object.draw(graphic, false, objectBorder);
            if(drawIds){
                graphic.setColor(Color.black);
                graphic.drawString(object.getId() + "", (int)object.getCenterX() - fontSize/2, (int)object.getCenterY() + fontSize/2);
            }
            object.translate(getTranslateX(), getTranslateY());
        }
        return newImage;
    }
    /**
     * The time of when the image was taken.
     * @return Time of when image was taken.
     */
    public long getImageTime() {
        return imageTime;
    }
    /* Taken relevant code from VisionRealShape. */
    private List<ABObject> findObjects(){
        List<ABObject> objectList = new ArrayList<>();
        for (ConnectedComponent component : componentList){
            if ((component.getType() >= ImageSegmenter.PIG && component.getType() <= ImageSegmenter.STONE)){
                ABObject object = component.getBody();
                if(isValid(object)){
                    // To provide locations, consistent with the Scene, the objects 
                    // need to be in the global coordinate system and relative to the slingshot
                    object.translate(getTranslateX(), getTranslateY());
                    objectList.add(object);
                }
            }
        }
        return objectList;
    }

    public int getTranslateX() {
        return cutoff[0] - slingshot.x;
    }
    public int getTranslateY() {
        return cutoff[2] - slingshot.y;
    }
}
