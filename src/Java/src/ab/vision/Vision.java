/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014,XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe, Jim Keys,   Andrew Wang, Peng Zhang
 ** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package ab.vision;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.List;

public class Vision {
	private BufferedImage image;
	private VisionMBR visionMBR = null;
	private VisionRealShape visionRealShape = null;

	public Vision(BufferedImage image) {
		// TODO: Migrate to one Class (either MBR, Realshape or combination)
		this.image = image;
		maskUI();
		visionMBR = new VisionMBR(image);
		visionRealShape = new VisionRealShape(image);
		if (findSlingshotRealShape() == null){
			visionRealShape.setSling(visionMBR.findSlingshotMBR());
		}
	}

	/**
	 * Clear out interfering game menu elements
	 */
	private void maskUI() {
		Graphics2D g2d = image.createGraphics();
		g2d.setColor(Color.lightGray);
		g2d.fillRect(0, 0, 205, 70); // pause, retry, eagle button
		g2d.fillRect(630, 18, 205, 70); // high score
		g2d.fillRect(17, 374, 21, 84);  // zoom buttons

		// special handling for right triangle menu
		int x[] = {834, 808, 834};
		int y[] = {200, 241, 278};
		g2d.fillPolygon(x, y, 3);
	}

	public List<ABObject> findBirdsMBR() {
		return visionMBR.findBirds();

	}

	/**
	 * @return a list of MBRs of the blocks in the screenshot. Blocks: Stone,
	 *         Wood, Ice
	 */
	public List<ABObject> findBlocksMBR() {
		return visionMBR.findBlocks();
	}

	public List<ABObject> findTNTs() {
		return visionMBR.findTNTs();
	}

	public Rectangle findSlingshotRealShape() {
		return visionRealShape.findSling();
	}

	public List<ABObject> findPigsMBR() {
		return visionMBR.findPigs();
	}

	public List<ABObject> findPigsRealShape() {
		return visionRealShape.findPigs();
	}

	public List<ABObject> findBirdsRealShape() {
		return visionRealShape.findBirds();
	}

	public List<ABObject> findHills() {
		return visionRealShape.findHills();
	}

	public Rectangle findSlingshotMBR() {
		return visionMBR.findSlingshotMBR();
	}

	public List<Point> findTrajPoints() {
		return visionMBR.findTrajPoints();
	}

	/**
	 * @return a list of real shapes (represented by Body.java) of the blocks in
	 *         the screenshot. Blocks: Stone, Wood, Ice
	 */
	public List<ABObject> findBlocksRealShape() {
		return visionRealShape.findObjects();
	}

	public VisionMBR getMBRVision() {
		return visionMBR;
	}
    
    // return y coordinate of ground plane, based on IHSEV's code
    public int findGroundPlane(int slingPivotY) {
        if (this.image == null) { // shouldn't happen
            return -1;
        }
        int counter;
        int nHeight = this.image.getHeight();
        int nWidth = this.image.getWidth();
            
        for (int y = nHeight-1; y >= slingPivotY; y--) {
            counter = 0;
            for (int x = 0; x < nWidth; x++) {
                Color col = new Color(this.image.getRGB(x,y));
                final int r = col.getRed();
                final int g = col.getGreen();
                final int b = col.getBlue();
                if ((r >= 87 && r <= 90) && (g >= 161 && g <= 165) && (b >= 8 && b <= 10)) {
                    counter++;
                }
                if (counter > 20) { //
                    return y;
                }
            }
        }
        return 384; // no ground, use default
    }
}
