package meta;

import ab.demo.other.ClientActionRobotJava;
import main.BamBird;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

public class ActionRobot extends ClientActionRobotJava {
	private static ActionRobot ourInstance = new ActionRobot(BamBird.serverHost);

	public static ActionRobot get() {
		return ourInstance;
	}

	private ActionRobot(String ip) { super(ip); }

	/**
	 * Create a screenshot and mask all UI elements gray
	 * @param cutoffX Everything left of this value will be masked aswell. Default: 0 without masking
	 */
	public BufferedImage screenshotWithoutUI(int cutoffX) {
		BufferedImage screenShot = ActionRobot.get().doScreenShot();
		Graphics2D g2d = screenShot.createGraphics();
		g2d.setColor(Color.darkGray);
		g2d.fillRect(0, 0, 205, 70); // pause, retry, eagle button
		g2d.fillRect(630, 18, 205, 70); // high score
		if (cutoffX > 0)
			g2d.fillRect(0, 0, cutoffX, 480); // everything left of slingshot
		else
			g2d.fillRect(17, 374, 21, 84);  // zoom buttons

		// special handling for right triangle menu
		int x[] = {835, 806, 835};
		int y[] = {198, 241, 280};
		g2d.fillPolygon(x, y, 3);
		return screenShot;
	}

	/** @return Number of not identical pixel */
	public int pixelDifference(BufferedImage imgA, BufferedImage imgB) {
		int height = Math.min(imgA.getHeight(), imgB.getHeight());
		int width = Math.min(imgA.getWidth(), imgB.getWidth());
		int diff = imgA.getWidth() * imgA.getHeight() + imgB.getWidth() * imgB.getHeight() - 2 * width * height;
		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {
				if (imgA.getRGB(x, y) != imgB.getRGB(x, y))
					++diff;
			}
		}
		return diff;
	}
}
