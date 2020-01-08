package features;

import ab.utils.ImageSegFrame;
import helper.CustomLogger;
import meta.ActionRobot;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.List;

import static helper.Constants.DEBUG_ENABLED;

public class VisualDebugger {
	final static public boolean globalDebuggingEnabled = DEBUG_ENABLED;
	public boolean localDebuggingEnabled = true;
	public boolean visualOuputEnabled = true;
	public boolean fileOutputEnabled = true;

	private String _windowName;
	private BufferedImage _frameBuffer;
	private ImageSegFrame _frame;
	private Rectangle _bounds = new Rectangle();
	private int sequenceCounter = 0;


	public VisualDebugger(String windowName) {
		_windowName = windowName;
		clearBuffer(840, 480);
	}

	public void enableDebug(boolean visual, boolean file) {
		visualOuputEnabled = visual;
		fileOutputEnabled = file;
		localDebuggingEnabled = (visual || file);
	}

	public boolean canDrawVisual() { return (globalDebuggingEnabled && localDebuggingEnabled && visualOuputEnabled); }
	public boolean canSaveFile() { return (globalDebuggingEnabled && localDebuggingEnabled && fileOutputEnabled); }
	public boolean canOutputImage() { return (globalDebuggingEnabled && localDebuggingEnabled); }

	// ##############################

	private void clearBuffer(int w, int h) {
		_frameBuffer = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
		_bounds.width = w - 1;
		_bounds.height = h - 1;
	}

	public void setBlack() { _fn(()->{ clearBuffer(840, 480); }); }
	public void setBlack(int w, int h) { _fn(()->{ clearBuffer(w, h); }); }

	private void _fn(Runnable func) {
		if (!canOutputImage()) return;
		func.run();
		updateFrame();
	}

	private void updateFrame() {
		if (!visualOuputEnabled) return;
		if (_frame == null)
			_frame = new ImageSegFrame(_windowName, _frameBuffer);
		_frame.refresh(_frameBuffer);
	}

	public void setImage(BufferedImage img) {
		_fn(()->{ _frameBuffer = img; });
	}

	public void doScreenshot() {
		_fn(()->{ _frameBuffer = ActionRobot.get().doScreenShot(); });
	}

	public void drawPoint(int x, int y, int color) {
		if (_bounds.contains(x, y))
			_fn(()->{ _frameBuffer.setRGB(x, y, color); });
	}

	public void drawPoint(Point2D.Double p, int color) {
		if (_bounds.contains(p))
			_fn(()->{ _frameBuffer.setRGB((int)Math.round(p.x), (int)Math.round(p.y), color); });
	}

	public void drawPoints(List<Point2D.Double> ptList, int color) {
		_fn(()->{
			for (Point2D.Double p : ptList)
				if (_bounds.contains(p))
					_frameBuffer.setRGB((int) Math.round(p.x), (int) Math.round(p.y), color);
		});
	}

	public void drawTarget(Point2D.Double p, Color color) {
		_fn(()->{ drawTarget((int)p.x, (int)p.y, color); });
	}

	public void drawTarget(int x, int y, Color color) {
		_fn(()->{
			Graphics2D g2d = _frameBuffer.createGraphics();
			g2d.setColor(color);
			g2d.drawLine(x - 5, y, x + 5, y);
			g2d.drawLine(x, y - 5, x, y + 5);
		});
	}

	public void drawBoundingBox(Rectangle rect, Color color) {
		_fn(()-> {
			Graphics2D g2d = _frameBuffer.createGraphics();
			g2d.setColor(color);
			g2d.drawRect(rect.x - 1, rect.y - 1, rect.width + 1, rect.height + 1);
		});
	}

	public void drawQuadraticWithOffset(double[] weights, Point2D.Double offset, int color) {
		_fn(()->{
			for (int x = 0; x < _frameBuffer.getWidth(); x++) {
				double y = weights[0] * x * x + weights[1] * x + weights[2];
				Point p = new Point((int)Math.round(x + offset.x), (int)Math.round(offset.y - y));
				if (_bounds.contains(p))
					_frameBuffer.setRGB(p.x, p.y, color);
			}
		});
	}

	public void drawQuadratic(double[] weights, int color) {
		_fn(()->{
			for (int x = 0; x < _frameBuffer.getWidth(); x++) {
				int y = (int)(weights[0] * x * x + weights[1] * x + weights[2]);
				if (_bounds.contains(x, y))
					_frameBuffer.setRGB(x, y, color);
			}
		});
	}

	public Graphics2D getGraphics() {
		return _frameBuffer.createGraphics();
	}

	// ##############################

	public int incrementCounter() { return sequenceCounter++; }

	public void saveToFile(String filename) { saveToFileDirectly(filename, _frameBuffer); }

	public void saveToFileDirectly(String filename, BufferedImage img) {
		if (canSaveFile()) {
			try {
				File f = new File(String.format("debug/%s.png", filename));
				f.getParentFile().mkdirs();
				ImageIO.write(img, "png", f);
			} catch (Exception e) {
				CustomLogger.severe("[VisualDebugger] Error while writing image: " + e.getMessage());
			}
		}
	}
}
