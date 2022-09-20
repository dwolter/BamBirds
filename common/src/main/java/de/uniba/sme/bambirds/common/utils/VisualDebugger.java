package de.uniba.sme.bambirds.common.utils;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.util.List;

import static de.uniba.sme.bambirds.common.utils.Settings.VISUAL_DEBUG_ENABLED;

public class VisualDebugger {
	private static final Logger log = LogManager.getLogger(VisualDebugger.class);
	final static public boolean globalDebuggingEnabled = VISUAL_DEBUG_ENABLED;
	public boolean localDebuggingEnabled = true;
	public boolean visualOutputEnabled = true;
	public boolean fileOutputEnabled = true;

	private final String _windowName;
	private BufferedImage _frameBuffer;
	private ImageSegFrame _frame;
	private int sequenceCounter = 0;


	public VisualDebugger(String windowName) {
		_windowName = windowName;
		clearBuffer(Settings.IMAGE_WIDTH, Settings.IMAGE_HEIGHT);
	}

	public void enableDebug(boolean visual, boolean file) {
		visualOutputEnabled = visual;
		fileOutputEnabled = file;
		localDebuggingEnabled = (visual || file);
	}

	public boolean canDrawVisual() {
		return (globalDebuggingEnabled && localDebuggingEnabled && visualOutputEnabled);
	}

	public boolean canSaveFile() {
		return (globalDebuggingEnabled && localDebuggingEnabled && fileOutputEnabled);
	}

	public boolean canOutputImage() {
		return (globalDebuggingEnabled && localDebuggingEnabled);
	}

	// ##############################

	private void clearBuffer(int w, int h) {
		_frameBuffer = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
	}

	public void setBlack() {
		_fn(() -> clearBuffer(Settings.IMAGE_WIDTH, Settings.IMAGE_HEIGHT));
	}

	public void setBlack(int w, int h) {
		_fn(() -> clearBuffer(w, h));
	}

	private void _fn(Runnable func) {
		if (!canOutputImage()) return;
		func.run();
		updateFrame();
	}

	private void updateFrame() {
		if (!visualOutputEnabled) return;
		if (_frame == null)
			_frame = new ImageSegFrame(_windowName, _frameBuffer);
		_frame.refresh(_frameBuffer);
	}

	public void setImage(BufferedImage img) {
		_fn(() -> _frameBuffer = img);
	}

	public void drawPoint(int x, int y, int color) {
		_fn(() -> ImageUtil.drawPoint(_frameBuffer, x, y, color));
	}

	public void drawPoint(Point2D.Double p, int color) {
		_fn(() -> ImageUtil.drawPoint(_frameBuffer, p, color));
	}

	public void drawPoints(List<Point2D.Double> ptList, int color) {
		_fn(() -> ImageUtil.drawPoints(_frameBuffer, ptList, color));
	}

	public void drawTarget(Point2D.Double p, Color color) {
		_fn(() -> drawTarget((int) p.x, (int) p.y, color));
	}

	public void drawTarget(int x, int y, Color color) {
		_fn(() -> ImageUtil.drawTarget(_frameBuffer, x, y, color));
	}

	public void drawBoundingBox(Rectangle rect, Color color) {
		_fn(() -> ImageUtil.drawBoundingBox(_frameBuffer, rect, color));
	}

	public void drawQuadraticWithOffset(double[] weights, Point2D.Double offset, int color) {
		_fn(() -> ImageUtil.drawQuadraticWithOffset(_frameBuffer, weights, offset, color));
	}

	public void drawQuadratic(double[] weights, int color) {
		_fn(() -> ImageUtil.drawQuadratic(_frameBuffer, weights, color));
	}

	public Graphics2D getGraphics() {
		return _frameBuffer.createGraphics();
	}

	// ##############################

	public int incrementCounter() {
		return sequenceCounter++;
	}

	public void saveToFile(String filename) {
		saveToFileDirectly(filename, _frameBuffer);
	}

	public void saveToFileDirectly(String filename, BufferedImage img) {
		if (canSaveFile()) {
			ImageUtil.saveToDebugFile(img, filename);
		}
	}
}
