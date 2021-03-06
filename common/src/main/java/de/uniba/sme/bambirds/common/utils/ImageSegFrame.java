/*****************************************************************************
** ANGRYBIRDS AI AGENT FRAMEWORK
** Copyright (c) 2014,XiaoYu (Gary) Ge, Stephen Gould,Jochen Renz
**  Sahan Abeyasinghe, Jim Keys,   Andrew Wang, Peng Zhang
** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
*****************************************************************************/
package de.uniba.sme.bambirds.common.utils;

import javax.imageio.ImageIO;
import javax.swing.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class ImageSegFrame {
	private static final Logger log = LogManager.getLogger(ImageSegFrame.class);
	private static int _saveCount = 0;
	public static String saveFileDir = "";
	public static volatile boolean recordScreenshot = false;
	public static volatile boolean saveAndExit = false;

	public class ImagePanel extends JPanel implements KeyListener, MouseListener {
		/**
		* 
		*/
		private static final long serialVersionUID = -1162922707389749340L;
		protected JFrame _parent;
		protected Image _img = null;
		protected Popup _tip = null;
		protected int[][] _meta = null;
		protected Boolean _highlightMode = false;
		protected int _highlightIndex = -1;

		public Boolean bWaitingForKey = false;

		public ImagePanel(JFrame parent) {
			_parent = parent;
			addKeyListener(this);
			addMouseListener(this);
			setDoubleBuffered(true);
			// set focusable then you can use key listener
			setFocusable(true);
		}

		public void refresh(Image img) {
			refresh(img, null);
		}

		public void refresh(Image img, int[][] meta) {
			_img = img;
			_meta = meta;
			repaint();
		}

		public void paint(Graphics g) {

			if (_img != null) {
				if ((_meta != null) && (_highlightIndex != -1)) {
					BufferedImage canvas = ImageUtil.highlightRegions(_img, _meta, _highlightIndex, Color.RED);
					g.drawImage(canvas, 0, 0, null);
				} else {

					g.drawImage(_img, 0, 0, null);
				}
			}
		}

		public void highlightTarget(Point point) {
			Graphics2D g = (Graphics2D) getGraphics();
			paint(g);
			if (_img != null) {

				g.setColor(Color.red);
				g.setStroke(new BasicStroke(3));
				g.drawLine(point.x - 5, point.y - 5, point.x + 5, point.y + 5);
				g.drawLine(point.x + 5, point.y - 5, point.x - 5, point.y + 5);
			}
		}

		public void keyPressed(KeyEvent key) {
			// process key

			if (key.getKeyCode() == KeyEvent.VK_ENTER) {
				_parent.setVisible(false);
				_parent.dispose();

			} else if (key.getKeyCode() == KeyEvent.VK_D) {

				String imgFilename = saveFileDir + String.format("img%04d.png", _saveCount);
				log.info("saving image to " + imgFilename);
				BufferedImage bi = new BufferedImage(_img.getWidth(null), _img.getHeight(null), BufferedImage.TYPE_INT_ARGB);
				Graphics2D g2d = bi.createGraphics();
				g2d.drawImage(_img, 0, 0, null);
				g2d.dispose();
				try {
					ImageIO.write(bi, "png", new File(imgFilename));
				} catch (IOException e) {
					log.error("failed to save image " + imgFilename);
					e.printStackTrace();
				}

				if (_meta != null) {
					String metaFilename = String.format("meta%04d.txt", _saveCount);
					log.info("saving meta-data to " + metaFilename);
					try {
						PrintWriter ofs = new PrintWriter(new FileWriter(metaFilename));
						for (int i = 0; i < _meta.length; i++) {
							for (int j = 0; j < _meta[i].length; j++) {
								if (j > 0)
									ofs.print(' ');
								ofs.print(_meta[i][j]);
							}
							ofs.println();
						}
						ofs.close();

					} catch (IOException e) {
						e.printStackTrace();
					}

					_saveCount += 1;
				}
			} else if (key.getKeyCode() == KeyEvent.VK_H) {
				// toggle highlight mode
				if (_highlightMode) {
					_highlightMode = false;
					this.repaint();
				} else {
					_highlightMode = true;
					_highlightIndex = -1;
				}

			} else if (key.getKeyCode() == KeyEvent.VK_S) {
				String imgFilename = String.format("img%04d.png", _saveCount);
				log.info("saving image to " + imgFilename);
				BufferedImage bi = new BufferedImage(_img.getWidth(null), _img.getHeight(null), BufferedImage.TYPE_INT_ARGB);
				Graphics2D g2d = bi.createGraphics();
				g2d.drawImage(_img, 0, 0, null);
				g2d.dispose();
				try {
					ImageIO.write(bi, "png", new File(imgFilename));
				} catch (IOException e) {
					log.error("failed to save image " + imgFilename);
					e.printStackTrace();
				}
				_saveCount += 1;
			}

			// check if usercode is waiting for a keypress
			if (bWaitingForKey) {
				bWaitingForKey = false;
				return;
			}
		}

		public void keyTyped(KeyEvent key) {

		}

		public void keyReleased(KeyEvent key) {
		}

		public void mousePressed(MouseEvent e) {
		}

		public void mouseReleased(MouseEvent e) {
		}

		public void mouseEntered(MouseEvent e) {
		}

		public void mouseExited(MouseEvent e) {
		}

		public void mouseClicked(MouseEvent e) {
			if (_tip == null) {
				JToolTip toolTip = this.createToolTip();
				if (_meta == null) {
					toolTip.setTipText("(" + e.getX() + ", " + e.getY() + ")");

				} else {
					toolTip.setTipText("(" + e.getX() + ", " + e.getY() + "): " + _meta[e.getY()][e.getX()]);

					if (_highlightMode) {
						_highlightIndex = _meta[e.getY()][e.getX()];
						this.repaint();
					}
				}

				Point p = new Point(e.getX(), e.getY());
				SwingUtilities.convertPointToScreen(p, this);
				_tip = PopupFactory.getSharedInstance().getPopup(this, toolTip, p.x, p.y);
				_tip.show();
				toolTip.addMouseListener(new MouseAdapter() {
					public void mouseClicked(MouseEvent e) {
						if (_highlightMode) {
							_highlightIndex = -1;
							repaint();
						}
						_tip.hide();
						_tip = null;
					}
				});
			} else {
				if (_highlightMode) {
					_highlightIndex = -1;
					this.repaint();
				}
				_tip.hide();
				_tip = null;
			}
		}
	}

	protected JFrame frame;
	protected ImagePanel panel;
	protected Image img;
	protected int[][] meta;
	protected String name;
	protected volatile boolean refresh = false;
	private int bound_x = -1;
	private int bound_y = -1;

	public JFrame getFrame() {
		return frame;
	}

	public ImageSegFrame(String name, Image img, int[][] meta, int bound_x, int bound_y) {
		this.name = name;
		this.img = img;
		this.meta = meta;
		frame = new JFrame(name);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setResizable(false);
		panel = new ImagePanel(frame);

		frame.getContentPane().add(panel);

		frame.pack();
		Insets insets = frame.getInsets();
		frame.setSize(img.getWidth(null) + insets.left + insets.right, img.getHeight(null) + insets.top + insets.bottom);
		if (bound_x != -1 && bound_y != -1)
			frame.setBounds(bound_x, bound_y, frame.getSize().width, frame.getSize().height);
		else {
			GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
			GraphicsDevice defaultScreen = ge.getDefaultScreenDevice();
			Rectangle rect = defaultScreen.getDefaultConfiguration().getBounds();
			int x = (int) rect.getMaxX() - frame.getWidth();
			int y = 0;
			// log.info(frame.getWidth());
			frame.setLocation(x, y);

		}
		frame.setVisible(true);
		if (img != null && meta != null)
			panel.refresh(img, meta);
	}

	public ImageSegFrame(String name, Image img, int[][] meta) {

		this.name = name;
		this.img = img;
		this.meta = meta;
		frame = new JFrame(name);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		panel = new ImagePanel(frame);

		frame.getContentPane().add(panel);

		frame.pack();
		Insets insets = frame.getInsets();
		frame.setSize(img.getWidth(null) + insets.left + insets.right, img.getHeight(null) + insets.top + insets.bottom);
		if (bound_x != -1 && bound_y != -1)
			frame.setBounds(bound_x, bound_y, frame.getSize().width, frame.getSize().height);
		else {
			int x = 0;
			int y = 0;
			frame.setLocation(x, y);

		}
		frame.setVisible(true);
		frame.setResizable(false);
		if (img != null && meta != null)
			panel.refresh(img, meta);

		// panel.requestFocus();
	}

	public ImageSegFrame(String name, Image img) {
		this(name, img, null);
	}

	// set new image
	public void refresh(Image img) {
		panel.refresh(img);
	}

	public void refreshNow(Image img, int[][] meta) {
		this.img = img;
		this.meta = meta;
		refresh = true;
	}

	// set new image and meta information for tooltip
	public void refresh(Image img, int[][] meta) {

		panel.refresh(img, meta);

	}

	public void highlightTarget(Point point) {
		panel.highlightTarget(point);
	}

	// close the window
	public void close() {
		frame.setVisible(false);
		frame.dispose();
	}

	// wait for user input
	public void waitForKeyPress() {
		panel.bWaitingForKey = true;
		while (panel.bWaitingForKey) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
			}
		}
	}

}
