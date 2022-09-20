/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014,XiaoYu (Gary) Ge, Stephen Gould,Jochen Renz
 **  Sahan Abeyasinghe, Jim Keys,   Andrew Wang, Peng Zhang
 ** All rights reserved.
 **This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 **To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.utils;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.imageio.ImageIO;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JToolTip;
import javax.swing.Popup;
import javax.swing.PopupFactory;
import javax.swing.SwingUtilities;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.concurrent.TimeUnit;

public class ImageSegFrame {
	private static final Logger log = LogManager.getLogger(ImageSegFrame.class);
	private static int _saveCount = 0;
	public static final String saveFileDir = "";
	private static volatile boolean paused = false;

	public static class ImagePanel extends JPanel implements KeyListener, MouseListener, ActionListener {
		/**
		 *
		 */
		private static final long serialVersionUID = -1162922707389749340L;
		protected final JFrame _parent;
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

		private void close() {
			_parent.setVisible(false);
			_parent.dispose();
		}

		private void saveImage() {
			String imgFilename = saveFileDir + String.format("img%04d.png", _saveCount);
			log.debug("saving image to " + imgFilename);
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

		private void saveHighlights() {

			if (_meta != null) {
				String metaFilename = String.format("meta%04d.txt", _saveCount);
				log.debug("saving meta-data to " + metaFilename);
				try {
					PrintWriter ofs = new PrintWriter(new FileWriter(metaFilename));
					for (int[] ints : _meta) {
						for (int j = 0; j < ints.length; j++) {
							if (j > 0)
								ofs.print(' ');
							ofs.print(ints[j]);
						}
						ofs.println();
					}
					ofs.close();

				} catch (IOException e) {
					e.printStackTrace();
				}

			}
		}

		private void toggleHighlight() {
			// toggle highlight mode
			if (_highlightMode) {
				_highlightMode = false;
				this.repaint();
			} else {
				_highlightMode = true;
				_highlightIndex = -1;
			}
		}

		public void keyPressed(KeyEvent key) {
			// process key

			if (key.getKeyCode() == KeyEvent.VK_ENTER) {
				close();
			} else if (key.getKeyCode() == KeyEvent.VK_D) {
				saveImage();
				saveHighlights();
			} else if (key.getKeyCode() == KeyEvent.VK_H) {
				toggleHighlight();
			} else if (key.getKeyCode() == KeyEvent.VK_S) {
				saveImage();
			}

			// check if usercode is waiting for a keypress
			if (bWaitingForKey) {
				bWaitingForKey = false;
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

		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			switch (actionEvent.getActionCommand()) {
				case "close":
					close();
					break;
				case "highlight":
					toggleHighlight();
					break;
				case "save":
					saveHighlights();
				case "save_no_highlights":
					saveImage();
					break;
				default:
					break;
			}
		}
	}

	protected final JFrame frame;
	protected final ImagePanel panel;
	protected Image img;
	protected int[][] meta;
	protected final String name;
	protected volatile boolean refresh = false;

	public JFrame getFrame() {
		return frame;
	}

	public ImageSegFrame(String name, Image img, int[][] meta, int bound_x, int bound_y) {
		this.name = name;
		this.img = img;
		this.meta = meta;
		frame = new JFrame(name);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		panel = new ImagePanel(frame);

		frame.getContentPane().add(panel);

		JMenu commands = new JMenu("Commands");
		JMenuItem close = new JMenuItem("close", KeyEvent.VK_C);
		close.setActionCommand("close");
		close.addActionListener(panel);
		JMenuItem highlight = new JMenuItem("highlight", KeyEvent.VK_H);
		highlight.setActionCommand("highlight");
		highlight.addActionListener(panel);
		JMenuItem save = new JMenuItem("save current image", KeyEvent.VK_D);
		save.setActionCommand("save");
		save.addActionListener(panel);
		JMenuItem saveNoHighlight = new JMenuItem("save current image (no highlights)", KeyEvent.VK_S);
		saveNoHighlight.setActionCommand("save_no_highlights");
		saveNoHighlight.addActionListener(panel);
		JCheckBoxMenuItem pause = new JCheckBoxMenuItem("pause agent");
		pause.setSelected(false);
		pause.setToolTipText("Pauses the updating of the frame and in turn pauses the agent");
		pause.addItemListener(itemEvent -> {
			JCheckBoxMenuItem item = (JCheckBoxMenuItem) itemEvent.getItem();
			paused = item.getState();
		});
		commands.add(close);
		commands.add(highlight);
		commands.add(save);
		commands.add(saveNoHighlight);
		commands.add(pause);

		JMenuBar menuBar = new JMenuBar();
		menuBar.add(commands);

		frame.setJMenuBar(menuBar);

		frame.pack();
		refresh(img, meta);
		if (bound_x != -1 && bound_y != -1)
			frame.setBounds(bound_x, bound_y, frame.getSize().width, frame.getSize().height);
		else {
			int x = 0;
			int y = 0;
			frame.setLocation(x, y);

		}
		frame.setVisible(true);
		frame.setResizable(false);

	}

	public ImageSegFrame(String name, Image img, int[][] meta) {
		this(name, img, meta, -1, -1);
		// panel.requestFocus();
	}

	public ImageSegFrame(String name, Image img) {
		this(name, img, null);
	}

	// set new image
	public void refresh(Image img) {
		refresh(img, null);
	}

	public void refreshNow(Image img, int[][] meta) {
		this.img = img;
		this.meta = meta;
		refresh = true;
	}

	// set new image and meta information for tooltip
	public void refresh(Image img, int[][] meta) {
		while (paused) {
			try {
				TimeUnit.MILLISECONDS.sleep(100);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		panel.refresh(img, meta);
		Insets insets = frame.getInsets();
		frame.setSize(img.getWidth(null) + insets.left + insets.right, img.getHeight(null) + insets.top + insets.bottom + frame.getJMenuBar().getHeight());
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
			} catch (InterruptedException ignored) {
			}
		}
	}

}
