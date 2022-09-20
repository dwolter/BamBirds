/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/

package de.uniba.sme.bambirds.client;

import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.exceptions.ServerException.Reason;
import de.uniba.sme.bambirds.common.utils.ActionRobot;
import de.uniba.sme.bambirds.common.utils.ByteUtil;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.concurrent.Semaphore;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * A server/client version of the java util class that encodes client messages
 * and decodes the corresponding server messages complying with the protocols.
 * Its subclass is ClientActionRobotJava.java which decodes the received server
 * messages into java objects.
 */
public class ABActionRobot implements ActionRobot {
	private static final Logger LOG = LogManager.getLogger(ABActionRobot.class);

	private static final int RECEIVE_BUFFER_SIZE = 100000;
	private static final String DEFAULT_IP = "localhost";
	private static final int DEFAULT_PORT = 2004;
	private static final int SCREENSHOT_BYTE_COUNT = 3;
	private static final int SCREENSHOT_BUFFER_CHUNK_SIZE = 3;

	private Socket requestSocket;
	private OutputStream out;
	private InputStream in;

	private final Semaphore token = new Semaphore(1, true);

	public ABActionRobot(final String... args) throws ServerException {
		String ip = DEFAULT_IP;
		if (args.length > 0) {
			ip = args[0];
		}
		int port = DEFAULT_PORT;
		if (args.length > 1) {
			port = Integer.parseInt(args[1]);
		}
		try {
			// 1. creating a socket to connect to the server
			requestSocket = new Socket(ip, port);
			requestSocket.setReceiveBufferSize(RECEIVE_BUFFER_SIZE);
			LOG.info("Connected to " + ip + " in port 2004");
			out = requestSocket.getOutputStream();
			out.flush();
			in = requestSocket.getInputStream();
		} catch (UnknownHostException unknownHost) {
			LOG.error("You are trying to connect to an unknown host!");
		} catch (IOException ioException) {
			close();
			throw new ServerException(Reason.INVALID_RESPONSE, ioException);
		}
	}

	@Override
	public byte simpleMessage(final byte msg) throws ServerException {
		return simpleMessage(msg, 1)[0];
	}

	@Override
	public byte[] simpleMessage(final byte msg, final int responseLength) throws ServerException {
		return simpleMessage(new byte[] { msg }, responseLength);
	}

	@Override
	public byte simpleMessage(final byte[] msg) throws ServerException {
		return simpleMessage(msg, 1)[0];
	}

	@Override
	public byte[] simpleMessage(final byte[] msg, final int responseLength) throws ServerException {
		try {
			token.acquire();
			LOG.trace("Sending message {}", msg);
			out.write(msg);
			out.flush();
			byte[] response = new byte[responseLength];
			LOG.trace("Received message {}", response);
			if (in.read(response) == -1) {
				throw new ServerException(ServerException.Reason.INVALID_RESPONSE);
			}
			if (response[0] == -1) {
				throw new ServerException(ServerException.Reason.SHUTDOWN);
			}
			return response;
		} catch (SocketException | InterruptedException e) {
			// TODO: handle exception
			throw new ServerException(Reason.INVALID_RESPONSE, e);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} finally {
			token.release();
		}
		return new byte[responseLength];
	}

	@Override
	public BufferedImage doScreenShot() throws ServerException {
		BufferedImage bfImage = null;
		try {
			token.acquire();
			// 2. get Input and Output streams
			byte[] doScreenShot = ClientMessageEncoder.encodeDoScreenShot();
			LOG.trace("client executes command: screen shot");
			out.write(doScreenShot);
			out.flush();

			// Read the message head : 4-byte width and 4-byte height, respectively
			byte[] byteWidth = new byte[Integer.BYTES];
			byte[] byteHeight = new byte[Integer.BYTES];
			int width, height;
			if (in.read(byteWidth) == -1) {
				throw new ServerException(ServerException.Reason.INVALID_RESPONSE);
			}
			width = ByteUtil.bytesToInt(byteWidth);
			if (width == -1) {
				throw new ServerException(ServerException.Reason.SHUTDOWN);
			}
			if (in.read(byteHeight) == -1) {
				throw new ServerException(ServerException.Reason.INVALID_RESPONSE);
			}
			height = ByteUtil.bytesToInt(byteHeight);
			LOG.trace("message head read");

			// initialize total bytes of the screenshot message
			// not include the head
			int totalBytes = width * height * SCREENSHOT_BYTE_COUNT;

			// read the raw RGB data
			byte[] bytebuffer;
			// CustomLogger.info(width + " " + height);
			byte[] imgBytes = new byte[totalBytes];
			int hasReadBytes = 0;
			while (hasReadBytes < totalBytes) {
				bytebuffer = new byte[SCREENSHOT_BUFFER_CHUNK_SIZE];
				int nBytes = in.read(bytebuffer);
				if (nBytes != -1) {
					System.arraycopy(bytebuffer, 0, imgBytes, hasReadBytes, nBytes);
				} else {
					break;
				}
				hasReadBytes += nBytes;
			}
			LOG.trace("all bytes received");

			// set RGB data using BufferedImage
			bfImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
			for (int y = 0; y < height; y++) {
				for (int x = 0; x < width; x++) {
					int r = imgBytes[(y * width + x) * SCREENSHOT_BYTE_COUNT] & 0xff;
					int g = imgBytes[(y * width + x) * SCREENSHOT_BYTE_COUNT + 1] & 0xff;
					int b = imgBytes[(y * width + x) * SCREENSHOT_BYTE_COUNT + 2] & 0xff;
					Color color = new Color(r, g, b);
					int rgb;
					rgb = color.getRGB();
					bfImage.setRGB(x, y, rgb);
				}
			}
			LOG.trace("image created");

		} catch (IOException ioException) {
			ioException.printStackTrace();
		} catch (InterruptedException e) {
			throw new ServerException(Reason.INVALID_RESPONSE, e);
		} finally {
			token.release();
		}
		return bfImage;

	}

	// send message to fully zoom out
	@Override
	public byte fullyZoomOut() throws ServerException {
		return simpleMessage(ClientMessageEncoder.fullyZoomOut());
	}

	// send message to fully zoom out
	@Override
	public byte fullyZoomIn() throws ServerException {
		return simpleMessage(ClientMessageEncoder.fullyZoomIn());
	}

	@Override
	public byte clickInCenter() throws ServerException {
		return simpleMessage(ClientMessageEncoder.clickInCenter());
	}

	// register team id
	@Override
	public byte[] configure(byte[] teamId) throws ServerException {
		return simpleMessage(ClientMessageEncoder.configure(teamId), 4);
	}

	// load a certain level
	@Override
	public byte loadLevel(byte... i) throws ServerException {
		return simpleMessage(ClientMessageEncoder.loadLevel(i));
	}

	// send a message to restart the level
	@Override
	public byte restartLevel() throws ServerException {
		return simpleMessage(ClientMessageEncoder.restart());
	}

	// send a shot message to execute a shot in the safe mode
	@Override
	public byte shoot(byte[] fx, byte[] fy, byte[] dx, byte[] dy, byte[] t1, byte[] t2, boolean polar)
			throws ServerException {
		if (polar)
			return simpleMessage(ClientMessageEncoder.pShoot(fx, fy, dx, dy, t1, t2));
		else
			return simpleMessage(ClientMessageEncoder.cShoot(fx, fy, dx, dy, t1, t2));
	}

	// send a shot message to execute a shot in the fast mode
	@Override
	public byte shootFast(byte[] fx, byte[] fy, byte[] dx, byte[] dy, byte[] t1, byte[] t2, boolean polar)
			throws ServerException {
		if (polar)
			return simpleMessage(ClientMessageEncoder.pFastShoot(fx, fy, dx, dy, t1, t2));
		else
			return simpleMessage(ClientMessageEncoder.cFastShoot(fx, fy, dx, dy, t1, t2));
	}

	/**
	 * send a sequence of shots message
	 * 
	 * @throws ServerException
	 */
	@Override
	public byte[] shootSequence(byte[]... shots) throws ServerException {

		byte[] msg = ClientMessageEncoder.mergeArrays(new byte[] { ClientMessageTable.shootSeq.get() },
				new byte[] { (byte) shots.length });
		for (byte[] shot : shots) {
			msg = ClientMessageEncoder.mergeArrays(msg, new byte[] { ClientMessageTable.cShoot.get() }, shot);
		}

		return simpleMessage(msg,shots.length);
	}

	@Override
	public byte[] shootSequenceFast(byte[]... shots) throws ServerException {

		byte[] msg = ClientMessageEncoder.mergeArrays(new byte[] { ClientMessageTable.shootSeqFast.get() },
				new byte[] { (byte) shots.length });
		for (byte[] shot : shots) {
			msg = ClientMessageEncoder.mergeArrays(msg, new byte[] { ClientMessageTable.cShoot.get() }, shot);
		}

		return simpleMessage(msg,shots.length);
	}

	// send a message to get the current state
	@Override
	public byte getState() throws ServerException {
		return simpleMessage(ClientMessageEncoder.getState());
	}

	// send a message to score of each level
	@Override
	public byte[] getBestScores() throws ServerException {
		int level = 21;
		int totalBytes = level * 4;
		return simpleMessage(ClientMessageEncoder.getBestScores(), totalBytes);
	}

	// send a message to score of each level
	@Override
	public byte[] getMyScore() throws ServerException {
		int level = 21;
		int totalBytes = level * 4;
		return simpleMessage(ClientMessageEncoder.getMyScore(), totalBytes);
	}

	public static void main(String args[]) throws ServerException {
		ABActionRobot robot = new ABActionRobot();
		byte[] id = { 1, 2, 3, 4 };

		try {
			robot.configure(id);
			while (true)
				robot.doScreenShot();
		} catch (ServerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void close() {
		try {
			if (out != null) {
				out.close();
			}
			if (in != null) {
				in.close();
			}
			if (requestSocket != null) {
				requestSocket.close();
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public byte setSimulationSpeed(byte[] speed) throws ServerException {
		throw new UnsupportedOperationException("Not supported by ABServer");
	}

	@Override
	public byte[] getCurrentLevel() throws ServerException {
		return simpleMessage(ClientMessageEncoder.getCurrentLevel(),1);
	}

	@Override
	public byte[] getNumberOfLevels() throws ServerException {
		throw new UnsupportedOperationException("Not supported by ABServer");
	}

	@Override
	public byte[] getGroundTruth(boolean withScreenshot, boolean noisy) throws ServerException {
		throw new UnsupportedOperationException("Not supported by ABServer");
	}

	@Override
	public byte[] getCurrentLevelScore() throws ServerException {
		throw new UnsupportedOperationException("Not supported by ABServer");
	}

	@Override
	public byte reportNoveltyLikelihood(byte... info) throws ServerException {
		throw new UnsupportedOperationException("Not supported by ABServer");
	}

	@Override
	public byte[] readyForNewSet() throws ServerException {
		throw new UnsupportedOperationException("Not supported by ABServer");
	}

	@Override
	public byte[] requestNoveltyInformation() throws ServerException {
		throw new UnsupportedOperationException("Not supported by ABServer");
	}

	@Override
	public byte[] selectNextLevel() throws ServerException {
		throw new UnsupportedOperationException("Not supported by ABServer");
	}

}
