/*****************************************************************************
** ANGRYBIRDS AI AGENT FRAMEWORK
** Copyright (c) 2014,XiaoYu (Gary) Ge, Stephen Gould,Jochen Renz
**  Sahan Abeyasinghe, Jim Keys,   Andrew Wang, Peng Zhang
** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
*****************************************************************************/
package ab.utils;

import ab.server.Proxy;
import ab.server.proxy.message.ProxyScreenshotMessage;
import ab.vision.VisionUtils;
import helper.CustomLogger;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.net.UnknownHostException;

/* GameImageRecorder ------------------------------------------------------ */

public class GameImageRecorder {

    static public void main(String[] args) {

        // check command line arguments
        if (args.length != 1) {
            CustomLogger.severe("USAGE: java GameImageRecorder <directory>");
            System.exit(1);
        }
        if (!(new File(args[0])).isDirectory()) {
            CustomLogger.severe("ERROR: directory " + args[0] + " does not exist");
            System.exit(1);
        }

       
        // connect to game proxy
        Proxy proxy = null;
        try {
            proxy = new Proxy(9000) {
                @Override
                public void onOpen() {
                    CustomLogger.info("Connected to game proxy");
                }

                @Override
                public void onClose() {
                    CustomLogger.info("Disconnected from game proxy");
                }
                };
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        proxy.start();
        CustomLogger.info("Waiting for proxy to connect");
        proxy.waitForClients(1);

        // enter game loop
        int frameCount = 0;
        BufferedImage screenshot = null;
        while (true) {
            // capture screenshot
            byte[] imageBytes = proxy.send(new ProxyScreenshotMessage());
            BufferedImage image = null;
            try {
                image = ImageIO.read(new ByteArrayInputStream(imageBytes));
            } catch (IOException e) {
                e.printStackTrace();
            }

            // write image to disk
            if ((screenshot == null) ||
                (VisionUtils.numPixelsDifferent(screenshot, image) > 2048)) {
                final String imgFilename = String.format(args[0] + File.separator + "img%04d.png", frameCount);
                CustomLogger.info("saving image to " + imgFilename);
                try {
                    ImageIO.write(image, "png", new File(imgFilename));
                } catch (IOException e) {
                    CustomLogger.severe("failed to save image " + imgFilename);
                    e.printStackTrace();
                }

                // update frame count
                screenshot = image;
                frameCount += 1;
            }

            // sleep for a while
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) { }
        }
    }
}
