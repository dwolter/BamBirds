package helper;

import java.io.*;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

public class SrcFileCopy {

	static public void init() {
		copyResourceFolder("Prolog");
	}

	static private void copyResourceFolder(String sourceFolder) {
		List<String> resFiles = getFilenamesForRessourceDir(sourceFolder);
		try {
			for (String fileStr : resFiles) {
				File file = new File(fileStr);
				if (!file.exists()) {
					File parent = file.getParentFile();
					parent.mkdirs();
					//CustomLogger.info("Copying file: " + fileStr);

					InputStream inStream = Thread.currentThread().getContextClassLoader().getResourceAsStream(fileStr);
					OutputStream outStream = new FileOutputStream(file);

					byte[] buffer = new byte[8 * 1024];
					int bytesRead;
					while ((bytesRead = inStream.read(buffer)) != -1) {
						outStream.write(buffer, 0, bytesRead);
					}
					inStream.close();
					outStream.close();
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	static private List<String> getFilenamesForRessourceDir(String directoryName) {
		List<String> filenames = new ArrayList<>();
		try {
			URL url = Thread.currentThread().getContextClassLoader().getResource(directoryName);
			if (url != null && url.getProtocol().equals("jar")) {
				String dirname = directoryName + "/";
				String path = url.getPath();
				String jarPath = path.substring(5, path.indexOf("!"));
				JarFile jar = new JarFile(URLDecoder.decode(jarPath, StandardCharsets.UTF_8.name()));
				Enumeration<JarEntry> entries = jar.entries();
				while (entries.hasMoreElements()) {
					JarEntry entry = entries.nextElement();
					String name = entry.getName();
					if (name.startsWith(dirname) && !dirname.equals(name) && !name.endsWith("/"))
						filenames.add(name);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return filenames;
	}
}
