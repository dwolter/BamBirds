# Running AIBirds

You will need to ask the Organizers for the AngryBirds application

## Setup

- [Running AIBirds](#running-aibirds)
	- [Setup](#setup)
		- [Game and ABServer](#game-and-abserver)
		- [Setting up Chrome](#setting-up-chrome)
		- [Modify DNS records](#modify-dns-records)
		- [Running the Game](#running-the-game)
			- [Using a local web server](#using-a-local-web-server)
			- [Using Cache](#using-cache)
		- [Java Setup](#java-setup)
- [Run the Game, Server and Naive Agent](#run-the-game-server-and-naive-agent)
- [Additional Ressources](#additional-ressources)


### Game and ABServer

1. The AngryBirds application should be extracted to [./slingshot/](slingshot/). To get the application [contact the Organizers](http://aibirds.org/organisers.html)
2. The ABServer can be downloaded [here](http://aibirds.org/Software/AngryBirds/abV1.32.zip) and should be extracted in the current folder.


### Setting up Chrome
1. See [Organizers information](http://aibirds.org/basic-game-playing-software/offline-chrome.html) on which version to use.
2. Install the AIBirds Chrome extension: Open Chrome and navigate to [chrome://extensions/](chrome://extensions/). Check the `Developer mode` box, click `Load unpacked extension...` and choose the folder `abVx.xx/plugin`.

### Modify DNS records
1. Open the following file in a simple text editor as admin (e.g. Notepad on Windows or TextEdit on OS X). Note that some folders might be hidden.
   1.  Windows: `C:\WINDOWS\system32\drivers\etc\hosts`, 
   2.  OS X and Linux: `/etc/hosts`. 
2. Add this line to the bottom of the document:
`127.0.0.1       chrome.angrybirds.com`. Save the file.
3. Open a browser window and try to visit [`chrome.angrybirds.com`](http://chrome.angrybirds.com). An error message should be shown (e.g. "This site can't be reached").

### Running the Game
The game can be run in two different ways:

#### Using a local web server

This needs to be done every time you want to start the ABServer:

1. Open a new shell / terminal window and navigate to your `slingshot/` folder.
2. Start a local webserver on port 80 :
 - `php -S 127.0.0.1:80`
 - `python -m SimpleHTTPServer 80`
 - `python3 -m http.server 80`
 - alternatively use a graphical interface like [XAMPP](https://www.apachefriends.org/de/index.html) or [MAMP](https://www.mamp.info/en/)
3. If you get a permission denied error message you have to re-run the command with administrative privileges (Unix: `sudo !!`).
4. Open a new Chrome tab on `chrome.angrybirds.com` and make sure the game is running.


#### Using Cache

This needs to be done only once or after clearing the cache of your chrome installation:

1. Quit Chrome. 
2. Open the following path in your systems file explorer. Replace `$user$` with your user name and `$chrome$` with your version of Chrome (e.g. `google-chrome` for Chrome on Linux).  Windows: `C:\\Users\$user$\AppData\Local\Google\$chrome$\User Data\Default`, OS X: `/Users/$user$/Library/Application Support/Google/$chrome$/Default`, Linux: `/home/$user$/.config/$chrome$/Default`.
3. Unzip `Application Cache.zip` (it should be contained in the software you [get by the organizers](#game-and-abserver)). Copy the resulting folder `Application Cache` to the directory you've opened. By doing this you probably replace an existing folder.
4. Open Chrome and visit `chrome.angrybirds.com`. Angry Birds should be loading.


### Java Setup
[Download](http://www.oracle.com/technetwork/java/javase/downloads/index.html) and install the JRE (Java Runtime Environment) or JDK (Java Development Kit).

# Run the Game, Server and Naive Agent
After completing the necessary steps described above, you should be able to run the built-in naive agent.
1. Start Chrome and navigate to `chrome.angrybirds.com`. Make sure that the SD - mode is selected! HD does not work.
2. After the game has loaded, click on the big "PLAY" button and select the level pack on the bottom left ("Poached Eggs").
3. Open a terminal in the `abV1.32` folder.
4. Start the server: `java -jar ABServer.jar`.
5. Start the naive agent: `java -jar ABSoftware.jar -nasc`.
6. The server window should inform you about one connected client. Press "Start". The agent should select the first level and start playing! You have to manually confirm any pop-up dialogs.

# Additional Ressources
See `AIBirds-Getting-Started.pdf` and `AIBirds-Chrome-Issues.pdf` for more additional guidance. The Server and Agent is documented in `abV1.32/doc/`.
