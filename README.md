# BamBird 2019
- [BamBird 2019](#bambird-2019)
	- [Team Description](#team-description)
	- [Repository structure](#repository-structure)
	- [Prerequisities](#prerequisities)
	- [Run from compiled jar](#run-from-compiled-jar)
	- [Run from source](#run-from-source)
		- [Using Ant](#using-ant)

## [Team Description](doc/team_description_2019.md)

## Repository structure

- [game/](game/): files required to install and start the game (including
  **instructions in [README](game/README.md)** file)
- [src/](src/): source code of our agent
  - [src/Java/jars/](src/Java/jars/): Java libraries necessary to compile and run the agent
  - [src/Java/src/](src/Java/src/): Java source code
  - [src/Prolog/](src/Prolog/): Prolog source code
  - [src/Python/](src/Python/): Python source code
- [doc/](doc/): documentation, etc.

## Prerequisities

* Setup **AngryBirds** and **ABServer** according to [game/README.md](game/README.md)
* Install Java 8
* [Install Prolog](https://www.swi-prolog.org/Download.html)
* [Install ant](https://ant.apache.org/manual/install.html) if running from source

## Run from compiled jar

To run the agent, run the command 
```bash 
java -jar BamBird.jar <hostname> <teamid> <path_to_prolog> [<starting_level> <rounds> <range>]
```


| variable           | use                                                                                                                                    |
| ------------------ | -------------------------------------------------------------------------------------------------------------------------------------- |
| `<hostname>`       | name of the server                                                                                                                     |
| `<teamid> `        | ID of the BamBirds team                                                                                                                |
| `<path_to_prolog>` | path to SWI-Prolog (swipl) in your machine                                                                                             |
| `<starting_level>` | starting level for the agent (levels with lower ID's will not be played)                                                               |
| `<rounds>`         | If running in [`PERFORMANCE_MEASUREMENT_ENABLED`](src/Java/src/helper/Constants.java) mode, number of rounds played. `-1` for infinite |
| `<range>`          | Number of level ID's played after the starting level. `0` for maximum provided by server                                               |

> ***Important:***
> 
> The directory `Prolog/` will be exported from the jar. Make sure nothing gets overwritten!
> 
> The path to the prolog executable must not contain any spaces!

## Run from source

Make sure you are in the root directory of the BamBird repository (the same
directory this very file is located in).

1. Open this url in Chrome: [chrome.angrybirds.com](http://chrome.angrybirds.com)

	Make sure [System is correctly configured](game/README.md#setup). You have
	to select *SD* mode of the Angry Birds game, otherwise the Agent won't
	work.

2. Run the AIBirds server:

	Open a terminal and enter the following command:

	```
	ant run-server
	```

	This will open the server application.

3. Run the Agent:

	Open another terminal and enter this command:

	```
	ant run
	```

	Now the server application should have detected a connected client. Press
	*Start* in the server application.

4. Shutdown: 
	
	**Server**: simply close the server application window or open the terminal in which you started the server and press `Ctrl-C`.

	**Agent**: open the terminal in which you started the agent and press `Ctrl-C`. 

### Using Ant


The BamBird agent can be compiled, run and packaged
using [Apache Ant](https://ant.apache.org/). Commandline arguments can be passed like this: `-D<arg>=<val>`

Use one of the following targets:

- **`run-server`**

	Execute the AIBirds server (`game/abV1.33/ABServer.jar`).

	| arg      | use                                                | default |
	| -------- | -------------------------------------------------- | :-----: |
	| `time`   | Time the agent(s) have to play (current max `127`) |  `30`   |
	| `levels` | Number of levels available to the agents           |  `21`   |

- **`run-server-old`**

	Execute the old AIBirds server (`game/abV1.32/ABServer.jar`).

- **`run`**

	Execute the BamBird agent (`main.BamBird.class`).

	> ***Note:***
	>
	> BamBird expects the Prolog files to be in a directory called `./Prolog/` (relative to the current working directory, i.e. the directory you are currently in). If they are missing the planning will not work. There is already a symbolic link pointing to `src/Prolog` in the project root, but if your system does not support linux symlinks you need to create a new one.

	| arg      | use                                                                                                                                    |     default      |
	| -------- | -------------------------------------------------------------------------------------------------------------------------------------- | :--------------: |
	| `host`   | name of the server                                                                                                                     |   `localhost`    |
	| `team`   | ID of the BamBirds team                                                                                                                |       `0`        |
	| `swipl`  | path to SWI-Prolog (swipl) in your machine                                                                                             | `/usr/bin/swipl` |
	| `level`  | starting level for the agent (levels with lower ID's will not be played)                                                               |       `1`        |
	| `rounds` | If running in [`PERFORMANCE_MEASUREMENT_ENABLED`](src/Java/src/helper/Constants.java) mode, number of rounds played. `-1` for infinite |       `-1`       |
	| `range`  | Number of level ID's played after the starting level. `0` for maximum                                                                  |       `0`        |

	For example:
	```bash
	ant run -Dhost=localhost -Dteam=12345 -Dswipl=/usr/bin/swipl 
	```

	For testing purposes the default values should be just fine.

- **`dist`**

	Create a JAR containing all compiled Java classes (including the library
    JARs) and Prolog files in the `dist/` directory.

	> ***Note***:
	> 
	> The JAR packs the Prolog planner and will generate file and directory substructures. In order to add new Prolog files to the agent the JAVA source code has to be changed to also copy these files from the JAR


- `prepare`

	Create the directories in which the compiled files will be stored. This
    does not have to be called manually.

- `compile`

	Compile all Java files in the Java source directory to the `build/`
    directory and copy the Prolog files over. This does not have to be called
    manually.

- `clean`

	Remove the `build/` and `dist/` directories.
