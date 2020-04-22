# BamBird 2020
- [BamBird 2020](#bambird-2020)
	- [Team Description](#team-description)
	- [Repository structure](#repository-structure)
	- [Prerequisities](#prerequisities)
		- [Using AngryBirds (via Chrome)](#using-angrybirds-via-chrome)
		- [Using ScienceBirds](#using-sciencebirds)
	- [Run from distribution](#run-from-distribution)
	- [Run from source](#run-from-source)
		- [With AngryBirds](#with-angrybirds)
		- [With ScienceBirds](#with-sciencebirds)
		- [Using Gradle](#using-gradle)
		- [Debugging](#debugging)

## [Team Description](doc/team_description_2019.md)

## Repository structure

BamBirds is a gradle project and contains the following modules:


- [common/](common/): Common Utils and Data Structures
- [client/](client/): Client for connecting to ABServer or SBServer
- [vision/](vision/): Vision recognition for AngryBirds and ScienceBirds
- [planner/](planner/): Action planner
  - [src/main/prolog](planner/src/main/prolog/): Prolog Planner (also linked from [Prolog/](Prolog/))
  - [src/main/java](planner/src/main/java/): Knowledge generation and plan parsing for Prolog
- [level_selection/](level_selection/): Module for the Level Selection, containing Models for Prediction of Score and Level Outcome
  - [src/main/java](level_selection/src/main/java): The module for executing models in the agent
  - [src/main/python](level_selection/src/main/python): Python Module for generating Models for Java
  - [src/main/csharp](level_selection/src/main/csharp): C# Module for generating a Dirichlet Model
- [debugging/](debugging/): **WIP** shell for debugging and manual interaction with the modules
- [src/](src/): Bambirds Agent
  - [main/java](src/main/java): Java Code for the agent
  - [main/python](src/main/python): Python code for automated execution of the agent

Additional Resources and Information can be found here:

- [doc/](doc/): documentation, etc.
- [game/](game/): files required to install and start the AngryBirds Game (including
  **instructions in [README](game/README.md)** file)

## Prerequisities

* Install Java 8
* [Install Prolog](https://www.swi-prolog.org/Download.html)

### Using AngryBirds (via Chrome)

* Setup **AngryBirds** and **ABServer** according to [game/README.md](game/README.md)

### Using ScienceBirds
* Clone the [ScienceBirds Framework](https://gitlab.com/aibirds/sciencebirdsframework)
  * Install a Java 12 or higher
  * Extract the ScienceBirds Game in [ScienceBirds folder](https://gitlab.com/aibirds/sciencebirdsframework/-/tree/master/ScienceBirds)


## Run from distribution

The distribution contains a `bin` and a `lib` folder.

To run the agent, run the command 
```bash 
./bin/bambirds
```

Configuration can be done either with the `config.properties` file or with commandline options.

For more information run 
```bash 
./bin/bambirds --help
```

> ***Important:***
> 
> The directory `Prolog/` will be exported. Make sure nothing gets overwritten!
> 
> The path to the prolog executable must not contain any spaces!

## Run from source

Make sure you are in the root directory of the BamBird repository (the same
directory this very file is located in).

### With AngryBirds

1. Open this url in Chrome: [chrome.angrybirds.com](http://chrome.angrybirds.com)

	Make sure [System is correctly configured](game/README.md#setup). You have
	to select *SD* mode of the Angry Birds game, otherwise the Agent won't
	work.

2. Run the AIBirds server:

	Open a terminal and enter the following command:

	```bash
	./gradlew run_server

	# Or manually

	java -jar game/abV1.33/ABServer.jar -t <time limit> -l <number of levels>
	```

	This will open the server application.

3. Run the Agent:

	Open another terminal and enter this command:

	```bash
	./gradlew run
	```

	Now the server application should have detected a connected client. Press
	*Start* in the server application.

	> Make sure to set `SERVER_TYPE=ANGRY_BIRDS` in `config.properties`

4. Shutdown: 
	
	**Server**: simply close the server application window or open the terminal in which you started the server and press `Ctrl-C`.

	**Agent**: open the terminal in which you started the agent and press `Ctrl-C`. 

### With ScienceBirds

1. Start the ScienceBirds Unity Game

2. Run the AIBirds server:

	Open the folder sciencebirdsframework and use java 12 or higher to run. For example:

	```bash
	/usr/lib/jvm/java-13-openjdk/bin/java -jar game_playing_interface.jar
	```

	This will start the server application.

3. Run the Agent:

	Open another terminal and enter this command:

	```bash
	./gradlew run
	```

	> Make sure to set `SERVER_TYPE=SCIENCE_BIRDS` in `config.properties`

4. Shutdown: 
	
	**Server**: simply close the server application window or open the terminal in which you started the server and press `Ctrl-C`.

	**Agent**: open the terminal in which you started the agent and press `Ctrl-C`. 

### Using Gradle


The BamBird agent can be compiled, run and packaged
using [Gradle](https://gradle.org/).

Use one of the following tasks:

- **`tasks`**

	Display all available Tasks

- **`run_server`**

	Execute the AIBirds server (`game/abV1.33/ABServer.jar`).

- **`run_server_old`**

	Execute the old AIBirds server (`game/abV1.32/ABServer.jar`).

- **`run`**

	Execute the BamBird agent (`de.uniba.sme.bambirds.BamBirds`).

	> ***Note:***
	>
	> The Planner expects the Prolog files to be in a directory called `./Prolog/` (relative to the current working directory, i.e. the directory you are currently in). If they are missing the planning will not work. There is already a symbolic link pointing to `planner/src/main/prolog/planner` in the project root, but if your system does not support linux symlinks you need to create a new one.	

	Configuration should be done via the `config.properties` file.

	For testing purposes the default values should be just fine.

- **`test`**

	Run JUnit tests

- **`distTar`/`distZip`**

	Create a tar/zip containing executables, all dependencies and Prolog files in the `build/distributions/` directory. Files in [src/main/dist](src/main/dist) will be included.

	> ***Note***:
	> 
	> The JAR of `planner/` packs the Prolog planner and will generate file and directory substructures.

- `compileJava`

	Compile all Java files 

- `clean`

	Remove the `build/` directory.

- `javadoc`

	Generate the javadoc in `build/docs/javadoc` directory

- `<subproject>:<task>`
	Execute a Task for a subproject

### Debugging

The Debugging can be done also via gradle with an external debugger like in eclipse, intellij or vscode (with the "Debugger for Java" extension)

Run the jvm in debugging mode with
```bash
./gradlew run --debug-jvm
```
and then attach to it on Port 5005. For VSCode the debugging task "Debug (Attach) - Local" will do exactly that.