# BamBird 2021

- [BamBird 2021](#bambird-2021)
  - [Team Description](#team-descriptiondocteam-descriptionsteam_description_2021md)
  - [Repository structure](#repository-structure)
  - [Prerequisities](#prerequisities)
    - [Using AngryBirds (via Chromium)](#using-angrybirds-via-chromium)
    - [Using ScienceBirds](#using-sciencebirds)
  - [Run from distribution](#run-from-distribution)
  - [Run from source](#run-from-source)
    - [With AngryBirds](#with-angrybirds)
      - [With Docker](#with-docker)
    - [With ScienceBirds](#with-sciencebirds)
    - [Using Gradle](#using-gradle)
    - [Configuration](#configuration)
    - [Python execution](#python-execution)
    - [Troubleshooting](#troubleshooting)
    - [Debugging](#debugging)

> Warning: Some of the setup described here is specific to the way the project is setup internally. Some of the things like the angry birds server or our setup of the ABServer cannot be provided publicly because of legal constraints.

> If you want to contribute / collaborate with us, please contact [Diedrich Wolter](mailto:diedrich.wolter@uni-bamberg.de)

## [Team Description](doc/Team Descriptions/team_description_2021.md)

## Repository structure

BamBirds is a gradle project and contains the following modules:

- [common/](common/): Common Utils and Data Structures
- [client/](client/): Client for connecting to ABServer or SBServer
- [vision/](vision/): Vision recognition for AngryBirds and ScienceBirds
- [planner/](planner/): Action planner (More info in [the README](planner/README.md))
  - [src/main/prolog](planner/src/main/prolog/): Prolog Planner
  - [src/main/java](planner/src/main/java/): Knowledge generation and plan parsing for Prolog
  - [behind_the_corner](planner/behind_the_corner): SWI-Prolog native library for rebounds 
- [level_selection/](level_selection/): Module for the Level Selection, containing Models for Prediction of Score and Level Outcome
  - [src/main/java](level_selection/src/main/java): The module for executing models in the agent
  - [src/main/python](level_selection/src/main/python): Python Module for generating Models for Java
  - [src/main/csharp](level_selection/src/main/csharp): C# Module for generating a Dirichlet Model
- [debugging/](debugging/): **WIP** shell for debugging and manual interaction with the modules
- [src/](src/): Bambirds Agent
  - [main/java](src/main/java): Java Code for the agent
  - [main/python](src/main/python): Python code for automated execution of the agent

Additional Resources and Information can be found here:

- [doc/](doc/): documentation, project reports etc.
- [game/](game/): files required to install and start the AngryBirds Game (including
  **instructions in [README](game/README.md)** file), custom levels (with chrome extension), level editor

## Prerequisities

- Install Java 8
- [Install SWI-Prolog >= 8.2](https://www.swi-prolog.org/Download.html)
  - Make sure to set `PATH_TO_SWIPL` in [`config.properties`](#configuration) to the executable on the system if is not located on the `PATH` (System Variables on Windows)
- A c++ compiler for native prolog libraries
  - Windows:
    - Currently not supported unless running in [wsl](https://docs.microsoft.com/en-us/windows/wsl/)
  - Linux, MacOS, WSL:
    - G++ compiler with support for C++11 (gcc, clang)
    - libboost-dev ([Boost C++ development libraries](https://www.boost.org/))
- (Optional) Python >= 3.8 for some modules and evaluation

Or if you want to run with docker:

- docker
- docker-compose

### Using AngryBirds (via Chromium)

- Setup **AngryBirds** and **ABServer** according to [game/README.md](game/README.md)

### Using ScienceBirds

- Clone the [ScienceBirds Framework](https://gitlab.com/aibirds/sciencebirdsframework)
  - Install a Java 12 or higher
  - Extract the ScienceBirds Game in [ScienceBirds folder](https://gitlab.com/aibirds/sciencebirdsframework/-/tree/master/ScienceBirds)

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

## Run from source

Make sure you are in the root directory of the BamBird repository (the same
directory this very file is located in).

### With AngryBirds

1. Open this url in Chromium: [chrome.angrybirds.com](http://chrome.angrybirds.com)

   Make sure [System is correctly configured](game/README.md#setup). You have
   to select _SD_ mode of the Angry Birds game, otherwise the Agent won't
   work.

2. Run the AIBirds server:

   Open a terminal and enter the following command:

   ```bash
   ./gradlew game:abserver:run
   ```

   This will open the server application.

3. Run the Agent:

   Open another terminal and enter this command:

   ```bash
   ./gradlew :run
   ```

   Now the server application should have detected a connected client. Press
   _Start_ in the server application.

   > Make sure to set `SERVER_TYPE=ANGRY_BIRDS` in `config.properties`

4. Shutdown:

   **Server**: simply close the server application window or open the terminal in which you started the server and press `Ctrl-C`.

   **Agent**: open the terminal in which you started the agent and press `Ctrl-C`.

#### With Docker

You can simplify the approach with docker. By default the `docker-compose.yml` file only contains the game and connection server.

```sh
docker-compose up -d
```

Then open the URL in Chromium [chrome.angrybirds.com](http://chrome.angrybirds.com). Make sure [System is correctly configured](game/README.md#setup) ([Running the game](game/README.md#running-the-game) is already done with the previous command)

To run the agent in docker as well, you can create a `docker-compose.override.yml` with the following content:

```yml
services:
  agent:
    image: registry.sme.uni-bamberg.de/bambirds/testing/build
    working_dir: /bambirds
    # If your user id is not 1000, change it to your userid (`id -u` on linux)
    #
    # It is not required, but highly advised to set this, since then all generated gradle files are not
    # created with the wrong permissions
    user: "1000"
    volumes:
      - ./:/bambirds
    command:
      - ./gradlew 
      - :run
      - --args=-h abserver
```
Then start this container with
```sh
docker-compose up -d agent
```

If you want to execute other commands inside the agent, replace the command with your command or the following:
```yaml
    command:
      - sh
      - -c
      - while :; do sleep 2073600; done
```
Then use this to execute a command inside the container (`bash` for normal shell):
```sh
docker-compose exec agent <your command>
```

> Important is here, that this will  most of the time not work on native windows because of the nasty `\r` and the different filesystem.
> If you want to run on windows, you are better off cloning the project in [wsl](https://docs.microsoft.com/en-us/windows/wsl/install-win10).

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
   ./gradlew :run
   ```

   > Make sure to set `SERVER_TYPE=SCIENCE_BIRDS` in `config.properties`

4. Shutdown:

   **Server**: open the terminal in which you started the server and press `Ctrl-C`.

   **Agent**: open the terminal in which you started the agent and press `Ctrl-C`.

### Using Gradle

The BamBird agent can be compiled, run and packaged
using [Gradle](https://gradle.org/).

Use one of the following tasks:

- **`tasks`**

  Display all available Tasks

- **`game:abserver:run`**

  Execute the AIBirds server (`game/abserver`).

- **`:run`**

  Execute the BamBird agent (`de.uniba.sme.bambirds.BamBirds`).

  [Configuration](#configuration) should be done via the `config.properties` file.

  For testing purposes the default values should be just fine.

  > The `:` before `run` is required to only run the root project and not other projects with the application plugin

- **`test`**

  Run JUnit tests

- **`distTar`/`distZip`**

  Create a tar/zip containing executables, all dependencies and Prolog files in the `build/distributions/` directory. Files in [src/main/dist](src/main/dist) will be included.

- `compileJava`

  Compile all Java files

- `clean`

  Remove the `build/` directory.

- `javadoc`

  Generate the javadoc in `build/docs/javadoc` directory

- `<subproject>:<task>`
  Execute a Task for a subproject

### Configuration

All configuration is saved in `config.properties` (file is created on the first execution of the agent):

| Name                                    |             Options/Type              | Use                                                                                                                                                                                                                             | Access in source                                 |
| :-------------------------------------- | :-----------------------------------: | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | :----------------------------------------------- |
| `HOST`                                  |        any server domain or ip        | the location of ABServer or SBServer                                                                                                                                                                                            | `Settings.SERVER_HOST`                           |
| `GAME_MODE`                             | `COMPETITION` \| `TRAINING` \| `DEMO` | can be used for changing mode of Agent between competition or training. The demo mode enables the visual export of the generated plans and display in Java (only supported on Unix Systems, requires `pdflatex` and `convert`). | `Settings.GAME_MODE`                             |
| `ROUNDS`                                |              `int` value              | when level-selection disabled the number of complete rounds to play, `-1` for unlimited                                                                                                                                         | `Settings.ROUNDS`                                |
| `TEAM_ID`                               |             `uint` value              | The id used to connect to server                                                                                                                                                                                                | `Settings.TEAM_ID`                               |
| `SERVER_TYPE`                           |   `ANGRY_BIRDS` \| `SCIENCE_BIRDS`    | Switch between the ABServer and SBServer (they have slightly different APIs)                                                                                                                                                    | `Settings.SERVER_TYPE`                           |
| `START_LEVEL`                           |              `int` value              | The id of the first level to play                                                                                                                                                                                               | `Settings.START_LEVEL`                           |
| `PATH_TO_SWIPL`                         |         file path or command          | The location of the swipl executable (On windows for example `"C:\\Program Files\\swipl\\bin\\swipl"`)                                                                                                                          | `Settings.PATH_TO_SWIPL`                         |
| `LEVEL_RANGE`                           |              `int` value              | The range of levels to play, 0 for all available, 1 for only the `START_LEVEL`                                                                                                                                                  | `Settings.START_LEVEL`                           |
| `VISUAL_DEBUG_ENABLED`                  |            `boolean` value            | Enable Visual Debugging                                                                                                                                                                                                         | `Settings.VISUAL_DEBUG_ENABLED`                  |
| `EXPORT_LEVEL_STATS`                    |            `boolean` value            | Export level statistics                                                                                                                                                                                                         | `Settings.EXPORT_LEVEL_STATS`                    |
| `DISABLE_LEVEL_SELECTION`               |            `boolean` value            | Disable the level seletction completely and run levels iterative                                                                                                                                                                | `Settings.DISABLE_LEVEL_SELECTION`               |
| `LEVEL_SELECTION_FIRST_ROUND_ITERATIVE` |            `boolean` value            | Run the first round of levels iterative                                                                                                                                                                                         | `Settings.LEVEL_SELECTION_FIRST_ROUND_ITERATIVE` |

Optionally also cli arguments can be passed to gradle with

```bash
./gradlew run --args='--help'
```

### Python execution

Some modules and scripts are written in python. The entrypoint is `main.py` where the entrypoints for the modules are reqistered.

Before execution you need to install some dependencies:
```
pip install -r requirements.txt
```

Then run `main.py` to see all registered modules:
```
python main.py --help
```

### Troubleshooting

- Verify that Gradle picks the correct Java version by running `./gradlew --version`.
- Verify that you have the correct [Chromium version](http://aibirds.org/basic-game-playing-software/offline-chrome.html)
- Verify that you can execute swi-prolog from the command line: `swipl` (or the path you have set in `config.properties`)

### Debugging

The Debugging can be done also via gradle with an external debugger like in eclipse, intellij or vscode (with the "Debugger for Java" extension)

Run the jvm in debugging mode with

```bash
./gradlew :run --debug-jvm
```

and then attach to it on Port 5005. For VSCode the debugging task "Debug (Attach) - Local" will do exactly that.
