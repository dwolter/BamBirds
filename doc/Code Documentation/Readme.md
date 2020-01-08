# Doxygen Documentation

## Requirements

- Install [Doxygen](http://www.stack.nl/~dimitri/doxygen/) (Mac: `brew install doxygen`)
- Install [GraphViz](http://www.graphviz.org/download/) (Mac: `brew install graphviz`)



## Building Documentation

Open your terminal / bash window in the same directory where this readme is located. Then run:

    doxygen configfile


Using the graphical interface of Doxygen you have to click File > Open and select the configuration file `configfile`.

However the GUI may produce errors. If the `dot` program (GraphViz) can not be found html will display no graphs. Normally Doxygen should be able to find `dot` in the run path but somehow this doesn't seem to work (at least for macOS). Contrary, running doxygen from the command line works properly.



## Clean

Delete the `html` directory and rebuild.  
Important files are: `configfile`, `index.html`, `logo.png` and this `readme.md` (please don't delete these)