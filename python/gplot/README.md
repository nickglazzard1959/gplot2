Using GPLOT from Python programs
================================

Plotting data from inside a Python program is often useful (pretty obviously!).
The package universally used for this is the excellent `matplotlib` but a small
Python library can be used to plot data with GPLOT too.

The library  can be installed using:

```
cd python/gplot
pip install .
```

Using a virtual environment, as described in the documentation for the `tools`
programs (see [here](../../README.md#itiot)), is highly recommended. These tools
must also be installed, as they include viewers for SVG and EPS files which the
library will potentially try to run.

After installation, you can `import gplot` and use the `gplot` class to provide
access to the graph plotting subset of GPLOT commands.

Note that behind the scenes, this works in the simplest way possible: a list of GPLOT
commands is compiled, and the GPLOT program is run to execute them, generating (usually)
an SVG format file which can be displayed (with `svgview`) or renamed to preserve the
output file.

Although this is a "simple" approach, it is quite effective on modern hardware (GPLOT/DIMFILM
is very "lightweight" by today's standards) and is not slow. In fact, the Python based `svgview`
program is often significantly slower to run than GPLOT, although the combination is quite
fast enough for normal purposes.

A minimal example of the use of the `gplot` module is:
```
import gplot
import numpy as np
x=np.linspace(0,7,101)
y=np.sin(x)
plt = gplot.gplot()
plt.xyline(x,y)
plt.draw()
```

This will display a cycle (and bit) of a sine wave in a "pop-up" window when `draw()` is called.

The default size is 800 by 800 pixels. On a 4K display, a nice size is 2048 x 1280, while a good HD display size
is 1280 x 800. To set a size use, for example:
```
plt = gplot.gplot(size=(2048,1280))
```

The default output device is `svg`. Any GPLOT device can be used, including `epscol` for EPS format files. In this case,
the `offset=(0,0)` option to the constructor might be useful to remove or adjust the defualt
"margin" of half an inch (note that sizes and offsets are in inches for EPS).

The names of the `gplot` class functions are usually identical to the GPLOT command names they generate
and the arguments, when they are names, also follow the names used in GPLOT, so the "cheat sheet" and other
GPLOT documentation can be used directly for this Python interface.

There are a few exceptions:

- When `on` and `off` values are concerned, these can be given as strings, or `"yes"` and `"no"` can be used
  or `True` and `False` can be used.
- Unlike GPLOT commands, Python function names cannot be abbreviated (!), so `csgroup` has been shortened to
  `csg`. 
- There are three separate functions to plot data with error bars (as the mechanism GPLOT uses to distinguish
  these types isn't applicable to Python). These are: `xyplot_yerrors()`, `xyplot_yerrors_asym()` and
  `xyplot_xyerrors()`. These can be given a `cmd` argument of `xyline` or `xypoint` to control the type of
  plot to generate.
- The marker to be used for points can be specified with a single character string or with a number. This
  may make selecting commonly used markers simpler.
- If drawing a single graph (without sub-figures), it isn't necessary to use `key()` to draw the key. This *is*
  needed, though, for all sub-figures except the last if a plot with multiple sub-figures and keys is being
  made.
- There is an "escape mechanism" that allows any GPLOT command to be inserted in to the command stream
  executed when `draw()` is called: `command("command text")`. This can be used to run the evaluator,
  for example.
  
Speaking of exceptions, the `gplot` module will throw exceptions to signal error conditions by default. It is 
possible to choose boolean condition code returns instead by setting `throwup=False` in the constructor. The
exceptions thrown are:

- `ValueError` when improper arguments are supplied to a function.
- `RuntimeError` if there is any problem related to running GPLOT or a viewing program, or if the
  constructor failed (which will throw `ValueError`) and the user has insisted on continuing anyway.
  
Automatic documentation for the `gplot` class can be found [here](https://github.com/nickglazzard1959/gplot2/blob/3d3e4bc703e5e0eab72d2458cace12519f5cd30d/python/gplot/gplot.gplot.html). This was created with:
```
python -m pydoc -w gplot.gplot
```
and hopefully will answer any detailed questions.

The subset of tutorial examples that cover graph plotting have been re-implemented in Python
in the file: `python/testpyif.py` to serve as examples and also to test almost all features
of the `gplot` module.

To run this, just use:
```
python testpyif.py
```
This will plot each tutorial example in turn.
