



Contents
========

* [**gplot**](#gplot)
	* [line: 11 - `__init__`](#line-11---__init__)
	* [line: 40 - `_get_image_file_name`](#line-40---_get_image_file_name)
	* [line: 52 - `_yesno_bool`](#line-52---_yesno_bool)
	* [line: 72 - `_check_init`](#line-72---_check_init)
	* [line: 79 - `csg`](#line-79---csg)
	* [line: 97 - `width`](#line-97---width)
	* [line: 110 - `symht`](#line-110---symht)
	* [line: 123 - `style`](#line-123---style)
	* [line: 144 - `colour`](#line-144---colour)
	* [line: 158 - `xyplot`](#line-158---xyplot)
	* [line: 191 - `xyplot_yerrors`](#line-191---xyplot_yerrors)
	* [line: 225 - `xyplot_yerrors_asym`](#line-225---xyplot_yerrors_asym)
	* [line: 261 - `xyplot_xyerrors`](#line-261---xyplot_xyerrors)
	* [line: 297 - `xyline`](#line-297---xyline)
	* [line: 307 - `xypoint`](#line-307---xypoint)
	* [line: 317 - `xyhistogram`](#line-317---xyhistogram)
	* [line: 327 - `grmove`](#line-327---grmove)
	* [line: 338 - `grdraw`](#line-338---grdraw)
	* [line: 349 - `xyauto`](#line-349---xyauto)
	* [line: 360 - `xrange`](#line-360---xrange)
	* [line: 371 - `yrange`](#line-371---yrange)
	* [line: 382 - `xysame`](#line-382---xysame)
	* [line: 393 - `xlinear`](#line-393---xlinear)
	* [line: 404 - `ylinear`](#line-404---ylinear)
	* [line: 415 - `xlog`](#line-415---xlog)
	* [line: 426 - `ylog`](#line-426---ylog)
	* [line: 437 - `intvalues`](#line-437---intvalues)
	* [line: 456 - `grid`](#line-456---grid)
	* [line: 475 - `outline`](#line-475---outline)
	* [line: 494 - `interpolate`](#line-494---interpolate)
	* [line: 513 - `histstyle`](#line-513---histstyle)
	* [line: 535 - `annotate`](#line-535---annotate)
	* [line: 548 - `rightannot`](#line-548---rightannot)
	* [line: 561 - `title`](#line-561---title)
	* [line: 575 - `xlabel`](#line-575---xlabel)
	* [line: 586 - `ylabel`](#line-586---ylabel)
	* [line: 597 - `rylabel`](#line-597---rylabel)
	* [line: 608 - `gstyle`](#line-608---gstyle)
	* [line: 627 - `axcut`](#line-627---axcut)
	* [line: 638 - `drawaxes`](#line-638---drawaxes)
	* [line: 651 - `marker`](#line-651---marker)
	* [line: 675 - `usekey`](#line-675---usekey)
	* [line: 688 - `addkey`](#line-688---addkey)
	* [line: 700 - `graphmode`](#line-700---graphmode)
	* [line: 713 - `subfig`](#line-713---subfig)
	* [line: 727 - `glabel`](#line-727---glabel)
	* [line: 741 - `command`](#line-741---command)
	* [line: 752 - `drawkeys`](#line-752---drawkeys)
	* [line: 764 - `reset`](#line-764---reset)
	* [line: 775 - `draw`](#line-775---draw)


&nbsp;

--------

--------
# **gplot**

```
Provide a Python callable interface to GPLOT.
This can be used to plot graphs (and potentially draw other things) from Python code.
```

--------
## line: 11 - `__init__`

```
def __init__(self, device='svg', size=None, offset=None, throwup=True):
```


> Create a `gplot` class instance (constructor).

--------
## line: 40 - `_get_image_file_name`

```
def _get_image_file_name(self):
```
>Internal: Get the current output image file.

--------
## line: 52 - `_yesno_bool`

```
def _yesno_bool(self, yn):
```
>Allow either boolean or strings for "yes/no" arguments to functions.  
>Permit `y[es]`, `n[o]`, `on`, `off` or `True`, `False`

--------
## line: 72 - `_check_init`

```
def _check_init(self):
```
>If initialisation failed and throwing exceptions, throw `RuntimeError`.

--------
## line: 79 - `csg`

```
def csg(self, which):
```
>Choose the colour/style group to which the following `colour()`, `width()` apply.

--------
## line: 97 - `width`

```
def width(self, width):
```
>Set line drawing width.

--------
## line: 110 - `symht`

```
def symht(self, height):
```
>Set symbol drawing height.

--------
## line: 123 - `style`

```
def style(self, which, length=None):
```
>Choose the line drawing style.

--------
## line: 144 - `colour`

```
def colour(self, r, g, b):
```
>Set the drawing colour for the current colour/style group.

--------
## line: 158 - `xyplot`

```
def xyplot(self, x, y, cmd):
```
>Draw `xyline`, `xypoint` or `xyhistogram`.

--------
## line: 191 - `xyplot_yerrors`

```
def xyplot_yerrors(self, x, y, e, cmd):
```
>Draw `xyline`, `xypoint` with symmetric Y error bars

--------
## line: 225 - `xyplot_yerrors_asym`

```
def xyplot_yerrors_asym(self, x, y, e1, e2, cmd):
```
>Draw `xyline`, `xypoint` with asymmetric Y error bars

--------
## line: 261 - `xyplot_xyerrors`

```
def xyplot_xyerrors(self, x, y, ex, ey, cmd):
```
>Draw `xyline`, `xypoint` with symmetric X and Y error bars

--------
## line: 297 - `xyline`

```
def xyline(self, x, y):
```
>Draw a line plot taking points from `x` and `y` arrays or lists.

--------
## line: 307 - `xypoint`

```
def xypoint(self, x, y):
```
>Draw a point plot taking points from `x` and `y` arrays or lists.

--------
## line: 317 - `xyhistogram`

```
def xyhistogram(self, x, y):
```
>Draw a histogram plot taking points from `x` and `y` arrays or lists.

--------
## line: 327 - `grmove`

```
def grmove(self, x, y):
```
>Move to graph coordinates `(x,y)`

--------
## line: 338 - `grdraw`

```
def grdraw(self, x, y):
```
>Draw to graph coordinates `(x,y)`

--------
## line: 349 - `xyauto`

```
def xyauto(self):
```
>Auto range both axes.

--------
## line: 360 - `xrange`

```
def xrange(self, xlo, xhi):
```
>Set X axis range.

--------
## line: 371 - `yrange`

```
def yrange(self, ylo, yhi):
```
>Set Y axis range.

--------
## line: 382 - `xysame`

```
def xysame(self):
```
>Keep last (auto determined) ranges on both axes.

--------
## line: 393 - `xlinear`

```
def xlinear(self):
```
>Set X axis to linear.

--------
## line: 404 - `ylinear`

```
def ylinear(self):
```
>Set Y axis to linear.

--------
## line: 415 - `xlog`

```
def xlog(self):
```
>Set X axis to log.

--------
## line: 426 - `ylog`

```
def ylog(self):
```
>Set Y axis to log.

--------
## line: 437 - `intvalues`

```
def intvalues(self, axisname):
```
>Try to use integer values on one, both or no axes.
>`axisname`: `none`, `x`, `y` or `both`

--------
## line: 456 - `grid`

```
def grid(self, axisname):
```
>Draw grid lines along one, both or no axes.
>`axisname`: `none`, `x`, `y` or `both`

--------
## line: 475 - `outline`

```
def outline(self, objname):
```
>Outline some feature.
>`objname`: `pane`, `blank`, `bounds`, `device`

--------
## line: 494 - `interpolate`

```
def interpolate(self, interpname):
```
>Set graph plotting interpolation type.
>`interpname`: `linear`, `cubic`, `quintic`

--------
## line: 513 - `histstyle`

```
def histstyle(self, histmode, width=None):
```
>Set histogram drawing style.
>`histmode`: `abut`, `abut+shade`, `lines`, `wide`, `wide+shade`

--------
## line: 535 - `annotate`

```
def annotate(self, uonoff):
```
>Turn graph annotation on or off.

--------
## line: 548 - `rightannot`

```
def rightannot(self, uonoff):
```
>Turn right edge graph annotation on or off.

--------
## line: 561 - `title`

```
def title(self, text, lower=False):
```
>Set the graph title.

--------
## line: 575 - `xlabel`

```
def xlabel(self, text):
```
>Set the X axis label.

--------
## line: 586 - `ylabel`

```
def ylabel(self, text):
```
>Set the Y axis label.

--------
## line: 597 - `rylabel`

```
def rylabel(self, text):
```
>Set the right side Y axis label.

--------
## line: 608 - `gstyle`

```
def gstyle(self, stylename):
```
>Set graph plotting style.
>`stylename`: `boxed`, `axes`, `open`

--------
## line: 627 - `axcut`

```
def axcut(self, xo, yo):
```
>Set the point through which drawn axes should pass.

--------
## line: 638 - `drawaxes`

```
def drawaxes(self, uonoff):
```
>Draw axes, or not.

--------
## line: 651 - `marker`

```
def marker(self, which):
```
>Set the marker to be used to plot points.

--------
## line: 675 - `usekey`

```
def usekey(self):
```
>Prepare to create a key for the graph.

--------
## line: 688 - `addkey`

```
def addkey(self, text):
```
>Add a key for the last specified `xyline` or `xypoint`.

--------
## line: 700 - `graphmode`

```
def graphmode(self, uonoff):
```
>Turn graphmode on or off.

--------
## line: 713 - `subfig`

```
def subfig(self, nx, ny, ix, iy, shrink=None):
```
>Draw the next graph as a sub-figure, `(ix,iy)` in a grid of `nx` x `ny`.

--------
## line: 727 - `glabel`

```
def glabel(self, gx, gy, length, angle, text):
```
>Draw a label with an arrow and boxed text.
>`(gx,gy)` are graph coordinates to which the arrow points.
>`length` is the length of the arrow shaft in bounds units.
>`angle` is the angle of the arrow shaft in degrees w.r.t. `+X`.

--------
## line: 741 - `command`

```
def command(self, cmd):
```
>Add an arbitrary **GPLOT** command. Useful for tests.

--------
## line: 752 - `drawkeys`

```
def drawkeys(self):
```
>Draw the keys legend panel.

--------
## line: 764 - `reset`

```
def reset(self):
```
>Reset state.

--------
## line: 775 - `draw`

```
def draw(self, outfile=None):
```
>Draw the accumulated graph description. Clear the commands.
>Prepare for the next frame.
