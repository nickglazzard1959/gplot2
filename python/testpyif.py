import gplot
import numpy as np

def obgraf1(plt, dodraw=True):
    """
    Simplest graph
    """
    plt.command('memtest')
    plt.title("A very simple graph")
    plt.xlabel("X axis")
    plt.ylabel("Y axis")
    plt.command('xyline')
    plt.outline('device')
    if dodraw:
        plt.draw()

def obgrf1a(plt):
    """
    Simplest graph with bounds
    """
    obgraf1(plt, False)
    plt.outline('bounds')
    plt.draw()

def obgraf2(plt):
    """
    Using colour/style groups
    """
    plt.graphmode(True)
    plt.command('memtest')
    plt.csg('general')
    plt.colour( 1, 0, 0 )
    plt.csg('text')
    plt.colour( 0, 0, 0 )
    plt.csg('annot')
    plt.colour( 0, 0.1, 1 )
    plt.title("A simple graph with better colours")
    plt.xlabel("X axis")
    plt.ylabel("Y axis")
    plt.command('xyline')
    plt.outline('device')
    plt.draw()

def obgraf3(plt):
    """
    Add a grid
    """
    plt.graphmode(True)
    plt.command('memtest')
    plt.csg('general')
    plt.colour( 1, 0, 0 )
    plt.csg('text')
    plt.colour( 0, 0, 0 )
    plt.csg('annot')
    plt.colour( 0, 0.1, 1 )
    plt.title("A simple graph with a grid")
    plt.xlabel("X axis")
    plt.ylabel("Y axis")
    plt.grid('both')
    plt.command('xyline')
    plt.outline('device')
    plt.draw()

def obgraf4(plt):
    """
    Graph plot with wider line
    """
    plt.graphmode(True)
    plt.command('memtest')
    plt.csg('general')
    plt.colour( 1, 0, 0 )
    plt.width(3)
    plt.csg('text')
    plt.colour( 0, 0, 0 )
    plt.csg('annot')
    plt.colour( 0, 0.1, 1 )
    plt.title("A simple graph with a wider line")
    plt.xlabel("X axis")
    plt.ylabel("Y axis")
    plt.grid('both')
    plt.command('xyline')
    plt.outline('device')
    plt.draw()

def obgraf5(plt):
    """
    Open style graph
    """
    plt.graphmode(True)
    plt.command('memtest')
    plt.csg('general')
    plt.colour( 1, 0, 0 )
    plt.csg('text')
    plt.colour( 0, 0, 0 )
    plt.csg('annot')
    plt.colour( 0, 0.1, 1 )
    plt.gstyle('open')
    plt.title("An open style simple graph")
    plt.xlabel("X axis")
    plt.ylabel("Y axis")
    plt.command('xyline')
    plt.outline('device')
    plt.draw()

def obgraf7(plt):
    """
    Axes style graph
    """
    plt.graphmode(True)
    plt.command('memtest')
    plt.csg('general')
    plt.colour( 1, 0, 0 )
    plt.csg('text')
    plt.colour( 0, 0, 0 )
    plt.csg('annot')
    plt.colour( 0, 0.1, 1 )
    plt.gstyle('axes')
    plt.title("An axes style simple graph")
    plt.xlabel("X axis")
    plt.ylabel("Y axis")
    plt.command('xyline')
    plt.outline('device')
    plt.draw()

def obgraf8(plt):
    """
    Axes style graph with grid
    """
    plt.graphmode(True)
    plt.command('memtest')
    plt.csg('general')
    plt.colour( 1, 0, 0 )
    plt.csg('text')
    plt.colour( 0, 0, 0 )
    plt.csg('annot')
    plt.colour( 0, 0.1, 1 )
    plt.gstyle('axes')
    plt.grid('both')
    plt.title("Axes style graph with grid")
    plt.xlabel("X axis")
    plt.ylabel("Y axis")
    plt.command('xyline')
    plt.outline('device')
    plt.draw()

def obgrf10(plt):
    """
    2 curve graph with inline data
    """
    plt.graphmode(True)
    plt.gstyle('boxed')
    plt.csg('general')
    plt.colour( 1, 0, 0 )
    plt.width(1)
    plt.style('solid')
    plt.csg('text')
    plt.colour( 0, 0, 0 )
    plt.csg('annot')
    plt.colour( 0, 0.1, 1 )
    x = np.array([0,1,2])
    y = np.array([0,1,0])
    plt.grid('both')
    plt.title("Two curves, same axes, inline data")
    plt.xlabel("X axis")
    plt.ylabel("Y axis")
    plt.xyline(x, y)
    plt.annotate('off')
    plt.xysame()
    x = np.array([0,1,2])
    y = np.array([1,0.1333,1])
    plt.csg('general')
    plt.colour( 1, 0, 1 )
    plt.width(3)
    plt.style('dashdot')
    plt.xyline(x, y)
    plt.outline('device')
    plt.draw()

def obgrf11(plt, full=True):
    """
    3 curve graph with inline data and keys
    """
    if full:
        plt.graphmode(True)
    plt.gstyle('boxed')
    plt.csg('general')
    plt.colour( 1, 0, 0 )
    plt.width(1)
    plt.style('solid')
    plt.csg('text')
    plt.colour( 0, 0, 0 )
    plt.csg('annot')
    plt.colour( 0, 0.1, 1 )
    x = np.array([0,1,2])
    y = np.array([0,1,0])
    plt.grid('both')
    plt.title("Two curves, same axes, inline data")
    plt.xlabel("X axis")
    plt.ylabel("Y axis")
    plt.usekey()
    plt.xyline(x, y)
    plt.addkey("First")
    plt.annotate('off')
    plt.xysame()
    x = np.array([0,1,2])
    y = np.array([1,0.1333,1])
    plt.csg('general')
    plt.colour( 1, 0, 1 )
    plt.width(3)
    plt.style('dashdot')
    plt.xyline(x, y)
    plt.addkey("Second")
    x = np.array([0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2])
    y = np.array([1,0.2,0.8,0.4,0.6,0.5,0.6,0.4,0.8,0.2,1])
    plt.colour( 0.1, 0.7, 0.1 )
    plt.width(2)
    plt.style('solid')
    plt.xyline(x, y)    
    plt.addkey("Third")
    if full:
        plt.draw()
    else:
        plt.drawkeys()

def obgrf12(plt):
    """
    Two curves, different axis ranges
    """
    plt.graphmode(True)
    plt.csg('annot')
    plt.colour( 0, 0, 1 )
    plt.csg('text')
    plt.colour( 0, 0, 1 )
    # --- Generate a sine wave.
    plt.command("EVAL 0.1,TWPI,2,*,0.1,+,201,XLIN,X,SIN")
    # --- Plot it with auto axis ranges.
    plt.xlabel("X")
    plt.ylabel("Sine value")
    plt.title("Two curves, different y axis ranges")
    plt.command('xyline')
    plt.annotate('off')
    # --- Define a triangle wave shape.
    x = np.array([0,5,10])
    y = np.array([0,100,0])
    # --- Plot that with auto axis, put values on right
    plt.csg('all')
    plt.colour( 0, 0, 0 )
    plt.rightannot('on')
    plt.rylabel("Right edge (triangle values)")
    plt.xyline(x, y)
    plt.outline('device')
    plt.draw()

def obgrf13(plt, full=True):
    """
    Two curves, different axis ranges, key
    """
    if full:
        plt.graphmode(True)
    plt.csg('annot')
    plt.colour( 0, 0, 1 )
    plt.csg('text')
    plt.colour( 0, 0, 1 )
    # --- Generate a sine wave.
    plt.command("EVAL 0.1,TWPI,2,*,0.1,+,201,XLIN,X,SIN")
    # --- We will use keys.
    plt.usekey()
    # --- Plot it with auto axis ranges.
    plt.xlabel("X")
    plt.ylabel("Sine value")
    plt.title("Two curves, different y axis ranges")
    plt.command('xyline')
    plt.addkey("Sine")
    plt.annotate('off')
    # --- Define a triangle wave shape.
    x = np.array([0,5,10])
    y = np.array([0,100,0])
    # --- Plot that with auto axis, put values on right
    plt.csg('all')
    plt.colour( 0, 0, 0 )
    plt.rightannot('on')
    plt.rylabel("Right edge (triangle values)")
    plt.xyline(x, y)
    plt.addkey("Triangle")
    if full:
        plt.outline('device')
        plt.draw()
    else:
        plt.drawkeys()

def obgrf14(plt):
    """
    Read an x,y data file and plot the contents
    """
    # --- Read the data file, first 2 columns, space or comma sep.
    x,y = np.loadtxt('../obey-files/daeg1', unpack=True)
    # --- DRAW THE GRAPH
    plt.graphmode(True)
    plt.csg('annot')
    plt.colour(0,0,0)
    plt.csg('text')
    plt.colour(0,0,0)
    plt.title('Mains voltage on 7-Aug-2025')
    plt.xlabel('Hours w.r.t. midnight 6/7 August')
    plt.ylabel('Volts (RMS 220V nominal)')
    plt.xyline(x,y)
    plt.outline('device')
    plt.draw()

def obgrf15(plt, full=True):
    """
    Read an x,y data file and plot the contents + label
    """
    # --- Read the data file, first 2 columns, space or comma sep.
    x,y = np.loadtxt('../obey-files/daeg1', unpack=True)
    # --- Draw the graph
    if full:
        plt.graphmode(True)
    plt.csg('annot')
    plt.colour(0,0,0)
    plt.csg('text')
    plt.colour(0,0,0)
    plt.title('Mains voltage on 7-Aug-2025')
    plt.xlabel('Hours w.r.t. midnight 6/7 August')
    plt.ylabel('Volts (RMS 220V nominal)')
    plt.xyline(x,y)
    # --- Add a label or two
    plt.csg('all')
    plt.colour(0,0,0)
    plt.glabel(9.8, 192.0, 0.1, 165, "What happened?")
    plt.colour(0.7, 0.384, 0.025)
    plt.glabel(14, 191.5, 0.05, 165, "Eek")
    plt.outline('device')
    if full:
        plt.draw()
    else:
        plt.drawkeys()

def obgrf16(plt):
    """
    Read an x,y,e data file and plot the contents
    """
    # --- Read the data file, first 3 columns, this one uses commas.
    # Note: This is actually easier with GPLOT itself!
    x,y,ye = np.loadtxt('../obey-files/daeg2', unpack=True, delimiter=',')
    # --- Draw the graph
    plt.graphmode(True)
    plt.csg('annot')
    plt.colour(0,0,0)
    plt.csg('text')
    plt.colour(0,0,0)
    plt.csg('general')
    plt.colour(0.8,0.1,0.1)    
    plt.title('L*LOOKS LIKE X*+2$+ WITH SYMMETRICAL ERRORS')
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.xyplot_yerrors(x,y,ye,'xyline')
    plt.outline('device')
    plt.draw()

def obgrf17(plt):
    """
    Read an x,y,eu,el data file and plot the contents
    """
    # --- Read the data file, first 4 columns, this one uses commas.
    # Note: This is actually easier with GPLOT itself! The file has 5 columns, BTW.
    x,y,ye1,ye2,fjunk = np.loadtxt('../obey-files/daeg3', unpack=True, delimiter=',')
    # --- Draw the graph
    plt.graphmode(True)
    plt.csg('annot')
    plt.colour(0,0,0)
    plt.csg('text')
    plt.colour(0,0,0)
    plt.csg('general')
    plt.colour(0.8,0.1,0.1)    
    plt.title('L*LOOKS LIKE X*+2$+ WITH SYMMETRICAL ERRORS')
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.xyplot_yerrors_asym(x,y,ye1,ye2,'xyline')
    plt.outline('device')
    plt.draw()      

def obgrf18(plt):
    """
    Read an x,y,eu,el,ypt data file and plot the contents with points.
    """
    # --- Read the data file, first 5 columns, this one uses commas.
    x,y,ye1,ye2,ypt = np.loadtxt('../obey-files/daeg3', unpack=True, delimiter=',')
    # --- Draw the graph
    plt.graphmode(True)
    plt.csg('annot')
    plt.colour(0,0,0)
    plt.csg('text')
    plt.colour(0,0,0)
    plt.csg('general')
    plt.colour(0.8,0.1,0.1)    
    plt.title('L*LOOKS LIKE X*+2$+ WITH SYMMETRICAL ERRORS')
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.xyplot_yerrors_asym(x,y,ye1,ye2,'xyline')
    # --- Plot points
    plt.xysame()
    plt.annotate(False)
    plt.csg('general')
    plt.colour(0,0,1)
    plt.symht(0.04)
    plt.xypoint(x,ypt)
    plt.outline('device')
    plt.draw()

def obgrf19(plt, better=False):
    """
    Read a counts only data file and make histograms.
    """
    if better:
        plt.graphmode(True)
        shrink = 0.98
    else:
        shrink = 1.0
    # Read the data file, first column only.
    y = np.loadtxt('../obey-files/daeg4', unpack=True)
    x = np.asarray(range(len(y)))
    # Set colours.
    plt.csg('annot')
    plt.colour(0,0,0)
    plt.csg('text')
    plt.colour(0,0,0)
    plt.csg('general')
    plt.colour(0.8,0.1,0.1)
    # Set the axis ranges and use int labels.
    plt.xrange( -1, 20 )
    plt.yrange( 0, 240 )
    plt.drawaxes(False)
    plt.intvalues('both')
    # Set title and axis labels.
    plt.title( "Counts histogram" )
    plt.xlabel( "Class number" )
    plt.ylabel( "Count" )
    # ABUT style in bottom left.
    plt.subfig(2,2,1,1,shrink)
    plt.histstyle('abut')
    plt.xyhistogram(x,y)
    plt.outline('pane')
    # SHADED ABUT style in bottom right.
    plt.subfig(2,2,2,1,shrink)
    plt.histstyle('abut+shade')
    plt.xyhistogram(x,y)
    plt.outline('pane')
    #  Specified width bars in top left.
    plt.subfig(2,2,1,2,shrink)
    plt.histstyle('wide', width=0.25)
    plt.xyhistogram(x,y)
    plt.outline('pane')
    #  Specified width bars in top left.
    plt.subfig(2,2,2,2,shrink)
    plt.histstyle('lines')
    plt.xyhistogram(x,y)
    plt.outline('pane')
    #
    plt.outline('device')
    plt.draw()

def obgrf20(plt):
    """
    Log y axis
    """
    plt.graphmode(True)
    # Set colours
    plt.csg('annot')
    plt.colour(0,0,1)
    plt.csg('text')
    plt.colour(0,0,1)
    # Squares of some numbers.
    x = np.array([1,2,3,4,5,6,7,8,9,10])
    y = np.array([1,4,9,16,25,36,49,64,81,100])
    # We will use keys.
    plt.usekey()
    # Plot it with log y axis
    plt.xlabel('x')
    plt.ylabel('log(y)')
    plt.title('*LY=X*+2$+ WITH LOG Y AXIS')
    plt.grid('both')
    plt.ylog()
    plt.xyline(x,y)
    plt.addkey('log')
    plt.annotate(False)
    # Plot it with linear y axis, values on right
    plt.csg('all')
    plt.colour(0,0,0)
    plt.rightannot(True)
    plt.ylinear()
    plt.xyline(x,y)
    plt.addkey('linear')
    #
    plt.outline('device')
    plt.draw()

def obgrf21(plt, full=True):
    """
    Log y axis
    """
    if full:
        plt.graphmode(True)
    # Set colours
    plt.csg('annot')
    plt.colour(0,0,1)
    plt.csg('text')
    plt.colour(0,0,1)
    # Squares of some numbers.
    x = np.array([1.0E7,2.0E7,3.0E7,4.0E7,5.0E7,6.0E7,7.0E7,8.0E7,9.0E7,1.0E8,2.0E8,3.0E8,
                  4.0E8,5.0E8,6.0E8,7.0E8,8.0E8,9.0E8,1.0E9])
    y = np.array([1,4,9,16,25,36,49,64,81,100,400,900,1600,2500,3600,4900,6400,8100,10000])
    # We will use keys.
    plt.usekey()
    # Plot it with log x and y axes
    plt.xlabel('log(x)')
    plt.ylabel('log(y)')
    plt.title('*LY=(*,X$,10*+7$+$.)*O*+2$+$* WITH LOG X AND Y AXES')
    plt.grid('both')
    plt.xlog()
    plt.ylog()
    plt.xyline(x,y)
    plt.addkey('log')
    plt.annotate(False)
    # Plot it with linear y axis, values on right
    plt.csg('all')
    plt.colour(0,0,0)
    plt.rightannot(True)
    plt.rylabel("Y (Linear)")
    plt.ylinear()
    plt.xyline(x,y)
    plt.addkey('linear')
    #
    plt.outline('device')
    if full:
        plt.draw()
    else:
        plt.drawkeys()

def obgf28m(plt):
    plt.graphmode(True)
    plt.subfig(2,2,1,1,0.98)
    obgrf13(plt, full=False)
    #
    plt.reset()
    plt.graphmode(True)
    plt.subfig(2,2,2,2,0.98)
    obgrf11(plt, full=False)
    #
    plt.reset()
    plt.graphmode(True)
    plt.subfig(2,2,1,2,0.98)
    obgrf21(plt, full=False)
    #
    plt.reset()
    plt.graphmode(True)
    plt.subfig(2,2,2,1,0.98)
    obgrf15(plt, full=False)
    plt.outline('pane')
    #
    plt.outline('device')
    plt.draw()

if __name__ == '__main__':

    plt = gplot.gplot('svg', size=(2048,1280)) #size=(1280,800))

    obgraf1(plt)
    obgrf1a(plt)
    obgraf2(plt)
    obgraf3(plt)
    obgraf4(plt)
    obgraf5(plt)
    obgraf7(plt)
    obgraf8(plt)
    obgrf10(plt)
    obgrf11(plt)
    obgrf12(plt)
    obgrf13(plt)
    obgrf14(plt)
    obgrf15(plt)
    obgrf16(plt)
    obgrf17(plt)
    obgrf18(plt)
    obgrf19(plt)
    obgrf19(plt, True)
    obgrf20(plt)
    obgrf21(plt)
    obgf28m(plt)
