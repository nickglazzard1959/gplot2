import numpy as np
import subprocess
import os

class gplot(object):
    """
    Provide a Python callable interface to GPLOT.
    This can be used to plot graphs (and potentially draw other things) from Python code.
    """

    def __init__(self, device='svg', size=None, offset=None, throwup=True):
        self.valid_devices = ['gterm','tek4k','epscol','svg']
        self.commands = []
        self.outfile = 'zzzzz'
        self.frameno = 1
        self.device = ''
        self.ok = False
        self.parm_string = ''
        self.keys = False
        self.nkeys = 0
        self.throwup = throwup

        if device in self.valid_devices:
            if (device == 'epscol') or (device == 'svg'):
                if (size is not None) and (len(size) == 2):
                    self.parm_string = f' {size[0]:d},{size[1]:d}'
                    if (device == 'epscol'):
                        if (offset is not None) and (len(offset) == 2):
                            self.parm_string += '+{offset[0]:d}+{offset[1]:d}'
                self.commands.append(f'device {device} {self.outfile} {self.parm_string}'+'\n')
                self.commands.append('reset\n')
            self.device = device
            self.ok = True

        else:
            if self.throwup:
                raise ValueError('Unknown device requested.')
                

    def _get_image_file_name(self):
        """
        Internal: Get the current output image file.
        """
        if self.device == 'epscol':
            extension = 'eps'
        elif self.device == 'svg':
            extension = 'svg'
        else:
            return 'none'
        return '{0:s}{1:03d}.{2:s}'.format(self.outfile,self.frameno,extension)

    def _yesno_bool(self, yn):
        """
        Allow either boolean or strings for "yes/no" arguments to functions.
        Permit y[es], n[o], on, off or True, False
        """
        if type(yn) is str:
            ynl = yn.lower()
            if (ynl == 'y') or (ynl == 'on'):
                return 'yes'
            elif (ynl == 'n') or (ynl == 'off'):
                return 'no'
            else:
                print('Yes/no argument not bool and garbled. Returning no.')
                return 'no'
        elif type(yn) is bool:
            return 'yes' if yn else 'no'
        else:
            print('Yes/no argument must be string or bool. Returning no.')
            return 'no'

    def _check_init(self):
        """
        If initialisation failed and throeong exceptions, do that.
        """
        if self.throwup:
            raise RuntimeError('gplot library initialisation failed.')
        
    def csg(self, which):
        """
        Choose the colour/style group to which the following colour(), width() apply.
        """
        if not self.ok:
            self._check_init()
            return False

        if which not in ['all', 'general', 'text', 'annot']:
            if self.throwup:
                raise ValueError('Invalid colour/style group.')
            else:
                print('gplot.csg(): Unknown colour/style group:', which)
                return False

        self.commands.append(f'csgroup {which}\n')
        return True

    def width(self, width):
        """
        Set line drawing width.
        """
        if not self.ok:
            self._check_init()
            return False

        width = max(0.01, min(100.0,width))
        
        self.commands.append(f'width {width:.3f}\n')
        return True

    def symht(self, height):
        """
        Set symbol drawing height.
        """
        if not self.ok:
            self._check_init()
            return False

        height = max(0.01, min(100.0,height))
        
        self.commands.append(f'symht {height:.3f}\n')
        return True
    
    def style(self, which, length=None):
        """
        Choose the line drawing style.
        """
        if not self.ok:
            self._check_init()
            return False

        if which not in ['solid', 'dash', 'dot', 'dashdot']:
            if self.throwup:
                raise ValueError('Invalid line drawing style.')
            else:
                print('gplot.style(): Unknown line style:', which)

        if length is not None:
            length = max(0.001, length)
            self.commands.append(f'style {which} {length:g}\n')
        else:
            self.commands.append(f'style {which}\n')
        return True    
    
    def colour(self, r, g, b):
        """
        Set the drawing colour for the current colour/style group.
        """
        if not self.ok:
            self._check_init()
            return False
        
        r = max(0.0, min(1.0, r))
        g = max(0.0, min(1.0, g))
        b = max(0.0, min(1.0, b))
        self.commands.append(f'colour {r:.3f} {g:.3f} {b:.3f}\n')
        return True

    def xyplot(self, x, y, cmd ):
        """
        Draw xyline, xypoint or xyhistogram.
        """
        if not self.ok:
            self._check_init()
            return False

        if cmd not in ['xyline', 'xypoint', 'xyhistogram']:
            if self.throwup:
                raise ValueError('Invalid plot type.')
            else:
                print('gplot.xyplot(): Unknown xy plot type:', cmd)
                return False

        n = len(x)
        if len(y) != n:
            if self.throwup:
                raise ValueError('x and y arrays have different lengths.')
            else:
                print('gplot.xyplot(): x and y arrays have different lengths.')
                return False

        self.commands.append('read HERE 1 2\n')
        for i in range(n):
            xi = x[i]
            yi = y[i]
            self.commands.append(f'{xi:g} {yi:g}\n')
        self.commands.append('EOF\n')

        self.commands.append(f'{cmd}\n')
        return True

    def xyplot_yerrors(self, x, y, e, cmd ):
        """
        Draw xyline, xypoint with symmetric Y error bars
        """
        if not self.ok:
            self._check_init()
            return False

        if cmd not in ['xyline', 'xypoint']:
            if self.throwup:
                raise ValueError('Invalid plot type.')
            else:
                print('gplot.xyplot(): Unknown xy plot type:', cmd)
                return False

        n = len(x)
        if len(y) != n:
            if self.throwup:
                raise ValueError('x and y arrays have different lengths.')
            else:
                print('gplot.xyplot_yerrors(): x and y arrays have different lengths.')
                return False

        self.commands.append('read HERE 1 2 3\n')
        for i in range(n):
            xi = x[i]
            yi = y[i]
            ei = e[i]
            self.commands.append(f'{xi:g} {yi:g} {ei:g}\n')
        self.commands.append('EOF\n')

        self.commands.append(f'{cmd}\n')
        return True

    def xyplot_yerrors_asym(self, x, y, e1, e2, cmd ):
        """
        Draw xyline, xypoint with asymmetric Y error bars
        """
        if not self.ok:
            self._check_init()
            return False

        if cmd not in ['xyline', 'xypoint']:
            if self.throwup:
                raise ValueError('Invalid plot type.')
            else:            
                print('gplot.xyplot_yerrors_asym(): Unknown xy plot type:', cmd)
                return False

        n = len(x)
        if len(y) != n:
            if self.throwup:
                raise ValueError('x and y arrays have different lengths.')
            else:
                print('gplot.xyplot_yerrors_asym(): x and y arrays have different lengths.')            
                return False

        self.commands.append('read HERE 1 2 3 4\n')
        for i in range(n):
            xi = x[i]
            yi = y[i]
            e1i = e1[i]
            e2i = e2[i]
            self.commands.append(f'{xi:g} {yi:g} {e1i:g} {e2i:g}\n')
        self.commands.append('EOF\n')

        self.commands.append('asymyerrors on\n')
        self.commands.append(f'{cmd}\n')
        return True

    def xyplot_xyerrors(self, x, y, ex, ey, cmd ):
        """
        Draw xyline, xypoint with symmetric X and Y error bars
        """
        if not self.ok:
            self._check_init()
            return False

        if cmd not in ['xyline', 'xypoint']:
            if self.throwup:
                raise ValueError('Invalid plot type.')
            else:            
                print('gplot.xyplot_xyerrors(): Unknown xy plot type:', cmd)
                return False

        n = len(x)
        if len(y) != n:
            if self.throwup:
                raise ValueError('x and y arrays have different lengths.')
            else:
                print('gplot.xyplot_yerrors(): x and y arrays have different lengths.')            
                return False

        self.commands.append('read HERE 1 2 3 4\n')
        for i in range(n):
            xi = x[i]
            yi = y[i]
            exi = ex[i]
            eyi = ey[i]
            self.commands.append(f'{xi:g} {yi:g} {eyi:g} {exi:g}\n')
        self.commands.append('EOF\n')

        self.commands.append('asymyerrors off\n')
        self.commands.append(f'{cmd}\n')
        return True    

    def xyline(self, x, y):
        """
        Draw a line plot taking points from x and y arrays or lists.
        """
        if not self.ok:
            self._check_init()
            return False

        return self.xyplot(x, y, 'xyline')

    def xypoint(self, x, y):
        """
        Draw a point plot taking points from x and y arrays or lists.
        """
        if not self.ok:
            self._check_init()
            return False

        return self.xyplot(x, y, 'xypoint')

    def xyhistogram(self, x, y):
        """
        Draw a histogram plot taking points from x and y arrays or lists.
        """
        if not self.ok:
            self._check_init()
            return False

        return self.xyplot(x, y, 'xyhistogram')

    def grmove(self, x, y):
        """
        Move to graph coordinates (x,y)
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(f'grmove {x:g} {y:g}\n')
        return True

    def grdraw(self, x, y):
        """
        Draw to graph coordinates (x,y)
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(f'grdraw {x:g} {y:g}\n')
        return True

    def xyauto(self):
        """
        Auto range both axes.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append('xyauto\n')
        return True
    
    def xrange(self, xlo, xhi):
        """
        Set X axis range.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(f'xrange {xlo:g} {xhi:g}\n')
        return True
    
    def yrange(self, ylo, yhi):
        """
        Set Y axis range.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(f'yrange {ylo:g} {yhi:g}\n')
        return True

    def xysame(self):
        """
        Keep last (auto determined) ranges on both axes.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append('xysame\n')
        return True

    def xlinear(self):
        """
        Set X axis to linear.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append('xlinear\n')
        return True    

    def ylinear(self):
        """
        Set Y axis to linear.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append('ylinear\n')
        return True    

    def xlog(self):
        """
        Set X axis to log.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append('xlog\n')
        return True    

    def ylog(self):
        """
        Set Y axis to log.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append('ylog\n')
        return True

    def intvalues(self, axisname):
        """
        Try to use integer values on one, both or no axes.
        axisname: none, x, y or both
        """
        if not self.ok:
            self._check_init()
            return False
        
        if axisname not in ['none', 'x', 'y', 'both']:
            if self.throwup:
                raise ValueError('Unknown axis name.')
            else:            
                print('gplot.intvalues(): Unknown axis name:', axisname)
                return False

        self.commands.append(f'intvalues {axisname}\n')
        return True        
    
    def grid(self, axisname):
        """
        Draw grid lines along one, both or no axes.
        axisname: none, x, y or both
        """
        if not self.ok:
            self._check_init()
            return False
        
        if axisname not in ['none', 'x', 'y', 'both']:
            if self.throwup:
                raise ValueError('Unknown axis name.')
            else:             
                print('gplot.grid(): Unknown axis name:', axisname)
                return False

        self.commands.append(f'grid {axisname}\n')
        return True

    def outline(self, objname):
        """
        Outline some feature.
        objname: pane, blank, bounds, device
        """
        if not self.ok:
            self._check_init()
            return False
        
        if objname not in ['pane', 'blank', 'bounds', 'device']:
            if self.throwup:
                raise ValueError('Unknown object to outline.')
            else:             
                print('gplot.outline(): Unknown object to outline:', objname)
                return False

        self.commands.append(f'outline {objname}\n')
        return True    

    def interpolate(self, interpname):
        """
        Set graph plotting interpolation type.
        interpname: linear, cubic, quintic
        """
        if not self.ok:
            self._check_init()
            return False
        
        if interpname not in ['linear', 'cubic', 'quintic']:
            if self.throwup:
                raise ValueError('Unknown interpolation type.')
            else:            
                print('gplot.interpolate(): Unknown interpolation type:', interpname)
                return False

        self.commands.append(f'interpolate {interpname}\n')
        return True

    def histstyle(self, histmode, width=None):
        """
        Set histogram drawing style.
        histmode: abut, abut+shade, lines, wide, wide+shade
        """
        if not self.ok:
            self._check_init()
            return False
        
        if histmode not in ['abut', 'abut+shade', 'lines', 'wide', 'wide+shade']:
            if self.throwup:
                raise ValueError('Unknown histogram style.')
            else:            
                print('gplot.histstyle(): Unknown histogram style:', histmode)
                return False

        if width is None:
            self.commands.append(f'histstyle {histmode}\n')
        else:
            self.commands.append(f'histstyle {histmode} {width:g}\n')
        return True

    def annotate(self, uonoff):
        """
        Turn graph annotation on or off.
        """
        if not self.ok:
            self._check_init()
            return False
        
        onoff = self._yesno_bool(uonoff)

        self.commands.append(f'annotate {onoff}\n')
        return True
    
    def rightannot(self, uonoff):
        """
        Turn right edge graph annotation on or off.
        """
        if not self.ok:
            self._check_init()
            return False
        
        onoff = self._yesno_bool(uonoff)

        self.commands.append(f'rightannot {onoff}\n')
        return True

    def title(self, text, lower=False):
        """
        Set the graph title.
        """
        if not self.ok:
            self._check_init()
            return False

        if lower:
            self.commands.append(f'title "{text}" yes\n')
        else:
            self.commands.append(f'title "{text}"\n')
        return True

    def xlabel(self, text):
        """
        Set the X axis label.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(f'xlabel "{text}"\n')
        return True
    
    def ylabel(self, text):
        """
        Set the Y axis label.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(f'ylabel "{text}"\n')
        return True
    
    def rylabel(self, text):
        """
        Set the right side Y axis label.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(f'rylabel "{text}"\n')
        return True

    def gstyle(self, stylename):
        """
        Set graph plotting style.
        stylename: boxed, axes, open
        """
        if not self.ok:
            self._check_init()
            return False
        
        if stylename not in ['boxed', 'axes', 'open']:
            if self.throwup:
                raise ValueError('Unknown graph style.')
            else:            
                print('gplot.gstyle(): Unknown graph style:', stylename)
                return False

        self.commands.append(f'gstyle {stylename}\n')
        return True

    def axcut(self, xo, yo):
        """
        Set the point through which drawn axes should pass.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(f'axcut {xo:g} {yo:g}\n')
        return True

    def drawaxes(self, uonoff):
        """
        Draw axes, or not.
        """
        if not self.ok:
            self._check_init()
            return False
        
        onoff = self._yesno_bool(uonoff)

        self.commands.append(f'axcut {onoff}\n')
        return True
    
    def marker(self, which):
        """
        Set the marker to be used to plot points.
        """
        if not self.ok:
            self._check_init()
            return False

        if type(which) is str:
            marker_dict = {'.':1, 'x':3, '+':4, 'o':5,
                           '#':6, '^':7, '*':9, 'O':22}
            if which not in marker_dict.keys():
                if self.throwup:
                    raise ValueError('Marker name not recognised.')
                else:                
                    print('gplot.marker(): marker name is not recognised:',which)
                    return False
            which = marker_dict[which]

        which = min(25,max(1,int(which)))

        self.commands.append(f'marker {which}\n')
        return True

    def usekey(self):
        """
        Prepare to create a key for the graph.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(f'usekey\n')
        self.keys = True
        self.nkeys = 0
        return True   

    def addkey(self, text):
        """
        Add a key for the last specified xyline or xypoint.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(f'addkey "{text}"\n')
        self.nkeys += 1
        return True

    def graphmode(self, uonoff):
        """
        Turn graphmode on or off.
        """
        if not self.ok:
            self._check_init()
            return False
        
        onoff = self._yesno_bool(uonoff)

        self.commands.append(f'graphmode {onoff}\n')
        return True

    def subfig(self, nx, ny, ix, iy, shrink=None):
        """
        Draw the next graph as a sub-figure, (ix,iy) in a grid of nx x ny.
        """
        if not self.ok:
            self._check_init()
            return False

        if shrink is None:
            self.commands.append(f'subfiggrid {nx} {ny} {ix} {iy}\n')
        else:
            self.commands.append(f'subfiggrid {nx} {ny} {ix} {iy} {shrink:g}\n')
        return True

    def glabel(self, gx, gy, length, angle, text):
        """
        Draw a label with an arrow and boxed text.
        (gx,gy) are graph coordinates to which the arrow points.
        length is the length of the arrow shaft in bounds units.
        angle is the angle of the arrow shaft in degrees wrt +X.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(f'glabel {gx:g} {gy:g} {length:g} {angle:g} "{text}"\n')
        return True

    def command(self, cmd ):
        """
        Add an arbitrary GPLOT command. Useful for tests.
        """
        if not self.ok:
            self._check_init()
            return False

        self.commands.append(cmd+'\n')
        return True

    def drawkeys(self):
        """
        Draw the keys legend panel.
        """
        if not self.ok:
            self._check_init()
            return False
        
        if self.keys and (self.nkeys > 0):
            self.commands.append(f'keys\n')
        return True

    def reset(self):
        """
        Reset state.
        """
        if not self.ok:
            self._check_init()
            return False
        
        self.commands.append('reset\n')
        return True
        
    def draw(self, outfile=None):
        """
        Draw the accumulated graph description. Clear the commands.
        Prepare for the next frame.
        """
        if not self.ok:
            self._check_init()
            return False

        # Finish the obey file. If keys have been used, draw them.
        self.drawkeys()
        self.commands.append('exit\n')

        # Write the obey file.
        if os.path.exists('obzzzzz'):
            os.remove('obzzzzz')
            
        try:
            with open('obzzzzz','w') as fout:
                fout.writelines(self.commands)

        except Exception as e:
            print('gplot.draw(): Error writing commands. Reason: ', e)
            if self.throwup:
                raise RuntimeError('Error writing commands.')
            else:            
                return False

        # Get the (temporary) image file name.
        image_file_name = self._get_image_file_name()
        if os.path.exists(image_file_name):
            os.remove(image_file_name)

        # Run GPLOT.
        cmd = ['gplot', 'obey=obzzzzz']
        try:
            retstring = subprocess.check_output(cmd, universal_newlines=True)
        except Exception as e:
            print('gplot.draw(): Failed to run gplot.')
            print('... Reason:', e)
            if self.throwup:
                raise RuntimeError('Failed to run gplot.')
            else:              
                return False

        # Read the output image file, if any.
        if self.device in ['epscol','svg']:
            if not os.path.exists(image_file_name):
                if self.throwup:
                    raise RuntimeError('Image file from gplot not found.')
                else:                    
                    print('... GPLOT failed. Expected image file:',image_file_name,'not found,')
                    return False

            # print('... generated plot file:', image_file_name)

            # If outfile was supplied, save the result under the specified name.
            if outfile is not None:
                try:
                    os.rename(image_file_name, outfile)
                    return True
                except Exception as e:
                    if self.throwup:
                        raise RuntimeError('Failed to save output file.')
                    else:                     
                        print('gplot.draw(): Failed to save output file as',outfile)
                        print('... Reason:', e)
                        return False

            # Otherwise, display the output image.
            if self.device == 'svg':
                display_pgm = 'svgview'
            else:
                display_pgm = 'epsview.sh'
            cmd = [display_pgm, image_file_name]

            try:
                retstring = subprocess.check_output(cmd, universal_newlines=True)
                print(retstring)
            except Exception as e:
                if self.throwup:
                    raise RuntimeError('Failed to run display program.')
                else: 
                    print('gplot.draw(): Failed to run', display_pgm)
                    print('... Reason:', e)
                    return False

        # Prepare for the next frame.
        self.commands = []
        self.frameno = 1
        self.keys = False
        self.nkeys = 0
        self.commands.append(f'device {self.device} {self.outfile} {self.parm_string}'+'\n')
        self.commands.append('reset\n')
        return True
