#!/usr/bin/env python
"""
svgview.py - Very minimal SVG file viewer. Needs pysdl2 and pysdl2-dll
"""

import warnings
warnings.filterwarnings("ignore", message="Using SDL2 binaries from pysdl2-dll.*", category=UserWarning)

import sdl2
import sdl2.ext
import sdl2.sdlimage

import sys
import argparse

def main():
    """
    Mainline. Does everything currently.
    """

    # Get and parse command line arguments.
    parser = argparse.ArgumentParser()
    parser.add_argument("filename", help="Name of SVG file to display.")
    parser.add_argument("-w", "--width", help="Width of window and SVG render width. Default: 1280")
    args = parser.parse_args()

    if args.width == None:
        win_w = 1280
    else:
        try:
            win_w = int(args.width)
        except Exception as e:
            print('Error parsing width, reason:', e)
            sys.exit(1)

    # Initialize SDL
    sdl2.ext.init()

    # Load the SVG image
    try:
        svg_surface = sdl2.ext.image.load_svg(args.filename, width=win_w)
    except sdl2.ext.SDLError as e:
        print('Error loading SVG file:', args.filename)
        print(' ... Reason:', e)
        sdl2.ext.quit()
        sys.exit(2)

    # Create a window
    window = sdl2.ext.Window("SVG View", size=(svg_surface.w,svg_surface.h))
    window.show()

    # Create a renderer
    renderer = sdl2.ext.Renderer(window)

    # Create a texture from the surface
    sdl2.ext.renderer.set_texture_scale_quality('best')
    svg_texture = sdl2.ext.renderer.Texture(renderer, svg_surface)

    # Clear the renderer to white. GPLOT/DIMFILM currently assumes a white background.
    # It does not put a white rect as the background.
    renderer.color = sdl2.ext.Color(255, 255, 255)
    renderer.clear()

    # Copy the texture to the renderer (display it)
    renderer.copy(svg_texture, dstrect=(0,0,svg_surface.w,svg_surface.h))

    # Present the renderer
    renderer.present()

    # Event loop to keep the window open
    running = True
    while running:
        for event in sdl2.ext.get_events():
            if event.type == sdl2.SDL_QUIT:
                running = False
            elif sdl2.ext.input.key_pressed(event, 'q'):
                running = False

    # Clean up
    sdl2.ext.quit()
    sys.exit(0)

if __name__ == '__main__':
    main()
    
