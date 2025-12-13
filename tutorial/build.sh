# Make the tutorial document on a Unix-like system.
# Copy SVG files needed by the main README.md to the top directory.
#
# Requires this tool has been installed:
#   npm install -g ghmd
#
cd ../obey-files
rm -f *.svg
gplot obey=obalsvg
cp *.svg ../tutorial
rm -f *.svg
cd ../tutorial
ghmd tutorial.md --embed-css
cp cht1001.svg	..
cp cht2001.svg	..
cp g1fm001.svg	..
cp gr27001.svg	..
cp iocd001.svg  ..
echo "Done."
