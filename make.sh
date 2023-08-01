#
# Runs Lazbuild to build the project for release purpose
#

cd src
lazbuild -B --bm="Release" --ws="qt5" Cantara.lpi
cd ..
mv src/cantara cantara
