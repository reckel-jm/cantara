#
# Runs Lazbuild to build the project for release purpose
#

cd src
lazbuild -B --ws="qt5" bgrabitmap/bgrabitmap/bgrabitmappack.lpk
lazbuild -B --bm="Release" --ws="qt6" Cantara.lpi
cd ..
mv src/cantara cantara
