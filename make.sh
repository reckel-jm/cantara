#
# Runs Lazbuild to build the project for release purpose
#

lazbuild -B --bm="Release" --ws="qt5" src/Cantara.lpi
mv src/cantara cantara
