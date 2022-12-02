#
# Runs Lazbuild to build the project for release purpose
#

lazbuild -B --bm="Release" --ws="qt5" src/Cantara.lpi

if [ $? == 0 ]; then

    mv src/cantara cantara
    echo "Move src/cantara to cantara"

    echo "Compilation succesfully done."

    exit 0

else

    echo "Unable to build Cantara. There was an error while compiling. Check the previous logs."

    echo 1

fi
