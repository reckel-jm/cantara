#
# Runs Lazbuild to build the project for release purpose
#

lazbuild -B --bm="Release" --ws="qt5" Cantara.lpi

if [ $? == 0 ]; then

    echo "Compilation succesfully done."

    exit 0

else

    echo "Unable to build Cantara. There was an error while compiling."

    echo 1

fi
