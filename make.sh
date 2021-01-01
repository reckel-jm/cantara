#
# Detects and parses the architecture
#

ARCH=$(uname -m)

case "$ARCH" in

 "i686") ARCH="i386";;

 "i586") ARCH="i386";;

 "i486") ARCH="i386";;

esac

echo "Target architecture: $ARCH"

#
# Detects and parses the OS
#

OS="linux"

echo "Target operating system: $OS"


#
# Command line to build the sofware
#

fpc -MObjFPC -Scghi -Cg -Os3 -l -vewnhibq -Filib/$ARCH-$OS -Fu/usr/lib/lazarus/components/rtticontrols/lib/$ARCH-$OS/gtk2 -Fu/usr/lib/lazarus/components/ideintf/units/$ARCH-$OS/gtk2 -Fu/usr/lib/lazarus/components/synedit/units/$ARCH-$OS/gtk2 -Fu/usr/lib/lazarus/components/lazcontrols/lib/$ARCH-$OS/gtk2 -Fu/usr/lib/lazarus/lcl/units/$ARCH-$OS/gtk2 -Fu/usr/lib/lazarus/lcl/units/$ARCH-$OS -Fu/usr/lib/lazarus/components/lazutils/lib/$ARCH-$OS -Fu/usr/lib/lazarus/packager/units/$ARCH-$OS -Fu. -FUlib/$ARCH-$OS -FE. -ocantara -dLCL -dLCLgtk2 Cantara.lpr

if [ $? == 0 ]; then

    echo "Kompilierung erfolgreich. Cantara kann nun mit 'sudo make install' installiert werden."

    exit 0

else

    echo "Kompilierung fehlgeschlagen. Möglicherweise ist Lazarus nicht installiert oder der vermutete Pfad ist falsch. In diesem Fall ist die Datei make.sh manuell anzupassen (Zeile 32)."

    echo 1

fi