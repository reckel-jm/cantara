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

fpc -MObjFPC -Scghi -Cg -Os3 -l -vewnhibq -Filib/x86_64-linux -Fu/usr/lib/lazarus/components/rtticontrols/lib/x86_64-linux/gtk2 -Fu/usr/lib/lazarus/components/ideintf/units/x86_64-linux/gtk2 -Fu/usr/lib/lazarus/components/synedit/units/x86_64-linux/gtk2 -Fu/usr/lib/lazarus/components/lazcontrols/lib/x86_64-linux/gtk2 -Fu/usr/lib/lazarus/lcl/units/x86_64-linux/gtk2 -Fu/usr/lib/lazarus/lcl/units/x86_64-linux -Fu/usr/lib/lazarus/components/lazutils/lib/x86_64-linux -Fu/usr/lib/lazarus/packager/units/x86_64-linux -Fu. -FUlib/x86_64-linux -FE. -o./cantara -dLCL -dLCLgtk2 Cantara.lpr