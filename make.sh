#
# Detects and parses the architecture
#

lazbuild -B Cantara.lpi --ws=qt5

if [ $? == 0 ]; then

    echo "Kompilierung erfolgreich. Cantara kann nun mit 'sudo make install' installiert werden."

    exit 0

else

    echo "Kompilierung fehlgeschlagen. Möglicherweise ist Lazarus nicht installiert oder der vermutete Pfad ist falsch. In diesem Fall ist die Datei make.sh manuell anzupassen (Zeile 32)."

    echo 1

fi
