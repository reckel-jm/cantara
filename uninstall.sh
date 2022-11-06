#
# Don´t use "rm -rf" in here, because you should only remove the files you created
#
rm -f /usr/share/icons/cantara.png

rm -f /usr/share/locale/de/LC_MESSAGES/cantara.mo
rm -f /usr/share/locale/zh/LC_MESSAGES/cantara.mo

rm -f /usr/bin/cantara

xdg-desktop-menu uninstall reckel-cantara.desktop
