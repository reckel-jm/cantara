#!/usr/bin/sh

install cantara /usr/bin/cantara
install app.cantara.Cantara.png /usr/share/icons/app.cantara.Cantara.png

xdg-desktop-menu install app.cantara.Cantara.desktop

for lang in de zh it es nl
do
    install src/languages/$lang/cantara.mo /usr/share/locale/$lang/LC_MESSAGES/cantara.mo
done