#!/usr/bin/sh

install cantara /usr/bin/cantara
install app.cantara.Cantara.png /usr/share/icons/app.cantara.Cantara.png

xdg-desktop-menu install app.cantara.Cantara.desktop

install src/languages/de/cantara.mo /usr/share/locale/de/LC_MESSAGES/cantara.mo
install src/languages/zh/cantara.mo /usr/share/locale/zh/LC_MESSAGES/cantara.mo
install src/languages/it/cantara.mo /usr/share/locale/it/LC_MESSAGES/cantara.mo
