#!/usr/bin/sh

install cantara /usr/bin/cantara
install app.cantara.Cantara.png /usr/share/icons/app.cantara.Cantara.png

xdg-desktop-menu install app.cantara.Cantara.desktop

install languages/de/cantara.mo /usr/share/locale/de/LC_MESSAGES/cantara.mo
install languages/zh/cantara.mo /usr/share/locale/zh/LC_MESSAGES/cantara.mo
