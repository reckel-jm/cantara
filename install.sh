#!/usr/bin/sh

install cantara /usr/bin/cantara
install Cantara.png /usr/share/icons/cantara.png

xdg-desktop-menu install reckel-cantara.desktop

install languages/de/cantara.mo /usr/share/locale/de/LC_MESSAGES/cantara.mo
install languages/zh/cantara.mo /usr/share/locale/zh/LC_MESSAGES/cantara.mo
