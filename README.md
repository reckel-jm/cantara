# Cantara
Ein freies Liedpräsentations-Programm // Open Source Song Presentation Software

[Lade die neuste Version für Windows herunter (EXE-Datei) // Download Latest Windows Version (with Installer)](https://github.com/reckel-jm/cantara/releases/download/release1-1/cantara-1-1.exe)

Linux-User können sich den Quelltext herunterladen und mit Lazarus selbst kompilieren.

## Deutsch

### Über dieses Programm
Cantara ist ein einfach zu bedienendes, schlichtes Liedpräsentations-Programm, welches in Lazarus (Pascal) geschrieben wurde. Mit diesem Programm lassen sich schnell und einfach Liedtexte anzeigen, sodass gemeinsames Musizieren und Singen erleichtert wird. Dies ist besonders für christliche Gruppen, Veranstaltungen und Gemeinden konzipiert, kann aber auch für andere Zwecke verwendet werden.

### Derzeitige Version
Die derzeitige Version ist 1.1 und wurde am 28.05.2020 veröffentlicht.

### Lieder einpflegen
Um die Lieder verwenden zu können, müssen sie zunächst eingepflegt werden. Ein Lied besteht dabei aus einer Textdatei mit der Struktur Liedname.song (die Dateiendung kann abweichen) und folgender Syntax:

    Strophe 1 Zeile 1
    Strophe 1 Zeile 2 (usw.)
    
    Strophe 2 Zeile 1
    Strophe 2 Zeile 2 (usw.)

Zwei Strophen werden immer durch doppelten Zeilenumbruch voneinander getrennt. Liedzeilen werden durch einen einfachen Zeilenumbruch voneinander getrennt. Die Lieder werden alle in einem Verzeichnis gespeichert, welches dann in den Einstellungen von Cantara ausgewählt wird.

Zur Zeit entsteht ein weiteres [GitHub Repository mit gemeinfreien christlichen Liedtexten](https://github.com/reckel-jm/cantara_songrepo), welche direkt in Cantara importiert werden können.

### Lizensierung

Cantara ist unter der GPL3 lizensiert. In der Datei [COPYING](https://github.com/reckel-jm/cantara/blob/master/COPYING) findet man Details dazu. Kurz zusammengefasst darf die Software und der Quelltext kostenfrei verwendet, weitergegeben und verändert werden, dabei muss allerdings der Name der Urheber erwähnt und die Lizenz beibehalten werden.

## Englisch
Unfortunaly, Cantara is not localised yet and only available in German. This will be changed soon. [An issue has been already opened](https://github.com/reckel-jm/cantara/issues/1)
### About the program
Cantara is a simple open source song presentation software written in Free Pascal/Lazarus which allows people to spontanously present song lyrics for a bigger audience for the purpose of singing together. This is especially useful for church groups and meetings. 

### Current Version
The current version is 1.1 released at May 28 2020.

### Song templates
In order to make songs appear in that list, song templates have to be prepared once. A song consists of a txt-File which has the following structure:

    Stanza 1 Line 1
    Stanza 1 Line 2
    
    Stanza 2 Line 1
    Stanza 2 Line 2

Stanzas are seperated by double line wrap. Lines of stanza are seperated by a single line wrap. Repetitions (such as refrains) can be implemented by copy and paste. That's it. The user set ups a directory where all the stanzas will be stored.

At the moment an other [GitHub Repository with public domain Christian song lyrics is under construction](https://github.com/reckel-jm/cantara_songrepo), which can be directly imported to Cantara.

### Settings
The color and font of the presentation can be changed in the settings.

### Licence
The program is licenced under GPL3. See [COPYING](https://github.com/reckel-jm/cantara/blob/master/COPYING) for details. You may use and change this software and it's source code and share it as you wish, but you need to add a copyright hint and keep the licence.

