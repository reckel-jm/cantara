# Cantara

**Hint**: This is just the repository for the source code. If you are not interested in the source code, please go to the [webpage](https://reckel-jm.github.io/cantara-song) with a complete documentation and instruction on downloading and installing.

**Pull Requests** with bug reports, suggestions or general feedback are very appreciated!

## About the program
Cantara is a simple open source song presentation software written in Free Pascal/Lazarus which allows people to spontanously present song lyrics for a bigger audience for the purpose of singing together. This is especially useful for church groups and meetings. 

## Current Version
The current version is 2.2 released at July 5, 2022. There exists also the 2.3 Beta which supports the CCLI Songselect lyrics format.

## Download and Installation

There are various ways how to download and install Cantara. For a detailed explenation, please check out the docs. [In the "Releases" section of this repository](https://github.com/reckel-jm/cantara/releases) there are several binary downloads for different operating system. You can also find Cantara in the [Snap Store](https://snapcraft.io/cantara).

If you would like to compile the latest state in the master brunch, you need to do the following:

 1. Download and install the [Lazarus IDE](https://www.lazarus-ide.org) – either via your distribution or their homepage.
 2. Clone the Github repository:

    git clone git@github.com:reckel-jm/cantara.git

 3. Open the `lazarus.lpi` file *as a project* in Lazarus and compile it **or** use `lazbuild` for the compilation via the command line:

    lazbuild -B Cantara.lpi

On Linux, you can change the used graphical framework via the `--ws=qt5` or `--ws=gtk2` option.

## Song templates
In order to make songs appear in that list, song templates have to be prepared once. A song consists of a txt-File which has the following structure:

    Stanza 1 Line 1
    Stanza 1 Line 2
    
    Stanza 2 Line 1
    Stanza 2 Line 2

Stanzas are seperated by double line wrap. Lines of stanza are seperated by a single line wrap. Repetitions (such as refrains) can be implemented by copy and paste. That's it. The user set ups a directory where all the stanzas will be stored.

At the moment an other [GitHub Repository with public domain Christian song lyrics is under construction](https://github.com/reckel-jm/cantara_songrepo), which can be directly imported to Cantara.

## Settings
The color and font of the presentation can be changed in the settings.

## Licence
The program is licenced under GPL3. See [COPYING](https://github.com/reckel-jm/cantara/blob/master/COPYING) for details. You may use and change this software and it's source code and share it as you wish, but you need to add a copyright hint and keep the licence.

