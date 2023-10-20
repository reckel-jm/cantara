# Contributing to Cantara

Thank you that you want to contribute to Cantara. Every help is very appreciated. This file provides basic information on how you can contribute.

## Reporting issues

Feel free to create an issue for general feedback, bug reports and feature requests.

## Translate Cantara to an other language

Please fork the project form the current master branch, take the file (src/locals/cantara.pot) as template and use a program like Poedit to create a po file. This file is saved in the same folder (src/locals) and has the syntax ```cantara.LC.po``` where LC is the international **language code** of the language which you have translated to (e.g. de for German, zh for Chineses, etc.). After that, you can submit a pull request. *You do not nead to compile your po file to a mo file*.

## Contribute to the source code

You are very welcome to contribute to the source code. There are not so many rules for contributing, but over the years, some development guidelines have been found as useful:

- **Seperation of the business logic from the UI**: The long time goal is implementing a Modell-View-Controller structure of Cantara. Currently Cantara separates the business logic (parsing of song files, creating and handling slides, export) from the user interface. Most of the business logic is in the (src/generics) folder. The user interface is located in either (src/frames) or (src/forms).

- **Follow basic Pascal coding guidelines**: Data Types start with a T, Interfaces with an I. Variables should follow the Pascal-style notation which always begins with a capitalised letter, e.g. ```TestVar1``` instead of ```testVar1```.

- **Test based development**: Use the project (src/fpccantaraclitest.lpi) for the development of the generic classes. Please write and run your own tests before implementing the generics into Cantara. For the user interface, we don't use tests at the moment.

- **Use encapsulated frames for UI**: For the Lazarus user interface part, we use encapsulated frames as much as possible. That means that a frame should not access its parent for the sake of interoperability.

Even if you are not that experienced in Pascal development and you are not sure whether you adhire to all this points, feel free to contribute. Any contribution is appreciated!
