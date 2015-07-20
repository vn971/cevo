This "README" is targeted to developers
----

This is a short developer README.

If you're not a developer,
you would probably want to check the [experimental](../../tree/experimental) branch
of this repository.

This branch's goal is to port C-Evo
to the free development environment called "Lazarus".

Why Lazarus?
----

Why do it, and why Lazarus?

* because we want a free IDE to encourage developers for joining in
* because this is a powerful IDE (lazarus it definitely more powerful in 2015 than the old Delphi 7 distribution).
* We will be able to launch the game on Linux and MacOS natively.
* We will even be able lauch the game in a browser! This is possible because lazarus can compile to gtk, and gtk has the "broadway" engine. (It eats a lot of traffic, but works in most cases.)


Current status:
----

* It's possible to start the game in both Windows and Linux (the environments that I could test).
* All AIs seem to work correctly. (They expand, attack etc.)
* terrain generation differs in Windows and Linux, making games unreadable across each other. Both terrain versions seem correct visually, but the picture's different. This can probably be fixed, but still needs to be done.
* the main screen graphics are broken. This has been gradually fixed already, but still needs some work.
* the game throws exceptions in Linux at every logical step made (like end of turn, investigate a city,..)
* the game is incredibly slow under Windows but fast under Linux. This is because of `BitBlt` usages, it should be easy to fix but needs to be done.
* the in-game manual cannot be browsed in Linux because it lacks proper "PVSB" custom component support (see the code).
* the more obvious errors got fixed, the more hard ones left.. The porting progress becomes harder.
* the game proved to be hard to port. A _lot_ of winapi was used originally. Even assebly! Even now the project has a lot of platform-specific code.
* compilation TODO-s are left, they look like "`lazarus todo`" or "`{$ifdef WINDOWS}`".


What can I do?
----

* Find and try to fix compilation-level issues.

This might require using google/duckduckgo/searx a lot.
Since I already did that for most of the project parts,
the most hard ones are left.

* Find bugs that annoy you, investigate and fix them.

You might use a debugger.
For example, there are `ScanLine` method usages,
and `BitBlt` with the copy mode different than "SRCCOPY".
You should understand what the code really meant to do,
and write it in correct Pascal code. Using methods, properties, loops etc.

// If you start with graphics, you'll also have easy time advertising your progress.:)
It's easy to share a screenshot.:)


Other notes
----

Main git repo: [https://gitlab.com/vn971/cevo](https://gitlab.com/vn971/cevo)

Github mirror: [https://github.com/vn971/cevo](https://gitlab.com/vn971/cevo)

If you start making changes, do commit them in a timely manner.
Don't wait for your changes to be 1000 lines of source code!
In the contrary, make small self-contained changes.

Feel free to ask questions on  [http://c-evo.org/bb/viewtopic.php?f=4&t=119](http://c-evo.org/bb/viewtopic.php?f=4&t=119)

You will need Lazarus-1.0.12+ to work on the project. I use Lazarus-1.4-RC2.

Please send pull requests if you'll do something. At least I'd be glad to know that someone did some progress in this work. :P :)
