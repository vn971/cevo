This "README" is targeted to developers
----

This is a short developer README.

If you're not a developer,
you would probably want to check the [master](../../tree/master) branch
of this repository.

This branch's goal is to port C-Evo
to the free development environment called "Lazarus".
Apart from switching to a free IDE,
it will also make the project cross-compilable.
That is, you would be able to launch the game on Linux/MacOS.


Current status:
----

* After about 70 commits, the compilation works almost fully!

The project compiles and is able to start itself in both Linux and Windows.

* There are a pair things I could not manage to cross-compile, yet.

It's easy to find them, search for either a comment:

```
    // lazarus todo
```

or for a conditional compilation block:

```
    {$ifdef WINDOWS}
```

* Apart from things that are not cross-compiled yet,
the game also has run-time incompatibilities.
As far as I see, the main thing is graphics.
If you'll launch the game, you'll see the differences.


What can I do?
----

* Find and try to fix compilation-level issues.

This might require using google/duckduckgo/searx a lot.
Since I already did that for most of the project parts,
only the most hard ones are left.
I don't really advise to go that way...
Better start with graphics (see below).

* Fix graphics issues.

The main reason for incompatibilities, as I see it,
is because of the project specifics.
It used a lot of 32bit-only and windows-only code.
(Personal note: hell, project even used assembly code!
I already fixed that though, moved to a pure Pascal code.)
So this is not only history, but the things that should be worked on, too.
I suspect the main reason of bugs to be the `ScanLine` method usage.
After doing `ScalLine`, the code goes to raw byte manipulation using pointer arithmetics.
By doing pointer arithmetics the code assumes some implementation details
about the internal data structure. This is probably broken.

You should understand what the code really meant to do,
and write it in pure correct Pascal code. Using methods, properties, loops etc.

If you go this way, you'll also have easy time advertising your progress.:)
It's easy to share a screenshot...:)


Other notes
----

If you start making changes, do commit them in a timely manner.
Don't wait for your changes to be 1000 lines of source code!
In the contrary, make small self-contained changes.

Feel free to ask questions on  [http://c-evo.org/bb/viewtopic.php?f=4&t=119](http://c-evo.org/bb/viewtopic.php?f=4&t=119)

You will need Lazarus-1.0.12+ to work on the project. I use Lazarus-1.4-RC2.

Please send pull requests if you'll do something. At least I'd be glad to know that someone did some progress over my work. :P :)
