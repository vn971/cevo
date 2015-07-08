C-evo
====

C-evo is a free empire building game for Windows and Linux(wine).

This repository is based on the original project
(Civilization Evolution http://c-evo.org),
but with these modifications:

Philosophical differences
====

* Development is happening in "git".
We want public bug reports, public MergeRequests/PullRequests,
public code changes, public forks.

* Pull requests for better Linux support will always be gladly accepted.

Actual differences by now
====

* The project is more developer-friendly.
Absolute paths were cleaned up. Directories got structured (separate dir for sources, separate dir for AIs, separate dir for graphics).

* Basic command line arguments were added. This allows nicer integration with Linux(wine). May be useful in Windows OS, too.

* Unit movement in Linux(wine) is fixed. (Does not affect Windows.)
This makes the game fully playable (and enjoyable;) in wine.

* Frames can be closed with "escape" and "enter" keys. A pair of memory leaks got fixed.

You can of course see all the technical differences using git.
Look for commits diverging from the official steffen-gerlach git branch.

Build instructions
====

Instructions using graphical IDE, presuming you have Delphi3-Delphi7 installed:

* click Menu > open project > cevo.dpr
* click Menu > Project > Build cevo
* play the game by running the project (F9)

Build instructions using command line, presuming you have Delphi3-Delphi7 installed:

* invoke  `dcc32 cevo.dpr`
* run the game with  `bin/CevoWin32.exe`

Building the game using a non-proprietary IDE...is not possible yet, unfortunately. We try to fix that in a separate git branch [here](../../tree/lazarus), but it's not ready yet.

Other
====
For license information, see LICENSE.txt (basically it's Public Domain)

If you would like to do AI development, please read the "Module Concept" section
and learn the client/server architecture of the game first:
http://c-evo.org/aidev.html

The package includes components named TButtonA, TButtonB, TButtonC,
TButtonN and TEOTButton. You must install these before you open the
cevo project.

If you're using Delphi 3, ignore the missing properties.
