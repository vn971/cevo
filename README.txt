C-evo is a free empire building game for Windows (see below on Linux support).

This repository is based on the original project (Civilization Evolution http://c-evo.org),
but with these modifications:

	This project is "hackable", no absolute paths are inlined and
	there is a step-by-step instruction on how to compile from source.

	Development is happening in "git".
	We want public bug reports, public MergeRequests/PullRequests, public forks.

	Pull requests for better Linux support will always be gladly accepted
	(Linux is strongly desired as a target platform alternative).

	You can of course see all the technical differences using git.
	Look for commits diverging from the official version (steffen-gerlach git branch).


Build instructions using graphical IDE:
	install Delphi3 - Delphi7 (tested on Delphi7). Some installation archives are only 200 Mb.
	click Menu > open project > CevoWin32.dpr
	click Menu > project > Build CevoWin32
	close the project and all files
	click Menu > open project > cevo.dpr
	play the game by running the project! (F9)

Build instructions using command line:
	install Delphi, same as above
	invoke `dcc32 CevoWin32.dpr`
	invoke `dcc32 cevo.dpr`
	run the game `target/CevoWin32.exe`

Notes:
	if you make source modifications, simply re-build "cevo.dpr"


For license information, see LICENSE.txt

If you would like to do AI development, please read the "Module Concept" section
and learn the client/server architecture of the game first:
http://c-evo.org/aidev.html

The package includes components named TButtonA, TButtonB, TButtonC,
TButtonN and TEOTButton. You must install these before you open the
cevo project.

If you're using Delphi 3, ignore the missing properties.
