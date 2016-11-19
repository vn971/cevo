#!/bin/sh -eu

if command -v mono >/dev/null ; then
	wine "`dirname "$0"`"/CevoDotNet.exe --windowed
else
	echo "In order to use C#-based AIs (like Liberator) please install wine-mono on your OS"
	wine "`dirname "$0"`"/CevoWin32.exe --windowed
fi
