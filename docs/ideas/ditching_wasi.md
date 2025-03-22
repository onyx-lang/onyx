Ditching WASI
-------------

After programming with the dedicated Onyx Runtime and extensible C interop,
I am beginning to hate WASI, as I see it as unnecessarily restrictive and all
implementations of WASI lack 50% of the features that it claim. If they fully
supported all features proposed by WASI, I may be more for it. But as it stands
WASI is an incomplete mess that has seen no active development in years and I
would like to ditch it for the Onyx Runtime. It will still remain available for
the WASI runtime, obviously, but I want to completely ditch it elsewhere.

In addition to reimplementing the file, directory and clock operations that were
available in WASI, Onyx could support a proper TCP/UDP networking protocol,
probably similar to the socket library in Python.

Good article about differences between Winsock and Berkeley sockets:
https://handsonnetworkprogramming.com/articles/differences-windows-winsock-linux-unix-bsd-sockets-compatibility/

One small technicality that has to be resolved is command line arguments without
WASI. I think just setting global data and then the Onyx `_startup` function will
envoke specific function like is done in WASI.

