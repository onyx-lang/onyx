This file discusses what Onyx will look like in its shipped form, on Windows, MacOS and Linux.

Windows
---------------------
Things that are needed:
    * onyx.exe exists in the path
    * onyx.exe knows where the core modules are installed (%APPDATA%\Local ?)

MacOS
---------------------
I know nothing about shipping portable things on MacOS...

Linux
---------------------
The way that build.sh installs Onyx on Linux is pretty close to the long term solution. That being,
copying the core library to /usr/share/onyx/core and the necessary dependencies (libwasmer at the
moment) to /usr/share/onyx/lib, and the executable to /usr/bin/onyx. This feels pretty intact with
how most things are shipped on Linux. The only change I would have would be to copy things to
/usr/local/... instead of /usr/... just to avoid possible conflicts.

