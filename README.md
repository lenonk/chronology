# chronology
Snapshot manager for ZFS

# Note
This is a work in progress project I whipped up in Lazarus to manage ZFS snapshots.<br>
I'm getting a surprising number of cloners on this project.  Please be aware that currently it probably only works on KDE, and definitely only works on Linux.  It requires Dolphin to be installed, since xdg-open was giving me some issues, and it's hardcoded to mount snapshots in /run/media.  Given the suprising response, I'll put some more time into it than I had planned and try to iron out some of the wrinkles.  If you're looking for something like this and Chronology doesn't currently work for you, keep checking back.

# What works so far:
Creating snapshots<br>
Deleting snapshots<br>
Browsing snapshots<br>
Scheduling snapshots<br>
All settings functional and saved in config file<br>

# What doesn't work:
Creating cron jobs for running scheduled snapshots<br>

# The Good:
Goals are being accomplished, and it's pretty.<br>

# The Bad:
I haven't used pascal in 20 years, and it's slow progress.<br>
The API for libzfs is mind bogglingly bad.  Or, at least hard to use.  I don't want to shit on anyone else's work.  However, I had to say the nay no to making pascal bindings for it.  That being the case, I'm system calling the ZFS CLI.  I will proably create a C wrapper library for libzfs in the future and make pascal bindings for that.<br>

# The Ugly:
Some of the code is hacky and amateurish, due to "The Bad".<br>
I see Major Refactoring in my future.<br>
As it's both bad and ugly, I'll reiterate that I'm system calling the ZFS CLI.
