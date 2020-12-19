# chronology
Snapshot manager for ZFS

This is a work in progress project I whipped up in Lazarus to manage ZFS snapshots.

# What works so far:
Creating snapshots<br>
Deleting snapshots<br>
Browsing snapshots<br>
Scheduling snapshots<br>
All settings functional and saved in config file<br>

# What doesn't work:
Creating cron jobs for running scheduled snapshots<br>

# The Good:
Goals are ebing accomplished, and it's pretty.<br>

# The Bad:
I haven't used pascal in 20 years, and it's slow progress.<br>
The API for libzfs is mind bogglingly bad.  Or, at least hard to use.  I don't want to shit on anyone else's work.  However, I had to say the nay no to making pascal bindings for it.  That being the case, I'm system calling the ZFS CLI.  I will proably create a C wrapper library for libzfs in the future and make pascal bindings for that.<br>

# The Ugly:
Some of the code is hacky and amateurish, due to "The Bad".<br>
I see Major Refactoring in my future.<br>
As it's both bad and ugly, I'll reiterate that I'm system calling the ZFS CLI.
