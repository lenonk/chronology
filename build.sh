#! /bin/bash

(cd src && mkdir -p lib/x86_64-linux)

(cd src && '/usr/bin/fpc' -MObjFPC -Scghi -Cg -g -gl -l -vewnhibq -Filib/x86_64-linux -Fl/usr/lib -Fu../../../.lazarus/lib/LazControls/lib/x86_64-linux/qt5 -Fu../../../.lazarus/lib/units/x86_64-linux/qt5 -Fu../../../.lazarus/lib/LCLBase/units/x86_64-linux -Fu../../../.lazarus/lib/LazUtils/lib/x86_64-linux -Fu../../../.lazarus/lib/units/x86_64-linux -Fu. -FUlib/x86_64-linux -FE. -ochronology -dLCL -dLCLqt5 chronology.lpr)

(cd src && '/usr/bin/fpc' -MObjFPC -Scghi -Cg -g -gl -l -vewnhibq -Filib/x86_64-linux -Fl/usr/lib -Fu../../../.lazarus/lib/LazControls/lib/x86_64-linux/qt5 -Fu../../../.lazarus/lib/units/x86_64-linux/qt5 -Fu../../../.lazarus/lib/LCLBase/units/x86_64-linux -Fu../../../.lazarus/lib/LazUtils/lib/x86_64-linux -Fu../../../.lazarus/lib/units/x86_64-linux -Fu. -FUlib/x86_64-linux -FE. -ochronology-sched -dLCL -dLCLqt5 -dNOGUI chronology.lpr)
