cmake_minimum_required(VERSION 3.0)
project(chronology)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake_modules")
enable_language(Pascal)

set(CMAKE_Pascal_FLAGS "-Si -MObjFPC -Scghi -Cg -g -gl -l -v0 -Filib/x86_64-linux -Fl/usr/lib -Fu../../../.lazarus/lib/LazControls/lib/x86_64-linux/qt5 -Fu../../../.lazarus/lib/units/x86_64-linux/qt5 -Fu../../../.lazarus/lib/LCLBase/units/x86_64-linux -Fu../../../.lazarus/lib/LazUtils/lib/x86_64-linux -Fu../../../.lazarus/lib/units/x86_64-linux -Fu. -FUlib/x86_64-linux -FE. -dLCL -dLCLqt5")

set(chronology_SRCS src/chronology.pas)

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/lib/x86_64-linux")

add_executable(chronology ${chronology_SRCS})

install(TARGETS chronology DESTINATION /usr/bin)



project(chronology-sched)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake_modules")
enable_language(Pascal)

set(CMAKE_Pascal_FLAGS "-Si -MObjFPC -Scghi -Cg -g -gl -l -v0 -Filib/x86_64-linux -Fl/usr/lib -Fu../../../.lazarus/lib/LazControls/lib/x86_64-linux/qt5 -Fu../../../.lazarus/lib/units/x86_64-linux/qt5 -Fu../../../.lazarus/lib/LCLBase/units/x86_64-linux -Fu../../../.lazarus/lib/LazUtils/lib/x86_64-linux -Fu../../../.lazarus/lib/units/x86_64-linux -Fu. -FUlib/x86_64-linux -FE. -dLCL -dLCLqt5 -dNOGUI")

set(chronology-sched_SRCS src/chronology-sched.pas)

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/lib/x86_64-linux")

add_executable(chronology-sched ${chronology-sched_SRCS})

install(TARGETS chronology-sched DESTINATION /usr/bin)

add_dependencies(chronology-sched chronology)

install(FILES ${CMAKE_SOURCE_DIR}/support/chronology.cron DESTINATION /etc/cron.d/chronology)
install(FILES ${CMAKE_SOURCE_DIR}/support/org.kde.lk.chronology.policy DESTINATION /usr/share/polkit-1/actions)
