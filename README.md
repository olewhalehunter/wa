
# ワ Wa

Wacom strokes through evdev over userland

keyboardless environment concept

ワコムタブレット evdevシステム

To calibrate a device bytestream, connect the device, check for the id in /dev/input/by-id/, and use the evdev tool evemu-record to register the appropriate functionality in wa.lisp.

[working with evdev tools](https://www.freedesktop.org/wiki/Evemu/)

currently supported devices:
* Wacom CTE-440

