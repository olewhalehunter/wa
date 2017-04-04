
# ワ Wa

Wacom strokes through evdev over userland

keyboardless environment concept

ワコムタブレット evdevシステム

To calibrate a device bytestream, connect the device, check for the id in /dev/input/by-id/, and use the evdev tool evemu-record while using the physical device to gather hardware event information and then register the appropriate functionality for each event in wa.lisp.

* [working with evdev tools](https://www.freedesktop.org/wiki/Evemu/)
* [kernel doc on hardware input event codes](https://www.kernel.org/doc/Documentation/input/event-codes.txt)

currently supported devices:
* Wacom CTE-440

