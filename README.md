
# ワ Wa

Wacom strokes through evdev over userland

keyboardless environment concept

ワコムタブレット evdevシステム

To calibrate a device bytestream, connect the device, check for the new device in /dev/input/, and use the evdev tool evemu-record while using the physical device to review hardware event information and register the appropriate event functionality in wa.lisp.

* [working with evdev tools](https://www.freedesktop.org/wiki/Evemu/)
* [kernel doc on hardware input event codes](https://www.kernel.org/doc/Documentation/input/event-codes.txt)
* [kernel input source (input event structs)](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input.h)

currently supported devices:
* Wacom CTE-440


thanks to [jtgans](https://github.com/jtgans/) for [cl-evdev](https://github.com/jtgans/cl-evdev), the [fork with wacom support added is here](https://github.com/olewhalehunter/cl-evdev)