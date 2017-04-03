
# ワ Wa

Wacom strokes through evdev over userland

keyboardless environment concept

ワコムタブレット evdevシステム

To calibrate a device bytestream, connect the device, check for the id in /dev/input/by-id/, set the corresponding variables 'evdev-id to that id and 'calibrate to 't in wa.lisp, run wa and write functions in process-wacom-stream in wa.lisp for each physical device workflow to process the bytes displayed in the stdout stream output to process the byestream.

currently supported devices:
* Wacom CTE-440

