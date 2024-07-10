# Audio
## PC Speaker
- To disable PC speaker error beep
  - In `/etc/modprobe.d/nobeep.conf`
  - Add the line `blacklist pcspkr`
  - [Arch Wiki](https://wiki.archlinux.org/title/Kernel_module#Blacklisting)

# Light
## [Keyboard Backlight](https://wiki.archlinux.org/title/keyboard_backlight)
- Check maximum keyboard brightness: `cat /sys/class/leds/vendor::kbd_backlight/max_brightness"
  - `vendor` can be:
    - `tpacpi`: Lenovo (ThinkPads)
    - `asus`: Asus
    - `dell`: Dell
- Set brightness:
  - Direct editing config file:
    - `echo [brightness_level] > /sys/class/leds/vendor::kbd_backlight/brightness`
    - This is a bit jank, one may run into issue with permissions
  - Use the `brightnessctl` utility:
    - `brightnessctl --list`: List all devices
    - `brightnessctl --device='device_id' info`: Information for a specific device
    - `brightnessctl --device='device_id' set [brightness_level]`: Set brightness level of device