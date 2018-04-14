# ezmon

## Description
This module contains a little utility that manages cycling through different
commands for particular monitor setups. Basically, it looks through
'/sys/class/drm' to discover what monitors (by EDID) are plugged into which
ports. The full list of (port, edid) values at any given time is hashed into a
unique identifier for any particular setup, and a file is associated with that
hash in the XdgConfig 'ezmon' directory.

The ezmon application can cycle through lines in that file and execute them. Any
kind of command is permitted, but the intended use is through lines of xrandr
commands to cycle through monitor setups across particular monitor layouts.

A simple command-line interface is included which allows cycling through
commands, resetting to the first command, creating and editing configuration
files for the current setup, and displaying some simple status information.

To keep track of cycling, the script stores the previous cycle-number and
layout-hash in two small files in the XdgData 'ezmon' directory. When these
files do not exist, the application should behave like it is starting a cycle.
If no configuration file exists for a particular layout, the default command is
"xrandr --auto"

## Example
A `tree` of my `~/.config/ezmon` directory
```
├── 398961001232439306
└── 4958540553805823642
```

Here the number `398...` corresponds to a BenQ monitor plugged into the VGA port
of my laptop and the `495...` corresponds to just the laptop screen alone.

The contents of `398...`
```
xrandr --output VGA1 --auto --primary --output LVDS1 --auto --right-of VGA1
xrandr --output VGA1 --auto --output LVDS1 --auto --same-as VGA1
xrandr --output VGA1 --auto --output LVDS1 --off
```

This means when I `ezmon reset` it calls the first line of the file. If I `ezmon
cycle` it will cycle through these commands. If I unplug my monitor and either
`cycle` or `reset` it will detect the change in monitors and switch to the file
corresponding to the new setup and run the first line.

Finally, there are `status`, `mkconfig`, and `edit` options that will display
the current status and help in creating and editing config files. These config
files can contain any code you want, so you could use them to launch the nukes
when switching monitors, caution is advised.

## Caveat
`ezmon` is very much Linux only due to its reliance on the linux `sysfs` and
xrandr. Furthermore the program author (me) is going on extended holiday very
soon, any alterations are probably best done by forking and running your own
copy, I will not be available to handle pull requests for at least 6 months.
