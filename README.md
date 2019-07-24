# Monitor Setup Utility

[ARandR][] is great for setting up monitors in a minimal way. I use it when
plugging in at some random location and/or to save scripts to set things to a
known configuration.

[arandr]: https://christian.amsuess.com/tools/arandr/

By contrast, this tool allows me to run a single command, with no options or
thought (e.g. from a udev hot-plug rule), and have that tool figure out where I
am and *what* saved-script (or literal command) to execute to configure my
monitors.

Plugging into my office dock, like I do every morning? The script notes that I'm
on my home wifi and a specific Display is now connected and it does the Right
Thing.

Unplugging to only my laptop screen connected? This script sees this and resets
to it as the single primary display.

## Usage

Create a configuration file that defines how to match a "context" and what
command to execute if matched:

**~/.monitors.yaml**:

```yaml
- name: none
  match:
    # Only our primary display is connected
    displays:
      connected:
        eq:
          - eDP1

  # Turn everything else off and turn that on
  exec:
    xrandr
      --output eDP1 --primary --mode 2560x1440
      --output DP1 --off
      --output DP2 --off
      --output DP2-1 --off
      --output DP2-2 --off
      --output DP2-3 --off
      --output HDMI1 --off
      --output HDMI2 --off
      --output VIRTUAL1 --off

- name: home-dual
  match:
    # My second display is connected
    displays:
      connected:
        eq:
          - eDP1
          - DP2-2

    # And I'm on one of my home wifi networks
    wifi:
      essid:
        in:
          - pb-and-j
          - pb-and-j-5g

  # Set up those two displays how I like
  exec:
    xrandr
      --output eDP1 --mode 2560x1440 --right-of DP2-2
      --output DP1 --off
      --output DP2 --off
      --output DP2-1 --off
      --output DP2-2 --primary --mode 2560x1440
      --output DP2-3 --off
      --output HDMI1 --off
      --output HDMI2 --off
      --output VIRTUAL1 --off
```

Then run:

```console
msu
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
