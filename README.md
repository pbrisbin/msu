# Monitor Setup Utility

Setup my monitor(s) automatically using `xrandr` no matter what machine 
I'm on or what displays are or are not connected.

## Usage

```
$ msu
```

* Disable all disconnected displays

* Enable first connected display at it's highest resolution

* Enable all other connected displays, at their highest resolutions, 
  extending each to the right of the last

## Hooks

If `$XDG_CONFIG_HOME/msu/after-setup` exists and is executable, it is 
executed after the setup occurs. Use this to do things like reset 
wallpapers or restart any programs sensitive to monitor sizes.

## Installation

```
$ cabal install msu
```
