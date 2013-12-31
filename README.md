# Monitor Setup Utility

A wrapper over `xrandr` with the following goals:

* Perform the most common setup by default
* Have simple, one-word commands for other common situations
* Execute whatever setup is required as a single `xrandr` invocation

## Usage

```
$ msu
```

* Disable any disconnected displays (`--output <name> --off`)
* Enable any connected displays, at their first mode, extending each to 
  the right of the last (`--output <name> --mode <mode> --right-of 
  <last>`)

## Options

*TODO: mirror, extend-one, etc*

## Hooks

*TODO*

If `$XDG_CONFIG_HOME/msu/after-setup` exists and is executable, it is 
executed after the setup occurs. Use this to do things like reset 
wallpapers or restart any programs sensitive to monitor sizes.

## Install

*TODO: git clone, cabal install, etc*
