# Monitor Setup Utility

A wrapper over `xrandr` with the following goals:

* Perform the most common setup by default
* Have simple, one-word commands for other common situations
* Execute whatever setup is required as a single `xrandr` invocation

## Usage

*TODO: currently prints the command to run, does not run it!*

```
$ msu
```

* Disable any disconnected displays (`--output <name> --off`)
* Enable any connected displays, at their first mode, extending each to 
  the right of the last (`--output <name> --mode <mode> --right-of 
  <last>`)

## Hooks

*TODO*

If `$XDG_CONFIG_HOME/msu/after-setup` exists and is executable, it is 
executed after the setup occurs. Use this to do things like reset 
wallpapers or restart any programs sensitive to monitor sizes.

## Install

```
$ git clone https://github.com/pbrisbin/msu && cd ./msu && cabal install
```
