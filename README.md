# Monitor Setup Utility

Setup my monitor(s) automatically (using `xrandr`) no matter what 
machine I'm on or what displays are currently connected.

## Usage

```
$ msu
```

This will:

* Disable all disconnected displays
* Enable the first connected display at its highest resolution
* Enable all other connected displays, at their highest resolutions, 
  extending each to the right of the last
* Run the `after-setup` hook (see below)

## Hooks

If `$XDG_CONFIG_HOME/msu/after-setup` exists and is executable, it is 
executed after the setup occurs. Use this to do things like reset 
wallpapers or restart any programs sensitive to monitor sizes.

## Installation

MSU is written in Haskell and installed using Cabal. Install GHC and 
cabal-install via your favorite package manager, then:

```
$ cabal update
$ cabal install msu
```

Also, be sure that `~/.cabal/bin` is in your `$PATH`.

## TODO

* `extend-{left,right,up,down}` - similar to default behavior, but 
  extends in the direction given.
* `mirror` - works out the best way (resolution-wise) to mirror all 
  connected displays then does so.
* Some method for limiting or stating explicitly which monitors are 
  extended or mirrored.

## Notes

Xrandr lists your primary monitor first. MSU doesn't rely on that per 
se, but it will process them in order and you usually want the primary 
processed first. If you find it doesn't do the Right Thing already, try 
explicitly registering one of your displays as primary (see `man xrandr` 
for how).
