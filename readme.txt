GPSP for ARM
=============

This is a fork of notaz' standalone version of gpSP, the focus of which will be ARM devices, specifically the Leapster Explorer (via Retroleap www.github.com/retroleap).  The Explorer spec is an ARMv5te CPU running at 393Mhz (overclockable to 533Mhz) and 64MB RAM.

The aim of this fork is to improve upon the original fork for LF1000 by nirvous (https://github.com/nirvous/gpsp_lf1000), whilst also backporting changes made in other forks and also the Libretro port (https://github.com/libretro/gpsp) which have seen many improvements in recent years.

Notable work so far -

- Fixed display initialisation on startup (nirvous' fork display garbled when launching from gmenunx)
- Ported input functions from nirvous' fork and fixed key mappings (config menu access and FPS counter toggle now work)
- Fixed in-game graphics in scaled/unscaled tear modes (was rendering scaled and unscaled at the same time)
- Sound now mostly working (nirvous' fork sound not working properly in Retroleap)
- Fixed nasty random crashing (caused by SDL_CONDWAIT loops in sound.c)
- Default speed set at 400Mhz, as setting to 533Mhz can cause problems for some devices - option to overclock still available.

To do -

- Fix (or remove) graphics anti-tear modes
- Fix sound dropouts which still occur in some games
- Fix volume controls (in-built functions appear not to work despite key mappings being correct)
- Backport fixes from Libretro gpSP
- Check other forks for any other possible fixes that can be applied (e.g. gpSP Kai)
- Debug specific game issues/crashes still outstanding (e.g. Banjo Kazooie)

Setup Info -

- A GBA BIOS file is required in the same directory as the executable.

- Buttons should be mapped correctly to their GBA counterparts on a Leapster Explorer.  Menu button will open the gpSP configuration menu from the file
  manager or in-game.  Brightness button will toggle FPS display.  Volume buttons don't currently work.

- A good general purpose setup is as follows -
  - Frameskipping mode Auto (this will sync games to 60FPS if possible and skip frames as required)
  - Frameskipping value 3 (this is the maximum number of frames to skip if needed to maintain sync)
  - Frameskipping mode Uniform (this is more for FS mode Manual, allowing you to skip frames regularly or at random)
  - Audio Buffer 4096 (can try this or 2048, other values seem to increase lag significantly or introduce sound drops)
  - CPU Speed 533Mhz (not always required, but can help to prevent audio stutter and general performance increase)

- Even with the above, certain games will have audio drop outs.  This is on the to-do list to fix.

- VERY IMPORTANT - restart gpSP after setting the audio buffer value as it will not be applied otherwise.  Also - when starting a game, go straight to the
  config menu to make sure that any setting you have applied are still set.  gpSP has a habit of reverting to default settings.  Once you re-apply settings,
  gpSP should save these in a game-specific config file.

