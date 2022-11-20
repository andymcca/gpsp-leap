GPSP for Retroleap
==================

This is a fork of the standalone version of gpSP, aimed at Retroleap (https://github.com/mac2612/retroleap), specifically the Leapster Explorer.

The aim of this fork is to improve upon the original fork for LF1000 by nirvous (https://github.com/nirvous/gpsp_lf1000), whilst also backporting changes made in the Libretro port (https://github.com/libretro/gpsp) which has seen many improvements in recent years.

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


