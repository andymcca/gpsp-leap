
#ifndef GPSP_CONFIG_H
#define GPSP_CONFIG_H

/* Cache sizes and their config knobs */
#if defined(PSP)
  #define ROM_TRANSLATION_CACHE_SIZE (1024 * 1024 * 2)
  #define RAM_TRANSLATION_CACHE_SIZE (1024 * 384)
  #define TRANSLATION_CACHE_LIMIT_THRESHOLD (1024 * 2)
#else
  #define ROM_TRANSLATION_CACHE_SIZE (1024 * 1024 * 10)
  #define RAM_TRANSLATION_CACHE_SIZE (1024 * 512)
  #define TRANSLATION_CACHE_LIMIT_THRESHOLD (1024 * 8)
#endif

/* Please note that RAM_TRANSLATION_CACHE_SIZE is limited to 512KB
   Check cpu_threaded.c for "memory tagging" for more info. */

/* This is MIPS specific for now */
#define STUB_ARENA_SIZE  (16*1024)

/* Hash table size for ROM trans cache lookups */
#define ROM_BRANCH_HASH_SIZE (1024 * 64)

#endif
