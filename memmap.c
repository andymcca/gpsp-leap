
#include "memmap.h"

#ifdef MMAP_JIT_CACHE

#ifdef WIN32

	#include <windows.h>
	#include <io.h>

	void *map_jit_block(unsigned size) {
		return VirtualAlloc(0, size, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE);
	}

	void unmap_jit_block(void *bufptr, unsigned size) {
		VirtualFree(bufptr, 0, MEM_RELEASE);
	}

#else

	#include <sys/mman.h>

	// Posix implementation
	void *map_jit_block(unsigned size) {
		return mmap(0, size, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_ANON | MAP_PRIVATE, -1, 0);
	}

	void unmap_jit_block(void *bufptr, unsigned size) {
		munmap(bufptr, size);
	}

#endif /* WIN32 */

#endif /* MMAP_JIT_CACHE */

