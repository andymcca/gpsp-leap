# gpSP makefile
# Gilead Kutnick - Exophase
# GP2X port(ion) - Z
# Retroleap changes - andymcca

# Retroleap Buildroot .mk file needs to set GP2X, WIZ and POLLUX flags to build correctly

# Global definitions

CC        = $(CROSS_COMPILE)gcc

OBJS      = main.o cpu.o memory.u video.o input.o sound.o gp2x.o gui.o        \
            cheats.o zip.o cpu_threaded.z arm_stub.o video_blend.o            \
            warm.o upscale_aspect.o
ifeq ($(WIZ),1)
POLLUX    = 1
OBJS      += pollux_dpc_set.o
BIN       = gpsp
endif
ifeq ($(CAANOO),1)
POLLUX    = 1
BIN       = gpsp_caanoo
endif
ifeq ($(BIN),)
BIN       = gpsp_gp2x
endif

-include Makefile.local

# Platform specific definitions 

VPATH      += .. ../arm ../gp2x
CFLAGS     += -DARM_ARCH -DGP2X_BUILD
ifeq ($(WIZ),1)
CFLAGS     += -DWIZ_BUILD
endif
ifeq ($(POLLUX),1)
CFLAGS     += -DPOLLUX_BUILD
endif

# debug build
#CFLAGS     += -std=c99 -msoft-float -funsigned-char -Wall -ggdb

#optimized build
# NOTE: -funroll-loops (and -O3) will slow down compiling considerably
CFLAGS     +=  -O3 -std=c99 -msoft-float -funsigned-char -fno-common           \
              -fno-builtin                                                    \

ifndef DEBUG
#CFLAGS     += -O2
endif

# Retroleap - you need to change these to point at sdl-config in the Retroleap toolchain.  May be able to utilise the CROSS_COMPILE env variable
INCLUDES   = `/home/lubuntu/newbuilds/retroleap/output/host/arm-buildroot-linux-gnueabi/sysroot/usr/bin/sdl-config --cflags`
LIBS       = `/home/lubuntu/newbuilds/retroleap/output/host/arm-buildroot-linux-gnueabi/sysroot/usr/bin/sdl-config --libs` \
              -lm -ldl -lpthread -lz


ifeq ($(WIZ)$(CAANOO),)
LIBS       += -static
endif

# Compilation:

.SUFFIXES: .c

all:    $(BIN)

cpu.o cpu_threaded.z: CFLAGS += -Wno-unused-variable -Wno-unused-label

%.z: %.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -o $@ $<

%.u: %.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -o $@ $<

%.o: %.S
	$(CC) $(ASFLAGS) $(INCLUDES) -c -o $@ $<

%.o: %.s
	$(CC) $(ASFLAGS) $(INCLUDES) -c -o $@ $<

$(BIN):	$(OBJS)
	$(CC) $(OBJS) $(LIBS) -o $(BIN)

clean:
	rm -f *.o *.u *.z $(BIN) 

# Caanoo specific - can probably be removed?
rel: gpsp_caanoo gpsp_caanoo.ini gpsp_caanoo.gpe warm_2.6.24.ko \
		../COPYING.DOC ../readme.txt readme_gp2x.txt ../game_config.txt
	rm -rf out
	mkdir -p out/gpsp_caanoo
	cp $^ out/gpsp_caanoo/
	mv out/gpsp_caanoo/gpsp_caanoo.ini out/
	mv out/gpsp_caanoo/readme_gp2x.txt out/gpsp_caanoo/readme_caanoo.txt
	echo -n '/mnt/sd' > out/gpsp_caanoo/romdir.txt
	cd out && zip -9 -r ../gpsp_caanoo.zip *
