# gameplaySP
#
# Copyright (C) 2006 Exophase <exophase@gmail.com>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

#include "../gpsp_config.h"

.align 4

#define defsymbl(symbol) \
.global symbol ;         \
.global _##symbol ;      \
symbol:                  \
_##symbol:

#ifndef _WIN32
# External symbols (data + functions)
#define _update_gba update_gba
#define _block_lookup_address_arm block_lookup_address_arm
#define _block_lookup_address_thumb block_lookup_address_thumb
#define _block_lookup_address_dual block_lookup_address_dual
#define _write_io_register8 write_io_register8
#define _write_io_register16 write_io_register16
#define _write_io_register32 write_io_register32
#define _flush_translation_cache_ram flush_translation_cache_ram
#define _write_eeprom write_eeprom
#define _write_backup write_backup
#define _write_rtc write_rtc
#define _read_memory8 read_memory8
#define _read_memory8s read_memory8s
#define _read_memory16 read_memory16
#define _read_memory16s read_memory16s
#define _read_memory32 read_memory32
#define _execute_store_cpsr_body execute_store_cpsr_body
#endif

.extern _spsr

.equ REG_SP,            (13 * 4)
.equ REG_LR,            (14 * 4)
.equ REG_PC,            (15 * 4)
.equ REG_CPSR,          (16 * 4)
.equ CPU_MODE,          (17 * 4)
.equ CPU_HALT_STATE,    (18 * 4)

.equ REG_N_FLAG,        (20 * 4)
.equ REG_Z_FLAG,        (21 * 4)
.equ REG_C_FLAG,        (22 * 4)
.equ REG_V_FLAG,        (23 * 4)
.equ CHANGED_PC_STATUS, (24 * 4)
.equ COMPLETED_FRAME,   (25 * 4)
.equ OAM_UPDATED,       (26 * 4)
.equ REG_SAVE,          (27 * 4)
.equ REG_SAVE2,         (28 * 4)
.equ REG_SAVE3,         (29 * 4)
.equ REG_SAVE4,         (30 * 4)
.equ REG_SAVE5,         (31 * 4)

.equ load_u8_tbl,           -(9 * 16 * 4)
.equ load_s8_tbl,           -(8 * 16 * 4)
.equ load_u16_tbl,          -(7 * 16 * 4)
.equ load_s16_tbl,          -(6 * 16 * 4)
.equ load_u32_tbl,          -(5 * 16 * 4)
.equ store_u8_tbl,          -(4 * 16 * 4)
.equ store_u16_tbl,         -(3 * 16 * 4)
.equ store_u32_tbl,         -(2 * 16 * 4)
.equ store_aligned_u32_tbl, -(1 * 16 * 4)
.equ PALETTE_RAM_OFF,     0x0100
.equ PALETTE_RAM_CNV_OFF, 0x0500
.equ OAM_RAM_OFF,         0x0900
.equ IWRAM_OFF,           0x0D00
.equ VRAM_OFF,           0x10D00
.equ EWRAM_OFF,          0x28D00
.equ IORAM_OFF,          0xA8D00
.equ SPSR_OFF,           0xA9100
.equ RDMAP_OFF,          0xA9200

#define REG_CYCLES          %ebp

# destroys ecx and edx

.macro collapse_flag offset, shift
  mov \offset(%ebx), %ecx
  shl $\shift, %ecx
  or %ecx, %edx
.endm

.macro collapse_flags_no_update
  xor %edx, %edx
  collapse_flag REG_N_FLAG, 31
  collapse_flag REG_Z_FLAG, 30
  collapse_flag REG_C_FLAG, 29
  collapse_flag REG_V_FLAG, 28
  mov REG_CPSR(%ebx), %ecx
  and $0xFF, %ecx
  or %ecx, %edx
.endm


.macro collapse_flags
  collapse_flags_no_update
  mov %edx, REG_CPSR(%ebx)
.endm

.macro extract_flag shift, offset
  mov REG_CPSR(%ebx), %edx
  shr $\shift, %edx
  and $0x01, %edx
  mov %edx, \offset(%ebx)
.endm

.macro extract_flags
  extract_flag 31, REG_N_FLAG
  extract_flag 30, REG_Z_FLAG
  extract_flag 29, REG_C_FLAG
  extract_flag 28, REG_V_FLAG
.endm

# Process a hardware event. Since an interrupt might be
# raised we have to check if the PC has changed.

# eax: current address

st:
  .asciz "u\n"

defsymbl(x86_update_gba)
  mov %eax, REG_PC(%ebx)      # current PC = eax
  collapse_flags              # update cpsr, trashes ecx and edx

  call _update_gba            # process the next event

  mov %eax, REG_CYCLES        # new cycle count

  # did we just complete a frame? go back to main then
  cmpl $0, COMPLETED_FRAME(%ebx)
  jne return_to_main

  # did the PC change?
  cmpl $1, CHANGED_PC_STATUS(%ebx)
  je lookup_pc
  ret                         # if not, go back to caller

# Perform this on an indirect branch that will definitely go to
# ARM code, IE anything that changes the PC in ARM mode except
# for BX and data processing to PC with the S bit set.

# eax: GBA address to branch to

defsymbl(x86_indirect_branch_arm)
  call _block_lookup_address_arm
  jmp *%eax

# For indirect branches that'll definitely go to Thumb. In
# Thumb mode any indirect branches except for BX.

defsymbl(x86_indirect_branch_thumb)
  call _block_lookup_address_thumb
  jmp *%eax

# For indirect branches that can go to either Thumb or ARM,
# mainly BX (also data processing to PC with S bit set, be
# sure to adjust the target with a 1 in the lowest bit for this)

defsymbl(x86_indirect_branch_dual)
  call _block_lookup_address_dual
  jmp *%eax


# General ext memory routines

ext_store_rtc8:               # No RTC writes on byte or word access
ext_store_rtc32:
ext_store_backup16:           # Backup (flash) accessed via byte writes
ext_store_backup32:
ext_store_ignore:
  ret                         # ignore these writes

ext_store_rtc16:
  and $0xFFFF, %edx           # make value 16bit
  and $0xFF, %eax             # mask address
  jmp _write_rtc              # write out RTC register

ext_store_backup8:
  and $0xFF, %edx             # make value 8bit
  and $0xFFFF, %eax           # mask address
  jmp _write_backup           # perform backup write



write_epilogue:
  cmp $0, %eax                # 0 return means nothing happened
  jz no_alert                 # if so we can leave

  collapse_flags              # make sure flags are good for function call
  cmp $2, %eax                # see if it was an SMC trigger
  je smc_write

alert_loop:
  call _update_gba            # process the next event
  
  # did we just complete a frame? go back to main then
  cmpl $0, COMPLETED_FRAME(%ebx)
  jne return_to_main

  # see if the halt status has changed
  mov CPU_HALT_STATE(%ebx), %edx

  cmp $0, %edx                # 0 means it has
  jnz alert_loop              # if not go again

  mov %eax, REG_CYCLES        # load new cycle count
  jmp lookup_pc               # pc has definitely changed

no_alert:
  ret

ext_store_eeprom:
  jmp _write_eeprom           # perform eeprom write


# Register wrapping for various sizes
#define reg32(n) %e##n##x
#define reg16(n)  %##n##x
#define reg8(n)   %##n##l

# 16 bit bus results in duplicated 8bit accesses
#define dup8()  mov %dl, %dh
#define noop()

# Writes to EWRAM and IWRAM must check for SMC
#define smc_check_store_aligned(opsuf, addrexp)
#define smc_check_store(opsuf, addrexp)                                      ;\
  cmp##opsuf $0, addrexp                            /* Check SMC mirror */   ;\
  jne smc_write

# Memory write routines

#define write_stubs(fname, wsize, opsuf, regfn, regfn16, addrm, dup8fn)      ;\
                                                                             ;\
  /* eax: address to write to */                                             ;\
  /* edx: value to write */                                                  ;\
                                                                             ;\
defsymbl(execute_##fname##_u##wsize)                                         ;\
  mov %eax, %ecx                                 /* ecx = address */         ;\
  shr $24, %ecx                                  /* ecx = address >> 24 */   ;\
  cmp $15, %ecx                                                              ;\
  ja ext_store_ignore                                                        ;\
  /* ecx = ext_store_u*_jtable[address >> 24] */                             ;\
  jmp *fname##_u##wsize##_tbl(%ebx, %ecx, 4)                                 ;\
                                                                             ;\
ext_##fname##_iwram##wsize:                                                  ;\
  and $(0x7FFF & addrm), %eax                              /* Addr wrap */   ;\
  mov regfn(d), (IWRAM_OFF+0x8000)(%ebx, %eax)          /* Actual write */   ;\
  smc_check_##fname(opsuf, IWRAM_OFF(%ebx, %eax))                            ;\
  ret                                                                        ;\
                                                                             ;\
ext_##fname##_ewram##wsize:                                                  ;\
  and $(0x3FFFF & addrm), %eax                             /* Addr wrap */   ;\
  mov regfn(d), EWRAM_OFF(%ebx, %eax)                   /* Actual write */   ;\
  smc_check_##fname(opsuf, (EWRAM_OFF+0x40000)(%ebx, %eax))                  ;\
  ret                                                                        ;\
                                                                             ;\
ext_##fname##_vram##wsize:                                                   ;\
  and $(0x1FFFE & addrm), %eax                             /* Addr wrap */   ;\
  dup8fn()                                 /* Double byte for 8b access */   ;\
  cmp $0x18000, %eax                               /* Weird 96KB mirror */   ;\
  jb 1f                                                                      ;\
  sub $0x8000, %eax                                 /* Mirror last bank */   ;\
1:                                                                           ;\
  mov regfn16(d), VRAM_OFF(%ebx, %eax)                  /* Actual write */   ;\
  ret                                                                        ;\
                                                                             ;\
ext_##fname##_oam##wsize:                                                    ;\
  and $(0x3FE & addrm), %eax                               /* Addr wrap */   ;\
  movl $1, OAM_UPDATED(%ebx)                         /* flag OAM update */   ;\
  dup8fn()                                 /* Double byte for 8b access */   ;\
  mov regfn16(d), OAM_RAM_OFF(%ebx, %eax)               /* Actual write */   ;\
  ret                                                                        ;\
                                                                             ;\
ext_##fname##_io##wsize:                                                     ;\
  and $(0x3FF & addrm), %eax                               /* Addr wrap */   ;\
  call _write_io_register##wsize                         /* Call C code */   ;\
  jmp write_epilogue                            /* Might need an update */   ;\


write_stubs(store,         32, l, reg32, reg32, ~3, noop)
write_stubs(store,         16, w, reg16, reg16, ~1, noop)
write_stubs(store,          8, b,  reg8, reg16, ~0, dup8)
write_stubs(store_aligned, 32, l, reg32, reg32, ~3, noop)

# Palette routines are a bit special, due to 16bit bus + decoded palette

ext_store_palette8:
  and $0x3FE, %eax            # wrap around address and align to 16bits
  mov %dl, %dh                # duplicate the byte to be written
  jmp ext_store_palette16b    # perform 16bit palette write

ext_store_palette16:
  and $0x3FF, %eax            # wrap around address
ext_store_palette16b:         # entry point for 8bit write
  mov %dx, PALETTE_RAM_OFF(%ebx, %eax) # write out palette value
  mov %edx, %ecx              # cx = dx
  shl $11, %ecx               # cx <<= 11 (red component is in high bits)
  mov %dh, %cl                # bottom bits of cx = top bits of dx
  shr $2, %cl                 # move the blue component to the bottom of cl
  and $0x03E0, %dx            # isolate green component of dx
  shl $1, %dx                 # make green component 6bits
  or %edx, %ecx               # combine green component into ecx
  # write out the freshly converted palette value
  mov %cx, PALETTE_RAM_CNV_OFF(%ebx, %eax)
  ret                         # done

ext_store_palette32:
  and $0x3FF, %eax            # wrap around address
  call ext_store_palette16b   # write first 16bits
  add $2, %eax                # go to next address
  shr $16, %edx               # go to next 16bits
  jmp ext_store_palette16b    # write next 16bits


# Memory load routines

#define load_stubs(rtype, movop, addrm, albits, slowfn)                      ;\
                                                                             ;\
  /* eax: address to read */                                                 ;\
  /* edx: current PC address */                                              ;\
                                                                             ;\
defsymbl(execute_load_##rtype)                                               ;\
  mov %eax, %ecx                                /* ecx = address */          ;\
  rol $8, %ecx                                  /* ecx = ror(address, 24) */ ;\
  and $((1<<(8+albits))-1), %ecx                /* preserve align+msb */     ;\
  cmp $15, %ecx                                                              ;\
  ja ext_load_slow##rtype                                                    ;\
  jmp *load_##rtype##_tbl(%ebx, %ecx, 4)                                     ;\
                                                                             ;\
ext_load_bios##rtype:                                                        ;\
  mov %edx, REG_PC(%ebx)                            /* Store current PC */   ;\
  jmp ext_load_slow##rtype                                                   ;\
                                                                             ;\
ext_load_iwram##rtype:                                                       ;\
  and $(0x7FFF & addrm), %eax                              /* Addr wrap */   ;\
  movop (IWRAM_OFF+0x8000)(%ebx, %eax), %eax                /* Read mem */   ;\
  ret                                                                        ;\
                                                                             ;\
ext_load_ewram##rtype:                                                       ;\
  and $(0x3FFFF & addrm), %eax                             /* Addr wrap */   ;\
  movop EWRAM_OFF(%ebx, %eax), %eax                         /* Read mem */   ;\
  ret                                                                        ;\
                                                                             ;\
ext_load_vram##rtype:                                                        ;\
  and $(0x1FFFF & addrm), %eax                             /* Addr wrap */   ;\
  cmp $0x18000, %eax                               /* Weird 96KB mirror */   ;\
  jb 1f                                                                      ;\
  sub $0x8000, %eax                                 /* Mirror last bank */   ;\
1:                                                                           ;\
  movop VRAM_OFF(%ebx, %eax), %eax                          /* Read mem */   ;\
  ret                                                                        ;\
                                                                             ;\
ext_load_oam##rtype:                                                         ;\
  and $(0x3FF & addrm), %eax                               /* Addr wrap */   ;\
  movop OAM_RAM_OFF(%ebx, %eax), %eax                       /* Read mem */   ;\
  ret                                                                        ;\
                                                                             ;\
ext_load_palette##rtype:                                                     ;\
  and $(0x3FF & addrm), %eax                               /* Addr wrap */   ;\
  movop PALETTE_RAM_OFF(%ebx, %eax), %eax                   /* Read mem */   ;\
  ret                                                                        ;\
                                                                             ;\
ext_load_io##rtype:                                                          ;\
  and $(0x3FF & addrm), %eax                               /* Addr wrap */   ;\
  movop IORAM_OFF(%ebx, %eax), %eax                         /* Read mem */   ;\
  ret                                                                        ;\
                                                                             ;\
ext_load_rom##rtype:                                                         ;\
  mov %eax, %ecx                                 /* ecx = address */         ;\
  shr $15, %ecx                                  /* ecx = address >> 15 */   ;\
  mov RDMAP_OFF(%ebx, %ecx, 4), %edx             /* Read rdmap pointer */    ;\
  mov %eax, %ecx                                 /* ecx = address */         ;\
  and $0x7FFF, %ecx                              /* ecx = address LSB */     ;\
  movop (%edx, %ecx), %eax                       /* Read mem */              ;\
  ret                                                                        ;\
                                                                             ;\
ext_load_slow##rtype:                                                        ;\
  jmp slowfn                                                                 ;\

load_stubs(u32, mov,    ~3, 2, _read_memory32)
load_stubs(u16, movzwl, ~1, 1, _read_memory16)
load_stubs(s16, movswl, ~1, 1, _read_memory16s)
load_stubs( u8, movzbl, ~0, 0, _read_memory8)
load_stubs( s8, movsbl, ~0, 0, _read_memory8s)


# %eax = new_cpsr
# %edx = store_mask

defsymbl(execute_store_cpsr)
  mov %edx, REG_SAVE(%ebx)        # save store_mask

  mov %eax, %ecx                  # ecx = new_cpsr
  and %edx, %ecx                  # ecx = new_cpsr & store_mask
  mov REG_CPSR(%ebx), %eax        # eax = cpsr
  not %edx                        # edx = ~store_mask
  and %edx, %eax                  # eax = cpsr & ~store_mask
  or %ecx, %eax                   # eax = new cpsr combined with old

  call _execute_store_cpsr_body   # do the dirty work in this C function

  extract_flags                   # pull out flag vars from new CPSR

  cmp $0, %eax                    # see if return value is 0
  jnz changed_pc_cpsr             # might have changed the PC

  ret                             # return

changed_pc_cpsr:
  add $4, %esp                    # get rid of current return address
  call _block_lookup_address_arm  # lookup new PC
  jmp *%eax

smc_write:
  call _flush_translation_cache_ram

lookup_pc:
  add $4, %esp                       # Can't return, discard addr
  movl $0, CHANGED_PC_STATUS(%ebx)   # Lookup new block and jump to it
  mov REG_PC(%ebx), %eax
  testl $0x20, REG_CPSR(%ebx)
  jz lookup_pc_arm

lookup_pc_thumb:
  call _block_lookup_address_thumb
  jmp *%eax

lookup_pc_arm:
  call _block_lookup_address_arm
  jmp *%eax

# eax: cycle counter

defsymbl(execute_arm_translate_internal)
  # Save main context, since we need to return gracefully
  pushl %ebx
  pushl %esi
  pushl %edi
  pushl %ebp

  movl %edx, %ebx             # load base register (arg1)
  extract_flags               # load flag variables
  movl %eax, REG_CYCLES       # load cycle counter (arg0)

  movl REG_PC(%ebx), %eax     # load PC

  # (if the CPU is halted, do not start executing but
  #  loop in the alert loop until it wakes up)
  cmpl $0, CPU_HALT_STATE(%ebx)
  je 1f
  call alert_loop     # Need to push something to the stack
  
1:
  testl $0x20, REG_CPSR(%ebx)
  jnz 2f

  call _block_lookup_address_arm
  jmp *%eax                   # jump to it

2:
  call _block_lookup_address_thumb
  jmp *%eax

return_to_main:
  add $4, %esp             # remove current return addr
  popl %ebp
  popl %edi
  popl %esi
  popl %ebx
  ret

#define load_table(atype)                                                    ;\
  .long ext_load_bios##atype          /* 0x00 BIOS                         */;\
  .long ext_load_slow##atype          /* 0x01 open read                    */;\
  .long ext_load_ewram##atype         /* 0x02 EWRAM                        */;\
  .long ext_load_iwram##atype         /* 0x03 IWRAM                        */;\
  .long ext_load_io##atype            /* 0x04 I/O registers                */;\
  .long ext_load_palette##atype       /* 0x05 Palette RAM                  */;\
  .long ext_load_vram##atype          /* 0x06 VRAM                         */;\
  .long ext_load_oam##atype           /* 0x07 OAM RAM                      */;\
  .long ext_load_rom##atype           /* 0x08 gamepak (or RTC)             */;\
  .long ext_load_rom##atype           /* 0x09 gamepak                      */;\
  .long ext_load_rom##atype           /* 0x0A gamepak                      */;\
  .long ext_load_rom##atype           /* 0x0B gamepak                      */;\
  .long ext_load_rom##atype           /* 0x0C gamepak                      */;\
  .long ext_load_slow##atype          /* 0x0D EEPROM (possibly)            */;\
  .long ext_load_slow##atype          /* 0x0E Flash ROM/SRAM               */;\
  .long ext_load_slow##atype          /* 0x0F open read                    */;\

#define store_table(asize)                                                   ;\
  .long ext_store_ignore              /* 0x00 BIOS, ignore                 */;\
  .long ext_store_ignore              /* 0x01 invalid, ignore              */;\
  .long ext_store_ewram##asize        /* 0x02 EWRAM                        */;\
  .long ext_store_iwram##asize        /* 0x03 IWRAM                        */;\
  .long ext_store_io##asize           /* 0x04 I/O registers                */;\
  .long ext_store_palette##asize      /* 0x05 Palette RAM                  */;\
  .long ext_store_vram##asize         /* 0x06 VRAM                         */;\
  .long ext_store_oam##asize          /* 0x07 OAM RAM                      */;\
  .long ext_store_rtc##asize          /* 0x08 gamepak (RTC or ignore)      */;\
  .long ext_store_ignore              /* 0x09 gamepak, ignore              */;\
  .long ext_store_ignore              /* 0x0A gamepak, ignore              */;\
  .long ext_store_ignore              /* 0x0B gamepak, ignore              */;\
  .long ext_store_ignore              /* 0x0C gamepak, ignore              */;\
  .long ext_store_eeprom              /* 0x0D EEPROM (possibly)            */;\
  .long ext_store_backup##asize       /* 0x0E Flash ROM/SRAM               */;\
  .long ext_store_ignore              /* 0x0F ignore                       */;\

.data

defsymbl(x86_table_data)
  load_table(u8)
  load_table(s8)
  load_table(u16)
  load_table(s16)
  load_table(u32)
  store_table(8)
  store_table(16)
  store_table(32)

  # aligned word writes (non SMC signaling)
  .long ext_store_ignore              # 0x00 BIOS, ignore
  .long ext_store_ignore              # 0x01 invalid, ignore
  .long ext_store_aligned_ewram32     # 0x02 EWRAM
  .long ext_store_aligned_iwram32     # 0x03 IWRAM
  .long ext_store_io32                # 0x04 I/O registers
  .long ext_store_palette32           # 0x05 Palette RAM
  .long ext_store_vram32              # 0x06 VRAM
  .long ext_store_oam32               # 0x07 OAM RAM
  .long ext_store_ignore              # 0x08 gamepak, ignore (no RTC in 32bit)
  .long ext_store_ignore              # 0x09 gamepak, ignore
  .long ext_store_ignore              # 0x0A gamepak, ignore
  .long ext_store_ignore              # 0x0B gamepak, ignore
  .long ext_store_ignore              # 0x0C gamepak, ignore
  .long ext_store_eeprom              # 0x0D EEPROM (possibly)
  .long ext_store_ignore              # 0x0E Flash ROM/SRAM must be 8bit
  .long ext_store_ignore              # 0x0F ignore


.bss
.align 64

defsymbl(x86_table_info)
  .space 9*4*16
defsymbl(reg)
  .space 0x100
defsymbl(palette_ram)
  .space 0x400
defsymbl(palette_ram_converted)
  .space 0x400
defsymbl(oam_ram)
  .space 0x400
defsymbl(iwram)
  .space 0x10000
defsymbl(vram)
  .space 0x18000
defsymbl(ewram)
  .space 0x80000
defsymbl(io_registers)
  .space 0x400
defsymbl(spsr)
  .space 24
defsymbl(reg_mode)
  .space 196
  .space 36   # padding
defsymbl(memory_map_read)
  .space 0x8000

#ifndef MMAP_JIT_CACHE
  #error "x86 dynarec builds *require* MMAP_JIT_CACHE"
#endif

