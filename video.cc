/* gameplaySP
 *
 * Copyright (C) 2006 Exophase <exophase@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

extern "C" {
  #include "common.h"
}

u16* gba_screen_pixels = NULL;

#define get_screen_pixels()   gba_screen_pixels
#define get_screen_pitch()    GBA_SCREEN_PITCH

typedef struct {
  u16 attr0, attr1, attr2, attr3;
} t_oam;

typedef struct {
  u16 pad0[3];
  u16 dx;
  u16 pad1[3];
  u16 dmx;
  u16 pad2[3];
  u16 dy;
  u16 pad3[3];
  u16 dmy;
} t_affp;

typedef void (* tile_render_function)(u32 layer_number, u32 start, u32 end,
 void *dest_ptr);
typedef void (* bitmap_render_function)(u32 start, u32 end, void *dest_ptr);

typedef void (*conditional_render_function)(
  u32 start, u32 end, u16 *scanline, u32 enable_flags);


typedef struct
{
  tile_render_function base[4];
  tile_render_function trans[4];
} tile_layer_render_struct;

typedef struct
{
  bitmap_render_function blit_render;
  bitmap_render_function scale_render;
  bitmap_render_function affine_render;
} bitmap_layer_render_struct;

static void render_scanline_conditional_tile(
  u32 start, u32 end, u16 *scanline, u32 enable_flags);
static void render_scanline_conditional_bitmap(
  u32 start, u32 end, u16 *scanline, u32 enable_flags);



#define advance_dest_ptr_base(delta)                                          \
  dest_ptr += delta                                                           \

#define advance_dest_ptr_transparent(delta)                                   \
  advance_dest_ptr_base(delta)                                                \


#define color_combine_mask_a(layer)                                           \
  ((read_ioreg(REG_BLDCNT) >> layer) & 0x01)                                  \

// For color blending operations, will create a mask that has in bit
// 10 if the layer is target B, and bit 9 if the layer is target A.

#define color_combine_mask(layer)                                             \
  (color_combine_mask_a(layer) |                                              \
   ((read_ioreg(REG_BLDCNT) >> (layer + 7)) & 0x02)) << 9                     \


// OBJ should only shift if the top isn't already OBJ
#define tile_expand_transparent_alpha_obj(index)                              \
  dest = dest_ptr[index];                                                     \
  if(dest & 0x00000100)                                                       \
    dest_ptr[index] = (dest & 0xFFFF0000) | current_pixel | pixel_combine;    \
  else                                                                        \
    dest_ptr[index] = (dest << 16) | current_pixel | pixel_combine;           \


// For color effects that don't need to preserve the previous layer.
// The color32 version should be used with 32bit wide dest_ptr so as to be
// compatible with alpha combine on top of it.

#define tile_expand_base_color16(index)                                       \
  dest_ptr[index] = current_pixel | pixel_combine                             \

#define tile_expand_transparent_color16(index)                                \
  tile_expand_base_color16(index)                                             \

#define tile_expand_transparent_color32(index)                                \
  tile_expand_base_color16(index)                                             \


// Operations for isolation 8bpp pixels within 32bpp pixel blocks.

#define tile_8bpp_pixel_op_mask(op_param)                                     \
  current_pixel = current_pixels & 0xFF                                       \

#define tile_8bpp_pixel_op_shift_mask(shift)                                  \
  current_pixel = (current_pixels >> shift) & 0xFF                            \

#define tile_8bpp_pixel_op_shift(shift)                                       \
  current_pixel = current_pixels >> shift                                     \

#define tile_8bpp_pixel_op_none(shift)                                        \

// Transparent (layered) writes should only replace what is there if the
// pixel is not transparent (zero)

#define tile_8bpp_draw_transparent(index, op, op_param, alpha_op)             \
  tile_8bpp_pixel_op_##op(op_param);                                          \
  if(current_pixel)                                                           \
  {                                                                           \
    tile_expand_transparent_##alpha_op(index);                                \
  }                                                                           \


// Get the current tile from the map in 8bpp mode

#define get_tile_8bpp()                                                       \
  current_tile = eswap16(*map_ptr);                                           \
  tile_ptr = tile_base + ((current_tile & 0x3FF) * 64)                        \


// Draw half of a tile in 8bpp mode, for base renderer

#define tile_8bpp_draw_four_noflip(index, combine_op, alpha_op)               \
  tile_8bpp_draw_##combine_op(index + 0, mask, 0, alpha_op);                  \
  tile_8bpp_draw_##combine_op(index + 1, shift_mask, 8, alpha_op);            \
  tile_8bpp_draw_##combine_op(index + 2, shift_mask, 16, alpha_op);           \
  tile_8bpp_draw_##combine_op(index + 3, shift, 24, alpha_op)                 \


// Like the above, but draws the half-tile horizontally flipped

#define tile_8bpp_draw_four_flip(index, combine_op, alpha_op)                 \
  tile_8bpp_draw_##combine_op(index + 3, mask, 0, alpha_op);                  \
  tile_8bpp_draw_##combine_op(index + 2, shift_mask, 8, alpha_op);            \
  tile_8bpp_draw_##combine_op(index + 1, shift_mask, 16, alpha_op);           \
  tile_8bpp_draw_##combine_op(index + 0, shift, 24, alpha_op)                 \


// Draw half of a tile in 8bpp mode, for transparent renderer; as an
// optimization the entire thing is checked against zero (in transparent
// capable renders it is more likely for the pixels to be transparent than
// opaque)

#define tile_8bpp_draw_four_transparent(index, alpha_op, flip_op)             \
  if(current_pixels != 0)                                                     \
  {                                                                           \
    tile_8bpp_draw_four_##flip_op(index, transparent, alpha_op);              \
  }                                                                           \

// Helper macro for drawing 8bpp tiles clipped against the edge of the screen

#define partial_tile_8bpp(combine_op, alpha_op)                               \
  for(i = 0; i < partial_tile_run; i++)                                       \
  {                                                                           \
    tile_8bpp_draw_##combine_op(0, mask, 0, alpha_op);                        \
    current_pixels >>= 8;                                                     \
    advance_dest_ptr_##combine_op(1);                                         \
  }                                                                           \


// Draws 8bpp tiles clipped against the left side of the screen,
// partial_tile_offset indicates how much clipped in it is, partial_tile_run
// indicates how much it should draw.

#define partial_tile_right_noflip_8bpp(combine_op, alpha_op)                  \
  if(partial_tile_offset >= 4)                                                \
  {                                                                           \
    current_pixels = eswap32(*((u32 *)(tile_ptr + 4))) >>                     \
     ((partial_tile_offset - 4) * 8);                                         \
    partial_tile_8bpp(combine_op, alpha_op);                                  \
  }                                                                           \
  else                                                                        \
  {                                                                           \
    partial_tile_run -= 4;                                                    \
    current_pixels = eswap32(*((u32 *)tile_ptr)) >> (partial_tile_offset * 8);\
    partial_tile_8bpp(combine_op, alpha_op);                                  \
    current_pixels = eswap32(*((u32 *)(tile_ptr + 4)));                       \
    tile_8bpp_draw_four_##combine_op(0, alpha_op, noflip);                    \
    advance_dest_ptr_##combine_op(4);                                         \
  }                                                                           \


// Draws 8bpp tiles clipped against both the left and right side of the
// screen, IE, runs of less than 8 - partial_tile_offset.

#define partial_tile_mid_noflip_8bpp(combine_op, alpha_op)                    \
  if(partial_tile_offset >= 4)                                                \
  {                                                                           \
    current_pixels = eswap32(*((u32 *)(tile_ptr + 4))) >>                     \
     ((partial_tile_offset - 4) * 8);                                         \
  }                                                                           \
  else                                                                        \
  {                                                                           \
    current_pixels = eswap32(*((u32 *)tile_ptr)) >> (partial_tile_offset * 8);\
    if((partial_tile_offset + partial_tile_run) > 4)                          \
    {                                                                         \
      u32 old_run = partial_tile_run;                                         \
      partial_tile_run = 4 - partial_tile_offset;                             \
      partial_tile_8bpp(combine_op, alpha_op);                                \
      partial_tile_run = old_run - partial_tile_run;                          \
      current_pixels = eswap32(*((u32 *)(tile_ptr + 4)));                     \
    }                                                                         \
  }                                                                           \
  partial_tile_8bpp(combine_op, alpha_op);                                    \


// Draws 8bpp tiles clipped against the right side of the screen,
// partial_tile_run indicates how much there is to draw.

#define partial_tile_left_noflip_8bpp(combine_op, alpha_op)                   \
  if(partial_tile_run >= 4)                                                   \
  {                                                                           \
    current_pixels = eswap32(*((u32 *)tile_ptr));                             \
    tile_8bpp_draw_four_##combine_op(0, alpha_op, noflip);                    \
    advance_dest_ptr_##combine_op(4);                                         \
    tile_ptr += 4;                                                            \
    partial_tile_run -= 4;                                                    \
  }                                                                           \
                                                                              \
  current_pixels = eswap32(*((u32 *)(tile_ptr)));                             \
  partial_tile_8bpp(combine_op, alpha_op)                                     \


// Draws a non-clipped (complete) 8bpp tile.

#define tile_noflip_8bpp(combine_op, alpha_op)                                \
  current_pixels = eswap32(*((u32 *)tile_ptr));                               \
  tile_8bpp_draw_four_##combine_op(0, alpha_op, noflip);                      \
  current_pixels = eswap32(*((u32 *)(tile_ptr + 4)));                         \
  tile_8bpp_draw_four_##combine_op(4, alpha_op, noflip)                       \


// Like the above versions but draws flipped tiles.

#define partial_tile_flip_8bpp(combine_op, alpha_op)                          \
  for(i = 0; i < partial_tile_run; i++)                                       \
  {                                                                           \
    tile_8bpp_draw_##combine_op(0, shift, 24, alpha_op);                      \
    current_pixels <<= 8;                                                     \
    advance_dest_ptr_##combine_op(1);                                         \
  }                                                                           \

#define partial_tile_right_flip_8bpp(combine_op, alpha_op)                    \
  if(partial_tile_offset >= 4)                                                \
  {                                                                           \
    current_pixels = eswap32(*((u32 *)tile_ptr)) <<                           \
                              ((partial_tile_offset - 4) * 8);                \
    partial_tile_flip_8bpp(combine_op, alpha_op);                             \
  }                                                                           \
  else                                                                        \
  {                                                                           \
    partial_tile_run -= 4;                                                    \
    current_pixels = eswap32(*((u32 *)(tile_ptr + 4))) <<                     \
     ((partial_tile_offset - 4) * 8);                                         \
    partial_tile_flip_8bpp(combine_op, alpha_op);                             \
    current_pixels = eswap32(*((u32 *)tile_ptr));                             \
    tile_8bpp_draw_four_##combine_op(0, alpha_op, flip);                      \
    advance_dest_ptr_##combine_op(4);                                         \
  }                                                                           \

#define partial_tile_mid_flip_8bpp(combine_op, alpha_op)                      \
  if(partial_tile_offset >= 4)                                                \
    current_pixels = eswap32(*((u32 *)tile_ptr)) <<                           \
                              ((partial_tile_offset - 4) * 8);                \
  else                                                                        \
  {                                                                           \
    current_pixels = eswap32(*((u32 *)(tile_ptr + 4))) <<                     \
     ((partial_tile_offset - 4) * 8);                                         \
                                                                              \
    if((partial_tile_offset + partial_tile_run) > 4)                          \
    {                                                                         \
      u32 old_run = partial_tile_run;                                         \
      partial_tile_run = 4 - partial_tile_offset;                             \
      partial_tile_flip_8bpp(combine_op, alpha_op);                           \
      partial_tile_run = old_run - partial_tile_run;                          \
      current_pixels = eswap32(*((u32 *)(tile_ptr)));                         \
    }                                                                         \
  }                                                                           \
  partial_tile_flip_8bpp(combine_op, alpha_op);                               \

#define partial_tile_left_flip_8bpp(combine_op, alpha_op)                     \
  if(partial_tile_run >= 4)                                                   \
  {                                                                           \
    current_pixels = eswap32(*((u32 *)(tile_ptr + 4)));                       \
    tile_8bpp_draw_four_##combine_op(0, alpha_op, flip);                      \
    advance_dest_ptr_##combine_op(4);                                         \
    tile_ptr -= 4;                                                            \
    partial_tile_run -= 4;                                                    \
  }                                                                           \
                                                                              \
  current_pixels = eswap32(*((u32 *)(tile_ptr + 4)));                         \
  partial_tile_flip_8bpp(combine_op, alpha_op)                                \

#define tile_flip_8bpp(combine_op, alpha_op)                                  \
  current_pixels = eswap32(*((u32 *)(tile_ptr + 4)));                         \
  tile_8bpp_draw_four_##combine_op(0, alpha_op, flip);                        \
  current_pixels = eswap32(*((u32 *)tile_ptr));                               \
  tile_8bpp_draw_four_##combine_op(4, alpha_op, flip)                         \


// Operations for isolating 4bpp tiles in a 32bit block

#define tile_4bpp_pixel_op_mask(op_param)                                     \
  current_pixel = current_pixels & 0x0F                                       \

#define tile_4bpp_pixel_op_shift_mask(shift)                                  \
  current_pixel = (current_pixels >> shift) & 0x0F                            \

#define tile_4bpp_pixel_op_shift(shift)                                       \
  current_pixel = current_pixels >> shift                                     \

#define tile_4bpp_pixel_op_none(op_param)                                     \


// Draws a single 4bpp pixel as layered, if not transparent.

#define tile_4bpp_draw_transparent(index, op, op_param, alpha_op)             \
  tile_4bpp_pixel_op_##op(op_param);                                          \
  if(current_pixel)                                                           \
  {                                                                           \
    current_pixel |= current_palette;                                         \
    tile_expand_transparent_##alpha_op(index);                                \
  }                                                                           \

// Draws eight 4bpp pixels.

#define tile_4bpp_draw_eight_noflip(combine_op, alpha_op)                     \
  tile_4bpp_draw_##combine_op(0, mask, 0, alpha_op);                          \
  tile_4bpp_draw_##combine_op(1, shift_mask, 4, alpha_op);                    \
  tile_4bpp_draw_##combine_op(2, shift_mask, 8, alpha_op);                    \
  tile_4bpp_draw_##combine_op(3, shift_mask, 12, alpha_op);                   \
  tile_4bpp_draw_##combine_op(4, shift_mask, 16, alpha_op);                   \
  tile_4bpp_draw_##combine_op(5, shift_mask, 20, alpha_op);                   \
  tile_4bpp_draw_##combine_op(6, shift_mask, 24, alpha_op);                   \
  tile_4bpp_draw_##combine_op(7, shift, 28, alpha_op)                         \


// Draws eight 4bpp pixels in reverse order (for hflip).

#define tile_4bpp_draw_eight_flip(combine_op, alpha_op)                       \
  tile_4bpp_draw_##combine_op(7, mask, 0, alpha_op);                          \
  tile_4bpp_draw_##combine_op(6, shift_mask, 4, alpha_op);                    \
  tile_4bpp_draw_##combine_op(5, shift_mask, 8, alpha_op);                    \
  tile_4bpp_draw_##combine_op(4, shift_mask, 12, alpha_op);                   \
  tile_4bpp_draw_##combine_op(3, shift_mask, 16, alpha_op);                   \
  tile_4bpp_draw_##combine_op(2, shift_mask, 20, alpha_op);                   \
  tile_4bpp_draw_##combine_op(1, shift_mask, 24, alpha_op);                   \
  tile_4bpp_draw_##combine_op(0, shift, 28, alpha_op)                         \


// Draws eight 4bpp pixels in transparent (layered) mode, checks if all are
// zero and if so draws nothing.

#define tile_4bpp_draw_eight_transparent(alpha_op, flip_op)                   \
  if(current_pixels != 0)                                                     \
  {                                                                           \
    tile_4bpp_draw_eight_##flip_op(transparent, alpha_op);                    \
  }                                                                           \


// Gets the current tile in 4bpp mode, also getting the current palette and
// the pixel block.

#define get_tile_4bpp()                                                       \
  current_tile = eswap16(*map_ptr);                                           \
  current_palette = (current_tile >> 12) << 4;                                \
  tile_ptr = tile_base + ((current_tile & 0x3FF) * 32);                       \


// Helper macro for drawing clipped 4bpp tiles.

#define partial_tile_4bpp(combine_op, alpha_op)                               \
  for(i = 0; i < partial_tile_run; i++)                                       \
  {                                                                           \
    tile_4bpp_draw_##combine_op(0, mask, 0, alpha_op);                        \
    current_pixels >>= 4;                                                     \
    advance_dest_ptr_##combine_op(1);                                         \
  }                                                                           \


// Draws a 4bpp tile clipped against the left edge of the screen.
// partial_tile_offset is how far in it's clipped, partial_tile_run is
// how many to draw.

#define partial_tile_right_noflip_4bpp(combine_op, alpha_op)                  \
  current_pixels = eswap32(*((u32 *)tile_ptr)) >> (partial_tile_offset * 4);  \
  partial_tile_4bpp(combine_op, alpha_op)                                     \


// Draws a 4bpp tile clipped against both edges of the screen, same as right.

#define partial_tile_mid_noflip_4bpp(combine_op, alpha_op)                    \
  partial_tile_right_noflip_4bpp(combine_op, alpha_op)                        \


// Draws a 4bpp tile clipped against the right edge of the screen.
// partial_tile_offset is how many to draw.

#define partial_tile_left_noflip_4bpp(combine_op, alpha_op)                   \
  current_pixels = eswap32(*((u32 *)tile_ptr));                               \
  partial_tile_4bpp(combine_op, alpha_op)                                     \


// Draws a complete 4bpp tile row (not clipped)
#define tile_noflip_4bpp(combine_op, alpha_op)                                \
  current_pixels = eswap32(*((u32 *)tile_ptr));                               \
  tile_4bpp_draw_eight_##combine_op(alpha_op, noflip)                         \


// Like the above, but draws flipped tiles.

#define partial_tile_flip_4bpp(combine_op, alpha_op)                          \
  for(i = 0; i < partial_tile_run; i++)                                       \
  {                                                                           \
    tile_4bpp_draw_##combine_op(0, shift, 28, alpha_op);                      \
    current_pixels <<= 4;                                                     \
    advance_dest_ptr_##combine_op(1);                                         \
  }                                                                           \

#define partial_tile_right_flip_4bpp(combine_op, alpha_op)                    \
  current_pixels = eswap32(*((u32 *)tile_ptr)) << (partial_tile_offset * 4);  \
  partial_tile_flip_4bpp(combine_op, alpha_op)                                \

#define partial_tile_mid_flip_4bpp(combine_op, alpha_op)                      \
  partial_tile_right_flip_4bpp(combine_op, alpha_op)                          \

#define partial_tile_left_flip_4bpp(combine_op, alpha_op)                     \
  current_pixels = eswap32(*((u32 *)tile_ptr));                               \
  partial_tile_flip_4bpp(combine_op, alpha_op)                                \

#define tile_flip_4bpp(combine_op, alpha_op)                                  \
  current_pixels = eswap32(*((u32 *)tile_ptr));                               \
  tile_4bpp_draw_eight_##combine_op(alpha_op, flip)                           \


// Advances a non-flipped 4bpp obj to the next tile.

#define obj_advance_noflip_4bpp()                                             \
  tile_ptr += 32                                                              \


// Advances a non-flipped 8bpp obj to the next tile.

#define obj_advance_noflip_8bpp()                                             \
  tile_ptr += 64                                                              \


// Advances a flipped 4bpp obj to the next tile.

#define obj_advance_flip_4bpp()                                               \
  tile_ptr -= 32                                                              \


// Advances a flipped 8bpp obj to the next tile.

#define obj_advance_flip_8bpp()                                               \
  tile_ptr -= 64                                                              \



// Draws multiple sequential tiles from an obj, flip_op determines if it should
// be flipped or not (set to flip or noflip)

#define multiple_tile_obj(combine_op, color_depth, alpha_op, flip_op)         \
  for(i = 0; i < tile_run; i++)                                               \
  {                                                                           \
    tile_##flip_op##_##color_depth(combine_op, alpha_op);                     \
    obj_advance_##flip_op##_##color_depth();                                  \
    advance_dest_ptr_##combine_op(8);                                         \
  }                                                                           \


// Draws an obj's tile clipped against the left side of the screen

#define partial_tile_right_obj(combine_op, color_depth, alpha_op, flip_op)    \
  partial_tile_right_##flip_op##_##color_depth(combine_op, alpha_op);         \
  obj_advance_##flip_op##_##color_depth()                                     \

// Draws an obj's tile clipped against both sides of the screen

#define partial_tile_mid_obj(combine_op, color_depth, alpha_op, flip_op)      \
  partial_tile_mid_##flip_op##_##color_depth(combine_op, alpha_op)            \

// Draws an obj's tile clipped against the right side of the screen

#define partial_tile_left_obj(combine_op, color_depth, alpha_op, flip_op)     \
  partial_tile_left_##flip_op##_##color_depth(combine_op, alpha_op)           \


// Extra variables specific for 8bpp/4bpp tile renderers.

#define tile_extra_variables_4bpp()                                           \
  u32 current_palette                                                         \


// Byte lengths of complete tiles and tile rows in 4bpp and 8bpp.

#define tile_width_4bpp 4
#define tile_size_4bpp 32
#define tile_width_8bpp 8
#define tile_size_8bpp 64

#define render_scanline_dest_partial_alpha  u32


static const u32 map_widths[] = { 256, 512, 256, 512 };

typedef enum
{
  FULLCOLOR,  // Regular rendering, output a 16 bit color
  INDXCOLOR,  // Rendering to indexed color, so we can later apply dark/bright
  STCKCOLOR,  // Stacks two indexed pixels (+flags) to apply blending
  PIXCOPY     // Special mode used for sprites, to allow for obj-window drawing
} rendtype;

// Renders non-affine tiled background layer.
// Will process a full or partial tile (start and end within 0..8) and draw
// it in either 8 or 4 bpp mode. Honors vertical and horizontal flip.

template<typename dsttype, rendtype rdtype, bool transparent, bool hflip>
static inline void render_tile_Nbpp(u32 layer,
  dsttype *dest_ptr, bool is8bpp, u32 start, u32 end, u16 tile,
  const u8 *tile_base, int vertical_pixel_flip
) {
  // tile contains the tile info (contains tile index, flip bits, pal info)
  // hflip causes the tile pixels lookup to be reversed (from MSB to LSB
  // If transparent is set, color 0 is honoured (no write). Otherwise we assume
  // that we are drawing the base layer, so palette[0] is used (backdrop).

  // Seek to the specified tile, using the tile number and size.
  // tile_base already points to the right tile-line vertical offset
  const u8 *tile_ptr = &tile_base[(tile & 0x3FF) * (is8bpp ? 64 : 32)];

  // Calculate combine masks. These store 2 bits of info: 1st and 2nd target.
  // If set, the current pixel belongs to a layer that is 1st or 2nd target.
  u32 bg_comb = color_combine_mask(5);
  u32 px_comb = color_combine_mask(layer);

  // On vertical flip, apply the mirror offset
  if (tile & 0x800)
    tile_ptr += vertical_pixel_flip;

  if (is8bpp) {
    // Each byte is a color, mapped to a palete. 8 bytes can be read as 64bit
    u64 tilepix = eswap64(*(u64*)tile_ptr);
    for (u32 i = start; i < end; i++, dest_ptr++) {
      // Honor hflip by selecting bytes in the correct order
      u32 sel = hflip ? (7-i) : i;
      u8 pval = (tilepix >> (sel*8)) & 0xFF;
      // Combine mask is different if we are rendering the backdrop color
      u16 combflg = pval ? px_comb : bg_comb;
      // Alhpa mode stacks previous value (unless rendering the first layer)
      if (!transparent || pval) {
        if (rdtype == FULLCOLOR)
          *dest_ptr = palette_ram_converted[pval];
        else if (rdtype == INDXCOLOR)
          *dest_ptr = pval | combflg;  // Add combine flags
        else if (rdtype == STCKCOLOR)
          // Stack pixels on top of the pixel value and combine flags
          *dest_ptr = pval | combflg | ((transparent ? *dest_ptr : bg_comb) << 16);
      }
    }
  } else {
    // In 4bpp mode, the tile[15..12] bits contain the sub-palette number.
    u16 tilepal = (tile >> 12) << 4;
    // Only 32 bits (8 pixels * 4 bits)
    u32 tilepix = eswap32(*(u32*)tile_ptr);
    for (u32 i = start; i < end; i++, dest_ptr++) {
      u32 sel = hflip ? (7-i) : i;
      u8 pval = (tilepix >> (sel*4)) & 0xF;
      u16 combflg = pval ? px_comb : bg_comb;
      if (!transparent || pval) {
        u8 colidx = pval ? (pval | tilepal) : 0;
        if (rdtype == FULLCOLOR)
          *dest_ptr = palette_ram_converted[colidx];
        else if (rdtype == INDXCOLOR)
          *dest_ptr = colidx | combflg;
        else if (rdtype == STCKCOLOR)
          *dest_ptr = colidx | combflg | ((transparent ? *dest_ptr : bg_comb) << 16);  // Stack pixels
      }
    }
  }
}

template<typename stype, rendtype rdtype, bool transparent>
static void render_scanline_text(u32 layer,
 u32 start, u32 end, void *scanline)
{
  // TODO: Move this to the caller since it makes more sense
  // If the layer is *NOT* first target, we will not combine with previous layer anyway
  // so we can "drop" the mixing bit
  if (rdtype == STCKCOLOR && transparent) {
    bool first_target = (read_ioreg(REG_BLDCNT) >> layer) & 1;
    if (!first_target) {
      render_scanline_text<stype, INDXCOLOR, true>(layer, start, end, scanline);
      return;
    }
  }

  u32 bg_control = read_ioreg(REG_BGxCNT(layer));
  u16 vcount = read_ioreg(REG_VCOUNT);
  u32 map_size = (bg_control >> 14) & 0x03;
  u32 map_width = map_widths[map_size];
  u32 hoffset = (start + read_ioreg(REG_BGxHOFS(layer))) % 512;
  u32 voffset = (vcount + read_ioreg(REG_BGxVOFS(layer))) % 512;
  stype *dest_ptr = ((stype*)scanline) + start;
  u32 i;

  // Background map data is in vram, at an offset specified in 2K blocks.
  // (each map data block is 32x32 tiles, at 16bpp, so 2KB)
  u32 base_block = (bg_control >> 8) & 0x1F;
  u16 *map_base = (u16 *)&vram[base_block * 2048];
  u16 *map_ptr, *second_ptr;

  end -= start;

  // Skip the top one/two block(s) if using the bottom half
  if ((map_size & 0x02) && (voffset >= 256))
    map_base += ((map_width / 8) * 32);

  // Skip the top tiles within the block
  map_base += (((voffset % 256) / 8) * 32);

  // we might need to render from two charblocks, store a second pointer.
  second_ptr = map_ptr = map_base;

  if(map_size & 0x01)    // If background is 512 pixels wide
  {
    if(hoffset >= 256)
    {
      // If we are rendering the right block, skip a whole charblock
      hoffset -= 256;
      map_ptr += (32 * 32);
    }
    else
    {
      // If we are rendering the left block, we might overrun into the right
      second_ptr += (32 * 32);
    }
  }
  else
  {
    hoffset %= 256;     // Background is 256 pixels wide
  }

  // Skip the left blocks within the block
  map_ptr += hoffset / 8;

  {
    bool mode8bpp = (bg_control & 0x80);   // Color depth 8bpp when set

    // Render a single scanline of text tiles
    u32 tilewidth = mode8bpp ? tile_width_8bpp : tile_width_4bpp;
    u32 vert_pix_offset = (voffset % 8) * tilewidth;
    // Calculate the pixel offset between a line and its "flipped" mirror.
    // The values can be {56, 40, 24, 8, -8, -24, -40, -56}
    s32 vflip_off = mode8bpp ?
         tile_size_8bpp - 2*vert_pix_offset - tile_width_8bpp :
         tile_size_4bpp - 2*vert_pix_offset - tile_width_4bpp;

    // The tilemap base is selected via bgcnt (16KiB chunks)
    u32 tilecntrl = (bg_control >> 2) & 0x03;
    // Account for the base offset plus the tile vertical offset
    u8 *tile_base = &vram[tilecntrl * 16*1024 + vert_pix_offset];
    // Number of pixels available until the end of the tile block
    u32 pixel_run = 256 - hoffset;

    u32 tile_hoff = hoffset % 8;
    u32 partial_hcnt = 8 - tile_hoff;

    if (tile_hoff) {
      // First partial tile, only right side is visible.
      u32 todraw = MIN(end, partial_hcnt); // [1..7]
      u32 stop = tile_hoff + todraw;   // Usually 8, unless short run.

      u16 tile = eswap16(*map_ptr++);
      if (tile & 0x400)   // Tile horizontal flip
        render_tile_Nbpp<stype, rdtype, transparent, true>(layer, dest_ptr, mode8bpp, tile_hoff, stop, tile, tile_base, vflip_off);
      else
        render_tile_Nbpp<stype, rdtype, transparent, false>(layer, dest_ptr, mode8bpp, tile_hoff, stop, tile, tile_base, vflip_off);

      dest_ptr += todraw;
      end -= todraw;
      pixel_run -= todraw;
    }

    if (!end)
      return;

    // Now render full tiles
    u32 todraw = MIN(end, pixel_run) / 8;

    for (i = 0; i < todraw; i++) {
      u16 tile = eswap16(*map_ptr++);
      if (tile & 0x400)   // Tile horizontal flip
        render_tile_Nbpp<stype, rdtype, transparent, true>(layer, &dest_ptr[i * 8], mode8bpp, 0, 8, tile, tile_base, vflip_off);
      else
        render_tile_Nbpp<stype, rdtype, transparent, false>(layer, &dest_ptr[i * 8], mode8bpp, 0, 8, tile, tile_base, vflip_off);
    }

    end -= todraw * 8;
    pixel_run -= todraw * 8;
    dest_ptr += todraw * 8;

    if (!end)
      return;

    // Switch to the next char block if we ran out of tiles
    if (!pixel_run)
      map_ptr = second_ptr;

    todraw = end / 8;
    if (todraw) {
      for (i = 0; i < todraw; i++) {
        u16 tile = eswap16(*map_ptr++);
        if (tile & 0x400)   // Tile horizontal flip
          render_tile_Nbpp<stype, rdtype, transparent, true>(layer, &dest_ptr[i * 8], mode8bpp, 0, 8, tile, tile_base, vflip_off);
        else
          render_tile_Nbpp<stype, rdtype, transparent, false>(layer, &dest_ptr[i * 8], mode8bpp, 0, 8, tile, tile_base, vflip_off);
      }

      end -= todraw * 8;
      dest_ptr += todraw * 8;
    }

    // Finalize the tile rendering the left side of it (from 0 up to "end").
    if (end) {
      u16 tile = eswap16(*map_ptr++);
      if (tile & 0x400)   // Tile horizontal flip
        render_tile_Nbpp<stype, rdtype, transparent, true>(layer, dest_ptr, mode8bpp, 0, end, tile, tile_base, vflip_off);
      else
        render_tile_Nbpp<stype, rdtype, transparent, false>(layer, dest_ptr, mode8bpp, 0, end, tile, tile_base, vflip_off);
    }
  }
}


s32 affine_reference_x[2];
s32 affine_reference_y[2];

static inline s32 signext28(u32 value)
{
  s32 ret = (s32)(value << 4);
  return ret >> 4;
}

void video_reload_counters()
{
  /* This happens every Vblank */
  affine_reference_x[0] = signext28(read_ioreg32(REG_BG2X_L));
  affine_reference_y[0] = signext28(read_ioreg32(REG_BG2Y_L));
  affine_reference_x[1] = signext28(read_ioreg32(REG_BG3X_L));
  affine_reference_y[1] = signext28(read_ioreg32(REG_BG3Y_L));
}


template<typename dsttype, rendtype rdtype, bool transparent>
static inline void render_pixel_8bpp(u32 layer,
  dsttype *dest_ptr, u32 px, u32 py, const u8 *tile_base, const u8 *map_base, u32 map_size
) {
  // Pitch represents the log2(number of tiles per row) (from 16 to 128)
  u32 map_pitch = map_size + 4;
  // Given coords (px,py) in the background space, find the tile.
  u32 mapoff = (px / 8) + ((py / 8) << map_pitch);
  // Each tile is 8x8, so 64 bytes each.
  const u8 *tile_ptr = &tile_base[map_base[mapoff] * tile_size_8bpp];
  // Read the 8bit color within the tile.
  u8 pval = tile_ptr[(px % 8) + ((py % 8) * 8)];

  // Calculate combine masks. These store 2 bits of info: 1st and 2nd target.
  // If set, the current pixel belongs to a layer that is 1st or 2nd target.
  u32 bg_comb = color_combine_mask(5);
  u32 px_comb = color_combine_mask(layer);

  // Combine mask is different if we are rendering the backdrop color
  u16 combflg = pval ? px_comb : bg_comb;
  // Alhpa mode stacks previous value (unless rendering the first layer)
  if (!transparent || pval) {
    if (rdtype == FULLCOLOR)
      *dest_ptr = palette_ram_converted[pval];
    else if (rdtype == INDXCOLOR)
      *dest_ptr = pval | combflg;  // Add combine flags
    else if (rdtype == STCKCOLOR)
      // Stack pixels on top of the pixel value and combine flags
      *dest_ptr = pval | combflg | ((transparent ? *dest_ptr : bg_comb) << 16);
  }
}

template<typename dsttype, rendtype rdtype>
static inline void render_bdrop_pixel_8bpp(dsttype *dest_ptr) {
  // Calculate combine masks. These store 2 bits of info: 1st and 2nd target.
  // If set, the current pixel belongs to a layer that is 1st or 2nd target.
  u32 bg_comb = color_combine_mask(5);
  u32 pval = 0;

  // Alhpa mode stacks previous value (unless rendering the first layer)
  if (rdtype == FULLCOLOR)
    *dest_ptr = palette_ram_converted[pval];
  else if (rdtype == INDXCOLOR)
    *dest_ptr = pval | bg_comb;  // Add combine flags
  else if (rdtype == STCKCOLOR)
    // Stack pixels on top of the pixel value and combine flags
    *dest_ptr = pval | bg_comb | (bg_comb << 16);
  // FIXME: Do we need double bg_comb? I do not think so!
}

// Affine background rendering logic.
// wrap extends the background infinitely, otherwise transparent/backdrop fill
// rotate indicates if there's any rotation (optimized version for no-rotation)
template <typename dsttype, rendtype rdtype, bool transparent, bool wrap, bool rotate>
static inline void render_affine_background(
  u32 layer, u32 start, u32 cnt, const u8 *map_base,
  u32 map_size, const u8 *tile_base, dsttype *dst_ptr) {

  s32 dx = (s16)read_ioreg(REG_BGxPA(layer));
  s32 dy = (s16)read_ioreg(REG_BGxPC(layer));

  s32 source_x = affine_reference_x[layer - 2] + (start * dx);
  s32 source_y = affine_reference_y[layer - 2] + (start * dy);

  // Maps are squared, four sizes available (128x128 to 1024x1024)
  u32 width_height = 128 << map_size;

  if (wrap) {
    // In wrap mode the entire space is covered, since it "wraps" at the edges
    while (cnt--) {
      u32 pixel_x = (u32)(source_x >> 8) & (width_height-1);
      u32 pixel_y = (u32)(source_y >> 8) & (width_height-1);

      // Lookup pixel and draw it.
      render_pixel_8bpp<dsttype, rdtype, transparent>(
        layer, dst_ptr++, pixel_x, pixel_y, tile_base, map_base, map_size);

      // Move to the next pixel, update coords accordingly
      source_x += dx;
      if (rotate)
        source_y += dy;
    }
  } else {

    // Early optimization if Y-coord is out completely for this line.
    // (if there's no rotation Y coord remains identical throughout the line).
    bool is_y_out = !rotate && ((u32)(source_y >> 8)) >= width_height;

    if (!is_y_out) {
      // Draw backdrop pixels if necessary until we reach the background edge.
      // TODO: on non-base cases this could perhaps be calculated in O(1)?
      while (cnt) {
        // Draw backdrop pixels if they lie outside of the background.
        u32 pixel_x = (u32)(source_x >> 8), pixel_y = (u32)(source_y >> 8);

        // Stop once we find a pixel that is actually *inside* the map.
        if (pixel_x < width_height && pixel_y < width_height)
          break;

        // Draw a "transparent" pixel if we are the base layer.
        if (!transparent)
          render_bdrop_pixel_8bpp<dsttype, rdtype>(dst_ptr);

        dst_ptr++;
        source_x += dx;
        if (rotate)
          source_y += dy;
        cnt--;
      }

      // Draw background pixels by looking them up in the map
      while (cnt) {
        u32 pixel_x = (u32)(source_x >> 8), pixel_y = (u32)(source_y >> 8);

        // Check if we run out of background pixels, stop drawing.
        if (pixel_x >= width_height || pixel_y >= width_height)
          break;

        // Lookup pixel and draw it.
        render_pixel_8bpp<dsttype, rdtype, transparent>(
          layer, dst_ptr++, pixel_x, pixel_y, tile_base, map_base, map_size);

        // Move to the next pixel, update coords accordingly
        cnt--;
        source_x += dx;
        if (rotate)
          source_y += dy;
      }
    }

    // Complete the line on the right, if we ran out over the bg edge.
    // Only necessary for the base layer, otherwise we can safely finish.
    if (!transparent)
      while (cnt--)
        render_bdrop_pixel_8bpp<dsttype, rdtype>(dst_ptr++);
  }
}


// Renders affine backgrounds. These differ substantially from non-affine
// ones. Tile maps are byte arrays (instead of 16 bit), limiting the map to
// 256 different tiles (with no flip bits and just one single 256 color pal).

template<typename dsttype, rendtype rdtype, bool transparent>
static void render_scanline_affine(u32 layer,
 u32 start, u32 end, void *scanline)
{
  u32 bg_control = read_ioreg(REG_BGxCNT(layer));
  u32 map_size = (bg_control >> 14) & 0x03;

  // Char block base pointer
  u32 base_block = (bg_control >> 8) & 0x1F;
  u8 *map_base = &vram[base_block * 2048];
  // The tilemap base is selected via bgcnt (16KiB chunks)
  u32 tilecntrl = (bg_control >> 2) & 0x03;
  u8 *tile_base = &vram[tilecntrl * 16*1024];

  dsttype *dest_ptr = ((dsttype*)scanline) + start;

  bool has_rotation = read_ioreg(REG_BGxPC(layer)) != 0;
  bool has_wrap = (bg_control >> 13) & 1;

  // Four specialized versions for faster rendering on specific cases like
  // scaling only or non-wrapped backgrounds.
  if (has_wrap) {
    if (has_rotation)
      render_affine_background<dsttype, rdtype, transparent, true, true>(
        layer, start, end - start, map_base, map_size, tile_base, dest_ptr);
    else
      render_affine_background<dsttype, rdtype, transparent, true, false>(
        layer, start, end - start, map_base, map_size, tile_base, dest_ptr);
  } else {
    if (has_rotation)
      render_affine_background<dsttype, rdtype, transparent, false, true>(
        layer, start, end - start, map_base, map_size, tile_base, dest_ptr);
    else
      render_affine_background<dsttype, rdtype, transparent, false, false>(
        layer, start, end - start, map_base, map_size, tile_base, dest_ptr);
  }
}


// Renders a bitmap honoring the pixel mode and any affine transformations.
// There's optimized versions for bitmaps without scaling / rotation.
template<unsigned mode, typename pixfmt, unsigned width, unsigned height, bool scale, bool rotate>
static inline void render_scanline_bitmap(u32 start, u32 end, void *scanline) {

  // Modes 4 and 5 feature double buffering.
  bool second_frame = (mode >= 4) && (read_ioreg(REG_DISPCNT) & 0x10);
  pixfmt *src_ptr = (pixfmt*)&vram[second_frame ? 0xA000 : 0x0000];
  u16 *dst_ptr = ((u16*)scanline) + start;

  s32 dx = (s16)read_ioreg(REG_BG2PA);
  s32 dy = (s16)read_ioreg(REG_BG2PC);

  s32 source_x = affine_reference_x[0] + (start * dx); // Always BG2
  s32 source_y = affine_reference_y[0] + (start * dy);

  // Premature abort render optimization if bitmap out of Y coordinate.
  bool is_y_out = !rotate && ((u32)(source_y >> 8)) >= height;
  if (is_y_out)
    return;

  if (!scale) {
    // Pretty much a blit onto the output buffer.
    // Skip to the X pixel (dest) and start copying (drawing really)
    if (source_x < 0) {
      // TODO: Not sure if the math is OK for non-integer offsets
      u32 delta = (-source_x + 255) >> 8;
      dst_ptr += delta;
      start += delta;
      source_x += delta << 8;
    }

    u32 pixel_y = (u32)(source_y >> 8);
    u32 pixel_x = (u32)(source_x >> 8);
    while (start < end && pixel_x < width) {
      // Pretty much pixel copier
      pixfmt *valptr = &src_ptr[pixel_x + (pixel_y * width)];
      pixfmt val = sizeof(pixfmt) == 2 ? eswap16(*valptr) : *valptr;

      if (mode != 4)
        *dst_ptr = convert_palette(val);         // Direct color
      else if (val)
        *dst_ptr = palette_ram_converted[val];   // Indexed color

      // Move to the next pixel, update coords accordingly
      start++;
      dst_ptr++;
      pixel_x++;
    }
  } else {

    // Look for the first pixel to be drawn.
    // TODO This can be calculated in O(1), at least for non-rotation
    while (start < end) {
      u32 pixel_x = (u32)(source_x >> 8), pixel_y = (u32)(source_y >> 8);

      // Stop once we find a pixel that is actually *inside*
      if (pixel_x < width && pixel_y < height)
        break;

      dst_ptr++;
      source_x += dx;
      if (rotate)
        source_y += dy;
      start++;
    }

    // Draw background pixels by looking them up in the map
    while (start < end) {
      u32 pixel_x = (u32)(source_x >> 8), pixel_y = (u32)(source_y >> 8);

      // Check if we run out of background pixels, stop drawing.
      if (pixel_x >= width || pixel_y >= height)
        break;

      // Lookup pixel and draw it.
      pixfmt *valptr = &src_ptr[pixel_x + (pixel_y * width)];
      pixfmt val = sizeof(pixfmt) == 2 ? eswap16(*valptr) : *valptr;

      if (mode != 4)
        *dst_ptr = convert_palette(val);         // Direct color
      else if (val)
        *dst_ptr = palette_ram_converted[val];   // Indexed color

      // Move to the next pixel, update coords accordingly
      start++;
      dst_ptr++;
      source_x += dx;
      if (rotate)
        source_y += dy;
    }
  }
}

// Fill in the renderers for a layer based on the mode type,

#define tile_layer_render_functions(type)                                     \
{                                                                             \
  {                                                                           \
    render_scanline_##type<u16, FULLCOLOR, false>,                            \
    render_scanline_##type<u16, INDXCOLOR, false>,   /* former color16 */     \
    render_scanline_##type<u32, INDXCOLOR, false>,   /* former color32 */     \
    render_scanline_##type<u32, STCKCOLOR, false>,   /* for alpha blending */ \
  },{                                                                         \
    render_scanline_##type<u16, FULLCOLOR, true>,                             \
    render_scanline_##type<u16, INDXCOLOR, true>,                             \
    render_scanline_##type<u32, INDXCOLOR, true>,                             \
    render_scanline_##type<u32, STCKCOLOR, true>,                             \
  }                                                                           \
}                                                                             \

#define bitmap_layer_render_functions(mode, ttype, w, h)                      \
{                                                                             \
  render_scanline_bitmap<mode, ttype, w, h, false, false>,                    \
  render_scanline_bitmap<mode, ttype, w, h, true, false>,                     \
  render_scanline_bitmap<mode, ttype, w, h, true, true>,                      \
}                                                                             \

// Structs containing functions to render the layers for each mode, for
// each render type.
static const tile_layer_render_struct tile_mode_renderers[3][4] =
{
  {
    tile_layer_render_functions(text), tile_layer_render_functions(text),
    tile_layer_render_functions(text), tile_layer_render_functions(text)
  },
  {
    tile_layer_render_functions(text), tile_layer_render_functions(text),
    tile_layer_render_functions(affine), tile_layer_render_functions(text)
  },
  {
    tile_layer_render_functions(text), tile_layer_render_functions(text),
    tile_layer_render_functions(affine), tile_layer_render_functions(affine)
  }
};

static const bitmap_layer_render_struct bitmap_mode_renderers[3] =
{
  bitmap_layer_render_functions(3, u16, 240, 160),
  bitmap_layer_render_functions(4, u8,  240, 160),
  bitmap_layer_render_functions(5, u16, 160, 128)
};


// Object/Sprite rendering logic

static const u32 obj_width_table[] =
  { 8, 16, 32, 64, 16, 32, 32, 64, 8, 8, 16, 32 };
static const u32 obj_height_table[] =
  { 8, 16, 32, 64, 8, 8, 16, 32, 16, 32, 32, 64 };

static const u8 obj_dim_table[3][4][2] = {
  { {8, 8}, {16, 16}, {32, 32}, {64, 64} },
  { {16, 8}, {32, 8}, {32, 16}, {64, 32} },
  { {8, 16}, {8, 32}, {16, 32}, {32, 64} }
};

static u8 obj_priority_list[5][160][128];
static u8 obj_priority_count[5][160];
static u8 obj_alpha_count[160];

typedef struct {
  s32 obj_x, obj_y;
  s32 obj_w, obj_h;
} t_sprite;

// Renders a tile row (8 pixels) for a regular (non-affine) object/sprite.
template<typename dsttype, rendtype rdtype, bool is8bpp, bool hflip>
static inline void render_obj_tile_Nbpp(
  dsttype *dest_ptr, u32 start, u32 end, const u8 *tile_ptr, u16 palette
) {
  // tile_ptr points to the tile row (32 or 64 bits depending on bpp).
  // renders the tile honoring hflip and start/end constraints

  // Calculate combine masks. These store 2 bits of info: 1st and 2nd target.
  // If set, the current pixel belongs to a layer that is 1st or 2nd target.
  u32 px_comb = color_combine_mask(4);

  if (is8bpp) {
    // Each byte is a color, mapped to a palete. 8 bytes can be read as 64bit
    u64 tilepix = eswap64(*(u64*)tile_ptr);
    for (u32 i = start; i < end; i++, dest_ptr++) {
      // Honor hflip by selecting bytes in the correct order
      u32 sel = hflip ? (7-i) : i;
      u8 pval = (tilepix >> (sel*8)) & 0xFF;
      // Alhpa mode stacks previous value
      if (pval) {
        if (rdtype == FULLCOLOR)
          *dest_ptr = palette_ram_converted[pval | 0x100];
        else if (rdtype == INDXCOLOR)
          *dest_ptr = pval | px_comb | 0x100;  // Add combine flags
        else if (rdtype == STCKCOLOR) {
          // Stack pixels on top of the pixel value and combine flags
          // We do not stack OBJ on OBJ, rather overwrite the previous object
          if (*dest_ptr & 0x100)
            *dest_ptr = pval | px_comb | 0x100 | ((*dest_ptr) & 0xFFFF0000);
          else
            *dest_ptr = pval | px_comb | 0x100 | ((*dest_ptr) << 16);
        }
        else if (rdtype == PIXCOPY)
          *dest_ptr = dest_ptr[240];
        // TODO implement partial alpha blending
      }
    }
  } else {
    // Only 32 bits (8 pixels * 4 bits)
    u32 tilepix = eswap32(*(u32*)tile_ptr);
    for (u32 i = start; i < end; i++, dest_ptr++) {
      u32 sel = hflip ? (7-i) : i;
      u8 pval = (tilepix >> (sel*4)) & 0xF;
      if (pval) {
        u8 colidx = pval | palette;
        if (rdtype == FULLCOLOR)
          *dest_ptr = palette_ram_converted[colidx | 0x100];
        else if (rdtype == INDXCOLOR)
          *dest_ptr = colidx | px_comb | 0x100;
        else if (rdtype == STCKCOLOR) {
          if (*dest_ptr & 0x100)
            *dest_ptr = colidx | px_comb | 0x100 | ((*dest_ptr) & 0xFFFF0000);
          else
            *dest_ptr = colidx | px_comb | 0x100 | ((*dest_ptr) << 16);  // Stack pixels
        }
        else if (rdtype == PIXCOPY)
          *dest_ptr = dest_ptr[240];
      }
    }
  }
}

// Renders a regular sprite (non-affine) row to screen.
// delta_x is the object X coordinate referenced from the window start.
// cnt is the maximum number of pixels to draw, honoring window, obj width, etc.
template <typename stype, rendtype rdtype, bool is8bpp, bool hflip>
static void render_object(
  s32 delta_x, u32 cnt, stype *dst_ptr, const u8* tile_ptr, u16 palette
) {
  // Tile size in bytes for each mode
  u32 tile_bsize = is8bpp ? tile_size_8bpp : tile_size_4bpp;
  // Number of bytes to advance (or rewind) on the tile map
  s32 tile_size_off = hflip ? -tile_bsize : tile_bsize;

  if (delta_x < 0) {      // Left part is outside of the screen/window.
    u32 offx = -delta_x;  // How many pixels did we skip from the object?
    s32 block_off = offx / 8;
    u32 tile_off = offx % 8;

    // Skip the first object tiles (skips in the flip direction)
    tile_ptr += block_off * tile_size_off;

    // Render a partial tile to the left
    if (tile_off) {
      u32 residual = 8 - tile_off;   // Pixel count to complete the first tile
      u32 maxpix = MIN(residual, cnt);
      render_obj_tile_Nbpp<stype, rdtype, is8bpp, hflip>(dst_ptr, tile_off, tile_off + maxpix, tile_ptr, palette);

      // Move to the next tile
      tile_ptr += tile_size_off;
      // Account for drawn pixels
      cnt -= maxpix;
      dst_ptr += maxpix;
    }
  } else {
    // Render object completely from the left. Skip the empty space to the left
    dst_ptr += delta_x;
  }

  // Render full tiles to the scan line.
  s32 num_tiles = cnt / 8;
  while (num_tiles--) {
    // Render full tiles
    render_obj_tile_Nbpp<stype, rdtype, is8bpp, hflip>(dst_ptr, 0, 8, tile_ptr, palette);
    tile_ptr += tile_size_off;
    dst_ptr += 8;
  }

  // Render any partial tile on the end
  cnt = cnt % 8;
  if (cnt)
    render_obj_tile_Nbpp<stype, rdtype, is8bpp, hflip>(dst_ptr, 0, cnt, tile_ptr, palette);
}


// Renders an affine sprite row to screen.
template <typename stype, rendtype rdtype, bool is8bpp, bool rotate>
static void render_affine_object(
  const t_sprite *obji, const t_affp *affp, bool is_double, u32 start, u32 end, stype *dst_ptr,
  const u8 *base_tile, u16 palette
) {
  // Tile size in bytes for each mode
  const u32 tile_bsize = is8bpp ? tile_size_8bpp : tile_size_4bpp;
  const u32 tile_bwidth = is8bpp ? tile_width_8bpp : tile_width_4bpp;

  // Affine params
  s32 dx = (s16)eswap16(affp->dx);
  s32 dy = (s16)eswap16(affp->dy);
  s32 dmx = (s16)eswap16(affp->dmx);
  s32 dmy = (s16)eswap16(affp->dmy);

  // Object dimensions and boundaries
  u32 obj_dimw = obji->obj_w;
  u32 obj_dimh = obji->obj_h;
  s32 middle_x = is_double ? obji->obj_w : (obji->obj_w / 2);
  s32 middle_y = is_double ? obji->obj_h : (obji->obj_h / 2);
  s32 obj_width  = is_double ? obji->obj_w * 2 : obji->obj_w;
  s32 obj_height = is_double ? obji->obj_h * 2 : obji->obj_h;

  s32 vcount = read_ioreg(REG_VCOUNT);
  s32 y_delta = vcount - (obji->obj_y + middle_y);

  if (obji->obj_x < (signed)start)
    middle_x -= (start - obji->obj_x);
  s32 source_x = (obj_dimw << 7) + (y_delta * dmx) - (middle_x * dx);
  s32 source_y = (obj_dimh << 7) + (y_delta * dmy) - (middle_x * dy);

  // Early optimization if Y-coord is out completely for this line.
  // (if there's no rotation Y coord remains identical throughout the line).
  if (!rotate && ((u32)(source_y >> 8)) >= (u32)obj_height)
    return;

  u32 d_start = MAX((signed)start, obji->obj_x);
  u32 d_end   = MIN((signed)end,   obji->obj_x + obj_width);
  u32 cnt = d_end - d_start;
  dst_ptr += d_start;

  bool obj1dmap = read_ioreg(REG_DISPCNT) & 0x40;
  const u32 tile_pitch = obj1dmap ? (obj_dimw / 8) * tile_bsize : 1024;

  // Skip pixels outside of the sprite area, until we reach the sprite "inside"
  while (cnt) {
    u32 pixel_x = (u32)(source_x >> 8), pixel_y = (u32)(source_y >> 8);

    // Stop once we find a pixel that is actually *inside* the map.
    if (pixel_x < obj_dimw && pixel_y < obj_dimh)
      break;

    dst_ptr++;
    source_x += dx;
    if (rotate)
      source_y += dy;
    cnt--;
  }

  // Draw sprite pixels by looking them up first. Lookup address is tricky!
  while (cnt) {
    u32 pixel_x = (u32)(source_x >> 8), pixel_y = (u32)(source_y >> 8);

    // Check if we run out of the sprite, then we can safely abort.
    if (pixel_x >= obj_dimw || pixel_y >= obj_dimh)
      return;

    // Lookup pixel and draw it.
    //render_pixel_8bpp<dsttype, rdtype, transparent>(
    //  layer, dst_ptr++, pixel_x, pixel_y, tile_base, map_base, map_size);
    u8 pixval;
    if (is8bpp) {
      // We lookup the byte directly and render it.
      const u32 tile_off =
        ((pixel_y >> 3) * tile_pitch) +    // Skip vertical blocks
        ((pixel_x >> 3) * tile_bsize) +    // Skip horizontal blocks
        ((pixel_y & 0x7) * tile_bwidth) +  // Skip vertical rows to the pixel
        (pixel_x & 0x7);                   // Skip the horizontal offset

      pixval = base_tile[tile_off];     // Read pixel value!
    } else {
      const u32 tile_off =
        ((pixel_y >> 3) * tile_pitch) +    // Skip vertical blocks
        ((pixel_x >> 3) * tile_bsize) +    // Skip horizontal blocks
        ((pixel_y & 0x7) * tile_bwidth) +  // Skip vertical rows to the pixel
        ((pixel_x >> 1) & 0x3);            // Skip the horizontal offset

      u8 pixpair = base_tile[tile_off];     // Read two pixels (4bit each)
      pixval = ((pixel_x & 1) ? pixpair >> 4 : pixpair & 0xF);
    }

    // Render the pixel value
    u32 comb = color_combine_mask(4);
    if (pixval) {
      if (rdtype == FULLCOLOR)
        *dst_ptr = palette_ram_converted[pixval | palette| 0x100];
      else if (rdtype == INDXCOLOR)
        *dst_ptr = pixval | palette | 0x100 | comb;  // Add combine flags
      else if (rdtype == STCKCOLOR) {
        // Stack pixels on top of the pixel value and combine flags
        if (*dst_ptr & 0x100)
          *dst_ptr = pixval | palette | 0x100 | comb | ((*dst_ptr) & 0xFFFF0000);
        else
          *dst_ptr = pixval | palette | 0x100 | comb | ((*dst_ptr) << 16);  // Stack pixels
        // TODO partial alpha
      }
      else if (rdtype == PIXCOPY)
        *dst_ptr = dst_ptr[240];
    }

    // Move to the next pixel, update coords accordingly
    cnt--;
    dst_ptr++;
    source_x += dx;
    if (rotate)
      source_y += dy;
  }
}

// Renders objects on a scanline for a given priority.
template <typename stype, rendtype rdtype, conditional_render_function copyfn>
static void render_scanline_objects(
  u32 start, u32 end, stype *scanline, u32 priority
) {
  // TODO move this to another place?
  // Skip alpha pass if you can do a regular color32 pass
  if (rdtype == STCKCOLOR && ((read_ioreg(REG_BLDCNT) >> 4) & 1) == 0) {
    // We cannot skip if there's some object being rendered TODO TODO
    render_scanline_objects<stype, INDXCOLOR, copyfn>(start, end, scanline, priority);
    return;
  }

  s32 vcount = read_ioreg(REG_VCOUNT);
  bool obj1dmap = read_ioreg(REG_DISPCNT) & 0x40;
  u32 objn;
  u32 objcnt = obj_priority_count[priority][vcount];
  u8 *objlist = obj_priority_list[priority][vcount];

  // Render all the visible objects for this priority.
  for (objn = 0; objn < objcnt; objn++) {
    // Objects in the list are pre-filtered and sorted in the appropriate order
    u32 objoff = objlist[objn];
    const t_oam *oamentry = &((t_oam*)oam_ram)[objoff];

    u16 obj_attr0 = eswap16(oamentry->attr0);
    u16 obj_attr1 = eswap16(oamentry->attr1);
    u16 obj_attr2 = eswap16(oamentry->attr2);
    u16 obj_shape = obj_attr0 >> 14;
    u16 obj_size = (obj_attr1 >> 14);
    bool is_affine = obj_attr0 & 0x100;
    bool is_8bpp = obj_attr0 & 0x2000;
    bool is_double = obj_attr0 & 0x200;

    t_sprite obji = {
      .obj_x = (s32)(obj_attr1 << 23) >> 23,
      .obj_y = obj_attr0 & 0xFF,
      .obj_w = obj_dim_table[obj_shape][obj_size][0],
      .obj_h = obj_dim_table[obj_shape][obj_size][1]
    };

    s32 obj_maxw = (is_affine && is_double) ? obji.obj_w * 2 : obji.obj_w;

    // The object could be out of the window, check and skip.
    if (obji.obj_x >= (signed)end || obji.obj_x + obj_maxw <= (signed)start)
      continue;

    const u8 *base_tile = &vram[
      0x10000 +                      // VRAM base for OBJ tile data
      (obj_attr2 & 0x3FF) * 32];     // Selected character block

    if (obji.obj_y > 160)
      obji.obj_y -= 256;

    // In PIXCOPY mode, we have already some stuff rendered (winout) and now
    // we render the "win-in" area for this object. The PIXCOPY function will
    // copy (merge) the two pixels depending on the result of the sprite render
    // The temporary buffer is rendered on the next scanline area.
    if (copyfn) {
      u32 sec_start = MAX((signed)start, obji.obj_x);
      u32 sec_end   = MIN((signed)end, obji.obj_x + obj_maxw);
      u32 obj_enable = read_ioreg(REG_WINOUT) >> 8;
      u16 *tmp_ptr = (u16*)&scanline[GBA_SCREEN_PITCH];
      copyfn(sec_start, sec_end, tmp_ptr, obj_enable);
    }

    if (is_affine) {
      u32 pnum = (obj_attr1 >> 9) & 0x1f;
      const t_affp *affp_base = (t_affp*)oam_ram;
      const t_affp *affp = &affp_base[pnum];
      u16 palette = (obj_attr2 >> 8) & 0xF0;

      if (affp->dy == 0) {   // No rotation happening (just scale)
        if (is_8bpp)
          render_affine_object<stype, rdtype, true, false>(&obji, affp, is_double, start, end, scanline, base_tile, 0);
        else
          render_affine_object<stype, rdtype, false, false>(&obji, affp, is_double, start, end, scanline, base_tile, palette);
      } else {               // Full rotation and scaling
        if (is_8bpp)
          render_affine_object<stype, rdtype, true, true>(&obji, affp, is_double, start, end, scanline, base_tile, 0);
        else
          render_affine_object<stype, rdtype, false, true>(&obji, affp, is_double, start, end, scanline, base_tile, palette);
      }
    } else {
      // The object could be out of the window, check and skip.
      if (obji.obj_x >= (signed)end || obji.obj_x + obji.obj_w <= (signed)start)
        continue;

      // Non-affine objects can be flipped on both edges.
      bool hflip = obj_attr1 & 0x1000;
      bool vflip = obj_attr1 & 0x2000;

      // Calulate the vertical offset (row) to be displayed. Account for vflip.
      u32 voffset = vflip ? obji.obj_y + obji.obj_h - vcount - 1 : vcount - obji.obj_y;

      // Calculate base tile for the object (points to the row to be drawn).
      u32 tile_bsize  = is_8bpp ? tile_size_8bpp : tile_size_4bpp;
      u32 tile_bwidth = is_8bpp ? tile_width_8bpp : tile_width_4bpp;
      u32 obj_pitch = obj1dmap ? (obji.obj_w / 8) * tile_bsize : 1024;
      u32 hflip_off = hflip ? ((obji.obj_w / 8) - 1) * tile_bsize : 0;

      // Calculate the pointer to the tile.
      const u8 *tile_ptr = &base_tile[
        (voffset / 8) * obj_pitch +    // Select tile row offset
        (voffset % 8) * tile_bwidth +  // Skip tile rows
        hflip_off];                     // Account for horizontal flip

      // Make everything relative to start
      s32 obj_x_offset  = obji.obj_x - start;
      u32 clipped_width = obj_x_offset >= 0 ? obji.obj_w : obji.obj_w + obj_x_offset;
      u32 max_range = obj_x_offset >= 0 ? end - obji.obj_x : end - start;
      u32 max_draw = MIN(max_range, clipped_width);

      // Render the object scanline using the correct mode.
      if (is_8bpp) {
        if (hflip)
          render_object<stype, rdtype, true, true>(obj_x_offset, max_draw, &scanline[start], tile_ptr, 0);
        else
          render_object<stype, rdtype, true, false>(obj_x_offset, max_draw, &scanline[start], tile_ptr, 0);
      } else {
        // In 4bpp mode calculate the palette number
        u16 palette = (obj_attr2 >> 8) & 0xF0;

        if (hflip)
          render_object<stype, rdtype, false, true>(obj_x_offset, max_draw, &scanline[start], tile_ptr, palette);
        else
          render_object<stype, rdtype, false, false>(obj_x_offset, max_draw, &scanline[start], tile_ptr, palette);
      }
    }
  }
}



// Adjust a flipped obj's starting position

#define obj_tile_offset_noflip(color_depth)                                   \

#define obj_tile_offset_flip(color_depth)                                     \
  + (tile_size_##color_depth * ((obj_width - 8) / 8))                         \


// Adjust the obj's starting point if it goes too far off the left edge of
// the screen.

#define obj_tile_right_offset_noflip(color_depth)                             \
  tile_ptr += (partial_tile_offset / 8) * tile_size_##color_depth             \

#define obj_tile_right_offset_flip(color_depth)                               \
  tile_ptr -= (partial_tile_offset / 8) * tile_size_##color_depth             \

// Get the current row offset into an obj in 1D map space

#define obj_tile_offset_1D(color_depth, flip_op)                              \
  tile_ptr = tile_base + ((obj_attribute_2 & 0x3FF) * 32)                     \
   + ((vertical_offset / 8) * (obj_width / 8) * tile_size_##color_depth)      \
   + ((vertical_offset % 8) * tile_width_##color_depth)                       \
   obj_tile_offset_##flip_op(color_depth)                                     \

// Get the current row offset into an obj in 2D map space

#define obj_tile_offset_2D(color_depth, flip_op)                              \
  tile_ptr = tile_base + ((obj_attribute_2 & 0x3FF) * 32)                     \
   + ((vertical_offset / 8) * 1024)                                           \
   + ((vertical_offset % 8) * tile_width_##color_depth)                       \
   obj_tile_offset_##flip_op(color_depth)                                     \


// Get the palette for 4bpp obj.

#define obj_get_palette_4bpp()                                                \
  current_palette = (obj_attribute_2 >> 8) & 0xF0                             \

#define obj_get_palette_8bpp()                                                \


// Render the current row of an obj.

#define obj_render(combine_op, color_depth, alpha_op, map_space, flip_op)     \
{                                                                             \
  obj_get_palette_##color_depth();                                            \
  obj_tile_offset_##map_space(color_depth, flip_op);                          \
                                                                              \
  if(obj_x < (s32)start)                                                      \
  {                                                                           \
    dest_ptr = scanline + start;                                              \
    pixel_run = obj_width - (start - obj_x);                                  \
    if((s32)pixel_run > 0)                                                    \
    {                                                                         \
      if((obj_x + obj_width) >= end)                                          \
      {                                                                       \
        pixel_run = end - start;                                              \
        partial_tile_offset = start - obj_x;                                  \
        obj_tile_right_offset_##flip_op(color_depth);                         \
        partial_tile_offset %= 8;                                             \
                                                                              \
        if(partial_tile_offset)                                               \
        {                                                                     \
          partial_tile_run = 8 - partial_tile_offset;                         \
          if((s32)pixel_run < (s32)partial_tile_run)                          \
          {                                                                   \
            if((s32)pixel_run > 0)                                            \
            {                                                                 \
              partial_tile_run = pixel_run;                                   \
              partial_tile_mid_obj(combine_op, color_depth, alpha_op,         \
               flip_op);                                                      \
            }                                                                 \
            continue;                                                         \
          }                                                                   \
          else                                                                \
          {                                                                   \
            pixel_run -= partial_tile_run;                                    \
            partial_tile_right_obj(combine_op, color_depth, alpha_op,         \
             flip_op);                                                        \
          }                                                                   \
        }                                                                     \
        tile_run = pixel_run / 8;                                             \
        multiple_tile_obj(combine_op, color_depth, alpha_op, flip_op);        \
        partial_tile_run = pixel_run % 8;                                     \
        if(partial_tile_run)                                                  \
        {                                                                     \
          partial_tile_left_obj(combine_op, color_depth, alpha_op,            \
           flip_op);                                                          \
        }                                                                     \
      }                                                                       \
      else                                                                    \
      {                                                                       \
        partial_tile_offset = start - obj_x;                                  \
        obj_tile_right_offset_##flip_op(color_depth);                         \
        partial_tile_offset %= 8;                                             \
        if(partial_tile_offset)                                               \
        {                                                                     \
          partial_tile_run = 8 - partial_tile_offset;                         \
          partial_tile_right_obj(combine_op, color_depth, alpha_op,           \
           flip_op);                                                          \
        }                                                                     \
        tile_run = pixel_run / 8;                                             \
        multiple_tile_obj(combine_op, color_depth, alpha_op, flip_op);        \
      }                                                                       \
    }                                                                         \
  }                                                                           \
  else                                                                        \
                                                                              \
  if((obj_x + obj_width) >= end)                                              \
  {                                                                           \
    pixel_run = end - obj_x;                                                  \
    if((s32)pixel_run > 0)                                                    \
    {                                                                         \
      dest_ptr = scanline + obj_x;                                            \
      tile_run = pixel_run / 8;                                               \
      multiple_tile_obj(combine_op, color_depth, alpha_op, flip_op);          \
      partial_tile_run = pixel_run % 8;                                       \
      if(partial_tile_run)                                                    \
      {                                                                       \
        partial_tile_left_obj(combine_op, color_depth, alpha_op, flip_op);    \
      }                                                                       \
    }                                                                         \
  }                                                                           \
  else                                                                        \
  {                                                                           \
    dest_ptr = scanline + obj_x;                                              \
    tile_run = obj_width / 8;                                                 \
    multiple_tile_obj(combine_op, color_depth, alpha_op, flip_op);            \
  }                                                                           \
}                                                                             \

#define obj_scale_offset_1D(color_depth)                                      \
  tile_ptr = tile_base + ((obj_attribute_2 & 0x3FF) * 32)                     \
   + ((vertical_offset / 8) * (max_x / 8) * tile_size_##color_depth)          \
   + ((vertical_offset % 8) * tile_width_##color_depth)                       \

// Get the current row offset into an obj in 2D map space

#define obj_scale_offset_2D(color_depth)                                      \
  tile_ptr = tile_base + ((obj_attribute_2 & 0x3FF) * 32)                     \
   + ((vertical_offset / 8) * 1024)                                           \
   + ((vertical_offset % 8) * tile_width_##color_depth)                       \

#define obj_render_scale_pixel_4bpp(combine_op, alpha_op)                     \
  current_pixel =                                                             \
     tile_ptr[tile_map_offset + ((tile_x >> 1) & 0x03)];                      \
  if(tile_x & 0x01)                                                           \
    current_pixel >>= 4;                                                      \
  else                                                                        \
    current_pixel &= 0x0F;                                                    \
                                                                              \
  tile_4bpp_draw_##combine_op(0, none, 0, alpha_op)                           \


#define obj_render_scale_pixel_8bpp(combine_op, alpha_op)                     \
  current_pixel = tile_ptr[tile_map_offset + (tile_x & 0x07)];                \
  tile_8bpp_draw_##combine_op(0, none, 0, alpha_op);                          \

#define obj_render_scale(combine_op, color_depth, alpha_op, map_space)        \
{                                                                             \
  u32 vertical_offset;                                                        \
  source_y += (y_delta * dmy);                                                \
  vertical_offset = (source_y >> 8);                                          \
  if((u32)vertical_offset < (u32)max_y)                                       \
  {                                                                           \
    obj_scale_offset_##map_space(color_depth);                                \
    source_x += (y_delta * dmx) - (middle_x * dx);                            \
                                                                              \
    for(i = 0; i < obj_width; i++)                                            \
    {                                                                         \
      tile_x = (source_x >> 8);                                               \
                                                                              \
      if((u32)tile_x < (u32)max_x)                                            \
        break;                                                                \
                                                                              \
      source_x += dx;                                                         \
      advance_dest_ptr_##combine_op(1);                                       \
    }                                                                         \
                                                                              \
    for(; i < obj_width; i++)                                                 \
    {                                                                         \
      tile_x = (source_x >> 8);                                               \
                                                                              \
      if((u32)tile_x >= (u32)max_x)                                           \
        break;                                                                \
                                                                              \
      tile_map_offset = (tile_x >> 3) * tile_size_##color_depth;              \
      obj_render_scale_pixel_##color_depth(combine_op, alpha_op);             \
                                                                              \
      source_x += dx;                                                         \
      advance_dest_ptr_##combine_op(1);                                       \
    }                                                                         \
  }                                                                           \
}                                                                             \


#define obj_rotate_offset_1D(color_depth)                                     \
  obj_tile_pitch = (max_x / 8) * tile_size_##color_depth                      \

#define obj_rotate_offset_2D(color_depth)                                     \
  obj_tile_pitch = 1024                                                       \

#define obj_render_rotate_pixel_4bpp(combine_op, alpha_op)                    \
   current_pixel = tile_ptr[tile_map_offset +                                 \
((tile_x >> 1) & 0x03) + ((tile_y & 0x07) * obj_pitch)];                      \
  if(tile_x & 0x01)                                                           \
    current_pixel >>= 4;                                                      \
  else                                                                        \
    current_pixel &= 0x0F;                                                    \
                                                                              \
  tile_4bpp_draw_##combine_op(0, none, 0, alpha_op)                           \

#define obj_render_rotate_pixel_8bpp(combine_op, alpha_op)                    \
  current_pixel = tile_ptr[tile_map_offset +                                  \
   (tile_x & 0x07) + ((tile_y & 0x07) * obj_pitch)];                          \
                                                                              \
  tile_8bpp_draw_##combine_op(0, none, 0, alpha_op)                           \

#define obj_render_rotate(combine_op, color_depth, alpha_op, map_space)       \
{                                                                             \
  tile_ptr = tile_base + ((obj_attribute_2 & 0x3FF) * 32);                    \
  obj_rotate_offset_##map_space(color_depth);                                 \
                                                                              \
  source_x += (y_delta * dmx) - (middle_x * dx);                              \
  source_y += (y_delta * dmy) - (middle_x * dy);                              \
                                                                              \
  for(i = 0; i < obj_width; i++)                                              \
  {                                                                           \
    tile_x = (source_x >> 8);                                                 \
    tile_y = (source_y >> 8);                                                 \
                                                                              \
    if(((u32)tile_x < (u32)max_x) && ((u32)tile_y < (u32)max_y))              \
      break;                                                                  \
                                                                              \
    source_x += dx;                                                           \
    source_y += dy;                                                           \
    advance_dest_ptr_##combine_op(1);                                         \
  }                                                                           \
                                                                              \
  for(; i < obj_width; i++)                                                   \
  {                                                                           \
    tile_x = (source_x >> 8);                                                 \
    tile_y = (source_y >> 8);                                                 \
                                                                              \
    if(((u32)tile_x >= (u32)max_x) || ((u32)tile_y >= (u32)max_y))            \
      break;                                                                  \
                                                                              \
    tile_map_offset = ((tile_x >> 3) * tile_size_##color_depth) +             \
    ((tile_y >> 3) * obj_tile_pitch);                                         \
    obj_render_rotate_pixel_##color_depth(combine_op, alpha_op);              \
                                                                              \
    source_x += dx;                                                           \
    source_y += dy;                                                           \
    advance_dest_ptr_##combine_op(1);                                         \
  }                                                                           \
}                                                                             \

// Render the current row of an affine transformed OBJ.

#define obj_render_affine(combine_op, color_depth, alpha_op, map_space)       \
{                                                                             \
  u16 *params = (u16 *)oam_ram + (((obj_attribute_1 >> 9) & 0x1F) * 16);      \
  s32 dx = (s16)eswap16(params[3]);                                           \
  s32 dmx = (s16)eswap16(params[7]);                                          \
  s32 dy = (s16)eswap16(params[11]);                                          \
  s32 dmy = (s16)eswap16(params[15]);                                         \
  s32 source_x, source_y;                                                     \
  s32 tile_x, tile_y;                                                         \
  u32 tile_map_offset;                                                        \
  s32 middle_x;                                                               \
  s32 middle_y;                                                               \
  s32 max_x = obj_width;                                                      \
  s32 max_y = obj_height;                                                     \
  s32 y_delta;                                                                \
  u32 obj_pitch = tile_width_##color_depth;                                   \
  u32 obj_tile_pitch;                                                         \
                                                                              \
  middle_x = (obj_width / 2);                                                 \
  middle_y = (obj_height / 2);                                                \
                                                                              \
  source_x = (middle_x << 8);                                                 \
  source_y = (middle_y << 8);                                                 \
                                                                              \
                                                                              \
  if(obj_attribute_0 & 0x200)                                                 \
  {                                                                           \
    obj_width *= 2;                                                           \
    obj_height *= 2;                                                          \
    middle_x *= 2;                                                            \
    middle_y *= 2;                                                            \
  }                                                                           \
                                                                              \
  if((s32)obj_x < (s32)start)                                                 \
  {                                                                           \
    u32 x_delta = start - obj_x;                                              \
    middle_x -= x_delta;                                                      \
    obj_width -= x_delta;                                                     \
    obj_x = start;                                                            \
                                                                              \
    if((s32)obj_width <= 0)                                                   \
      continue;                                                               \
  }                                                                           \
                                                                              \
  if((s32)(obj_x + obj_width) >= (s32)end)                                    \
  {                                                                           \
    obj_width = end - obj_x;                                                  \
                                                                              \
    if((s32)obj_width <= 0)                                                   \
      continue;                                                               \
  }                                                                           \
  dest_ptr = scanline + obj_x;                                                \
                                                                              \
  y_delta = vcount - (obj_y + middle_y);                                      \
                                                                              \
  obj_get_palette_##color_depth();                                            \
                                                                              \
  if(dy == 0)                                                                 \
  {                                                                           \
    obj_render_scale(combine_op, color_depth, alpha_op, map_space);           \
  }                                                                           \
  else                                                                        \
  {                                                                           \
    obj_render_rotate(combine_op, color_depth, alpha_op, map_space);          \
  }                                                                           \
}                                                                             \


// Build obj rendering functions


#define render_scanline_obj_extra_variables_normal(bg_type)                   \
  u16 *palette = palette_ram_converted + 256                                  \

#define render_scanline_obj_extra_variables_color()                           \
  u32 pixel_combine = color_combine_mask(4) | (1 << 8)                        \

#define render_scanline_obj_extra_variables_alpha_obj(map_space)              \
  render_scanline_obj_extra_variables_color();                                \
  u32 dest;                                                                   \
  if((pixel_combine & 0x00000200) == 0)                                       \
  {                                                                           \
    render_scanline_obj_color32_##map_space(priority, start, end, scanline);  \
    return;                                                                   \
  }                                                                           \

#define render_scanline_obj_extra_variables_partial_alpha(map_space)          \
  render_scanline_obj_extra_variables_color();                                \
  u32 base_pixel_combine = pixel_combine;                                     \
  u32 dest                                                                    \


#define render_scanline_obj_main(combine_op, alpha_op, map_space)             \
  if(obj_attribute_0 & 0x100)                                                 \
  {                                                                           \
    if((obj_attribute_0 >> 13) & 0x01)                                        \
    {                                                                         \
      obj_render_affine(combine_op, 8bpp, alpha_op, map_space);               \
    }                                                                         \
    else                                                                      \
    {                                                                         \
      obj_render_affine(combine_op, 4bpp, alpha_op, map_space);               \
    }                                                                         \
  }                                                                           \
  else                                                                        \
  {                                                                           \
    vertical_offset = vcount - obj_y;                                         \
                                                                              \
    if((obj_attribute_1 >> 13) & 0x01)                                        \
      vertical_offset = obj_height - vertical_offset - 1;                     \
                                                                              \
    switch(((obj_attribute_0 >> 12) & 0x02) |                                 \
     ((obj_attribute_1 >> 12) & 0x01))                                        \
    {                                                                         \
      case 0x0:                                                               \
        obj_render(combine_op, 4bpp, alpha_op, map_space, noflip);            \
        break;                                                                \
                                                                              \
      case 0x1:                                                               \
        obj_render(combine_op, 4bpp, alpha_op, map_space, flip);              \
        break;                                                                \
                                                                              \
      case 0x2:                                                               \
        obj_render(combine_op, 8bpp, alpha_op, map_space, noflip);            \
        break;                                                                \
                                                                              \
      case 0x3:                                                               \
        obj_render(combine_op, 8bpp, alpha_op, map_space, flip);              \
        break;                                                                \
    }                                                                         \
  }                                                                           \

#define render_scanline_obj_no_partial_alpha(combine_op, alpha_op, map_space) \
  render_scanline_obj_main(combine_op, alpha_op, map_space)                   \

#define render_scanline_obj_partial_alpha(combine_op, alpha_op, map_space)    \
  if((obj_attribute_0 >> 10) & 0x03)                                          \
  {                                                                           \
    pixel_combine = 0x00000300;    /* 1st target and palette 256 */           \
    render_scanline_obj_main(combine_op, alpha_obj, map_space);               \
  }                                                                           \
  else                                                                        \
  {                                                                           \
    pixel_combine = base_pixel_combine;                                       \
    render_scanline_obj_main(combine_op, color32, map_space);                 \
  }                                                                           \

#define render_scanline_obj_prologue_transparent(alpha_op)                    \

#define render_scanline_obj_builder(combine_op, alpha_op, map_space,          \
 partial_alpha_op)                                                            \
static void render_scanline_obj_##alpha_op##_##map_space(u32 priority,        \
 u32 start, u32 end, void *raw_dst)                                           \
{                                                                             \
  render_scanline_dest_##alpha_op *scanline =                                 \
                                  (render_scanline_dest_##alpha_op*)raw_dst;  \
  render_scanline_obj_extra_variables_##alpha_op(map_space);                  \
  u32 obj_num, i;                                                             \
  s32 obj_x, obj_y;                                                           \
  u32 obj_size;                                                               \
  u32 obj_width, obj_height;                                                  \
  u32 obj_attribute_0, obj_attribute_1, obj_attribute_2;                      \
  s32 vcount = read_ioreg(REG_VCOUNT);                                        \
  u32 tile_run;                                                               \
  u32 current_pixels;                                                         \
  u32 current_pixel;                                                          \
  u32 current_palette;                                                        \
  u32 vertical_offset;                                                        \
  u32 partial_tile_run, partial_tile_offset;                                  \
  u32 pixel_run;                                                              \
  u16 *oam_ptr;                                                               \
  render_scanline_dest_##alpha_op *dest_ptr;                                  \
  u8 *tile_base = vram + 0x10000;                                             \
  u8 *tile_ptr;                                                               \
  u32 obj_count = obj_priority_count[priority][vcount];                       \
  u8 *obj_list = obj_priority_list[priority][vcount];                         \
                                                                              \
  for(obj_num = 0; obj_num < obj_count; obj_num++)                            \
  {                                                                           \
    oam_ptr = oam_ram + (obj_list[obj_num] * 4);                              \
    obj_attribute_0 = eswap16(oam_ptr[0]);                                    \
    obj_attribute_1 = eswap16(oam_ptr[1]);                                    \
    obj_attribute_2 = eswap16(oam_ptr[2]);                                    \
    obj_size = ((obj_attribute_0 >> 12) & 0x0C) | (obj_attribute_1 >> 14);    \
                                                                              \
    obj_x = (s32)(obj_attribute_1 << 23) >> 23;                               \
    obj_width = obj_width_table[obj_size];                                    \
                                                                              \
    render_scanline_obj_prologue_##combine_op(alpha_op);                      \
                                                                              \
    obj_y = obj_attribute_0 & 0xFF;                                           \
                                                                              \
    if(obj_y > 160)                                                           \
      obj_y -= 256;                                                           \
                                                                              \
    obj_height = obj_height_table[obj_size];                                  \
    render_scanline_obj_##partial_alpha_op(combine_op, alpha_op, map_space);  \
  }                                                                           \
}                                                                             \

// There are actually used to render sprites to the scanline
// render_scanline_obj_builder(transparent, normal, 1D, no_partial_alpha);
// render_scanline_obj_builder(transparent, normal, 2D, no_partial_alpha);
// render_scanline_obj_builder(transparent, color16, 1D, no_partial_alpha);
// render_scanline_obj_builder(transparent, color16, 2D, no_partial_alpha);
// render_scanline_obj_builder(transparent, color32, 1D, no_partial_alpha);
// render_scanline_obj_builder(transparent, color32, 2D, no_partial_alpha);
// render_scanline_obj_builder(transparent, alpha_obj, 1D, no_partial_alpha);
// render_scanline_obj_builder(transparent, alpha_obj, 2D, no_partial_alpha);

// WIP: Remove these once we merge things with partial alpha and copy mode.
void render_scanline_obj_normal_1D(u32 priority,  u32 start, u32 end, void *raw_dst) {
  render_scanline_objects<u16, FULLCOLOR, nullptr>(start, end, (u16*)raw_dst, priority);
}
void render_scanline_obj_normal_2D(u32 priority,  u32 start, u32 end, void *raw_dst) {
  render_scanline_objects<u16, FULLCOLOR, nullptr>(start, end, (u16*)raw_dst, priority);
}

void render_scanline_obj_color16_1D(u32 priority,  u32 start, u32 end, void *raw_dst) {
  render_scanline_objects<u16, INDXCOLOR, nullptr>(start, end, (u16*)raw_dst, priority);
}
void render_scanline_obj_color16_2D(u32 priority,  u32 start, u32 end, void *raw_dst) {
  render_scanline_objects<u16, INDXCOLOR, nullptr>(start, end, (u16*)raw_dst, priority);
}

void render_scanline_obj_color32_1D(u32 priority,  u32 start, u32 end, void *raw_dst) {
  render_scanline_objects<u32, INDXCOLOR, nullptr>(start, end, (u32*)raw_dst, priority);
}
void render_scanline_obj_color32_2D(u32 priority,  u32 start, u32 end, void *raw_dst) {
  render_scanline_objects<u32, INDXCOLOR, nullptr>(start, end, (u32*)raw_dst, priority);
}

void render_scanline_obj_alpha_obj_1D(u32 priority,  u32 start, u32 end, void *raw_dst) {
  render_scanline_objects<u32, STCKCOLOR, nullptr>(start, end, (u32*)raw_dst, priority);
}
void render_scanline_obj_alpha_obj_2D(u32 priority,  u32 start, u32 end, void *raw_dst) {
  render_scanline_objects<u32, STCKCOLOR, nullptr>(start, end, (u32*)raw_dst, priority);
}

render_scanline_obj_builder(transparent, partial_alpha, 1D, partial_alpha);
render_scanline_obj_builder(transparent, partial_alpha, 2D, partial_alpha);

static const tile_render_function obj_mode_renderers[5][2] = {
  { render_scanline_obj_normal_2D,        render_scanline_obj_normal_1D        },
  { render_scanline_obj_color16_2D,       render_scanline_obj_color16_1D       },
  { render_scanline_obj_color32_2D,       render_scanline_obj_color32_1D       },
  { render_scanline_obj_alpha_obj_2D,     render_scanline_obj_alpha_obj_1D     },
  { render_scanline_obj_partial_alpha_2D, render_scanline_obj_partial_alpha_1D },
};

// These are used for winobj rendering
// render_scanline_obj_builder(copy, copy_tile, 1D, no_partial_alpha);
// render_scanline_obj_builder(copy, copy_tile, 2D, no_partial_alpha);
// render_scanline_obj_builder(copy, copy_bitmap, 1D, no_partial_alpha);
// render_scanline_obj_builder(copy, copy_bitmap, 2D, no_partial_alpha);

#define OBJ_MOD_NORMAL     0
#define OBJ_MOD_SEMITRAN   1
#define OBJ_MOD_WINDOW     2
#define OBJ_MOD_INVALID    3

// Goes through the object list in the OAM (from #127 to #0) and adds objects
// into a sorted list by priority for the current row.
// Invisible objects are discarded.
static void order_obj(u32 video_mode)
{
  s32 obj_num;
  u32 row;
  t_oam *oam_base = (t_oam*)oam_ram;

  memset(obj_priority_count, 0, sizeof(obj_priority_count));
  memset(obj_alpha_count, 0, sizeof(obj_alpha_count));

  for(obj_num = 127; obj_num >= 0; obj_num--)
  {
    t_oam *oam_ptr = &oam_base[obj_num];
    u16 obj_attr0 = eswap16(oam_ptr->attr0);
    // Bit 9 disables regular sprites. Used as double bit for affine ones.
    bool visible = (obj_attr0 & 0x0300) != 0x0200;
    if (visible) {
      u16 obj_shape = obj_attr0 >> 14;
      u32 obj_mode = (obj_attr0 >> 10) & 0x03;

      // Prohibited shape and mode
      bool invalid = (obj_shape == 0x3) || (obj_mode == OBJ_MOD_INVALID);
      if (!invalid) {
        u16 obj_attr1 = eswap16(oam_ptr->attr1);
        u16 obj_attr2 = eswap16(oam_ptr->attr2);
        u32 obj_priority = (obj_attr2 >> 10) & 0x03;

        if (((video_mode < 3) || ((obj_attr2 & 0x3FF) >= 512)))
        {
          // Calculate object size (from size and shape attr bits)
          u16 obj_size = (obj_attr1 >> 14);
          s32 obj_height = obj_dim_table[obj_shape][obj_size][1];
          s32 obj_width  = obj_dim_table[obj_shape][obj_size][0];
          s32 obj_y = obj_attr0 & 0xFF;

          if(obj_y > 160)
            obj_y -= 256;

          // Double size for affine sprites with double bit set
          if(obj_attr0 & 0x200)
          {
            obj_height *= 2;
            obj_width *= 2;
          }

          if(((obj_y + obj_height) > 0) && (obj_y < 160))
          {
            s32 obj_x = (s32)(obj_attr1 << 23) >> 23;

            if(((obj_x + obj_width) > 0) && (obj_x < 240))
            {
              // Clip Y coord and height to the 0..159 interval
              u32 starty = MAX(obj_y, 0);
              u32 endy   = MIN(obj_y + obj_height, 160);

              switch (obj_mode) {
              case OBJ_MOD_SEMITRAN:
                for(row = starty; row < endy; row++)
                {
                  u32 cur_cnt = obj_priority_count[obj_priority][row];
                  obj_priority_list[obj_priority][row][cur_cnt] = obj_num;
                  obj_priority_count[obj_priority][row] = cur_cnt + 1;
                  // Mark the row as having semi-transparent objects
                  obj_alpha_count[row] = 1;
                }
                break;
              case OBJ_MOD_WINDOW:
                obj_priority = 4;
                /* fallthrough */
              case OBJ_MOD_NORMAL:
                // Add the object to the list.
                for(row = starty; row < endy; row++)
                {
                  u32 cur_cnt = obj_priority_count[obj_priority][row];
                  obj_priority_list[obj_priority][row][cur_cnt] = obj_num;
                  obj_priority_count[obj_priority][row] = cur_cnt + 1;
                }
                break;
              };
            }
          }
        }
      }
    }
  }
}

u32 layer_order[16];
u32 layer_count;

// Sorts active BG/OBJ layers and generates an ordered list of layers.
// Things are drawn back to front, so lowest priority goes first.
static void order_layers(u32 layer_flags, u32 vcnt)
{
  bool obj_enabled = (layer_flags & 0x10);
  s32 priority;

  layer_count = 0;

  for(priority = 3; priority >= 0; priority--)
  {
    bool anyobj = obj_priority_count[priority][vcnt] > 0;
    s32 lnum;

    for(lnum = 3; lnum >= 0; lnum--)
    {
      if(((layer_flags >> lnum) & 1) &&
         ((read_ioreg(REG_BGxCNT(lnum)) & 0x03) == priority))
      {
        layer_order[layer_count++] = lnum;
      }
    }

    if(obj_enabled && anyobj)
      layer_order[layer_count++] = priority | 0x04;
  }
}


// Blending is performed by separating an RGB value into 0G0R0B (32 bit)
// Since blending factors are at most 16, mult/add operations do not overflow
// to the neighbouring color and can be performed much faster than separatedly

// Here follow the mask value to separate/expand the color to 32 bit,
// the mask to detect overflows in the blend operation and

#define BLND_MSK (SATR_MSK | SATG_MSK | SATB_MSK)

#ifdef USE_XBGR1555_FORMAT
  #define OVFG_MSK 0x04000000
  #define OVFR_MSK 0x00008000
  #define OVFB_MSK 0x00000020
  #define SATG_MSK 0x03E00000
  #define SATR_MSK 0x00007C00
  #define SATB_MSK 0x0000001F
#else
  #define OVFG_MSK 0x08000000
  #define OVFR_MSK 0x00010000
  #define OVFB_MSK 0x00000020
  #define SATG_MSK 0x07E00000
  #define SATR_MSK 0x0000F800
  #define SATB_MSK 0x0000001F
#endif

typedef enum
{
  BLEND_ONLY,   // Just alpha blending (if the pixels are 1st and 2nd target)
  BLEND_BRIGHT, // Perform alpha blending if appropiate, and brighten otherwise
  BLEND_DARK,   // Same but with darken effecg
} blendtype;

// Applies blending (and optional brighten/darken) effect to a bunch of
// color-indexed pixel pairs. Depending on the mode and the pixel target
// number, blending, darken/brighten or no effect will be applied.
template <blendtype bldtype>
static void merge_blend(u32 start, u32 end, u16 *dst, u32 *src) {
  u32 bldalpha = read_ioreg(REG_BLDALPHA);
  u32 brightf = MIN(16, read_ioreg(REG_BLDY) & 0x1F);
  u32 blend_a = MIN(16, (bldalpha >> 0) & 0x1F);
  u32 blend_b = MIN(16, (bldalpha >> 8) & 0x1F);

  bool can_saturate = blend_a + blend_b > 16;

  if (can_saturate) {
    // If blending can result in saturation, we need to clamp output values.
    while (start < end) {
      u32 pixpair = src[start];
      if ((pixpair & 0x04000200) == 0x04000200) {
        // Top pixel is 1st target, pixel below is 2nd target. Blend!
        u16 p1 = palette_ram_converted[(pixpair >>  0) & 0x1FF];
        u16 p2 = palette_ram_converted[(pixpair >> 16) & 0x1FF];
        u32 p1e = (p1 | (p1 << 16)) & BLND_MSK;
        u32 p2e = (p2 | (p2 << 16)) & BLND_MSK;
        u32 pfe = (((p1e * blend_a) + (p2e * blend_b)) >> 4);

        // If the overflow bit is set, saturate (set) all bits to one.
        if (pfe & (OVFR_MSK | OVFG_MSK | OVFB_MSK)) {
          if (pfe & OVFG_MSK)
            pfe |= SATG_MSK;
          if (pfe & OVFR_MSK)
            pfe |= SATR_MSK;
          if (pfe & OVFB_MSK)
            pfe |= SATB_MSK;
        }
        pfe &= BLND_MSK;
        dst[start++] = (pfe >> 16) | pfe;
      }
      else if ((bldtype != BLEND_ONLY) && (pixpair & 0x200) == 0x200) {
        // Top pixel is 1st-target, can still apply bright/dark effect.
        u16 pidx = palette_ram_converted[pixpair & 0x1FF];
        u32 epixel = (pidx | (pidx << 16)) & BLND_MSK;
        u32 pa = bldtype == BLEND_DARK ? 0 : ((BLND_MSK * brightf) >> 4) & BLND_MSK;
        u32 pb = ((epixel * (16 - brightf)) >> 4) & BLND_MSK;
        epixel = (pa + pb) & BLND_MSK;
        dst[start++] = (epixel >> 16) | epixel;
      }
      else {
        dst[start++] = palette_ram_converted[pixpair & 0x1FF];   // No effects
      }
    }
  } else {
    while (start < end) {
      u32 pixpair = src[start];
      if ((pixpair & 0x04000200) == 0x04000200) {
        // Top pixel is 1st target, pixel below is 2nd target. Blend!
        u16 p1 = palette_ram_converted[(pixpair >>  0) & 0x1FF];
        u16 p2 = palette_ram_converted[(pixpair >> 16) & 0x1FF];
        u32 p1e = (p1 | (p1 << 16)) & BLND_MSK;
        u32 p2e = (p2 | (p2 << 16)) & BLND_MSK;
        u32 pfe = (((p1e * blend_a) + (p2e * blend_b)) >> 4) & BLND_MSK;
        dst[start++] = (pfe >> 16) | pfe;
      }
      else if ((bldtype != BLEND_ONLY) && (pixpair & 0x200) == 0x200) {
        // Top pixel is 1st-target, can still apply bright/dark effect.
        u16 pidx = palette_ram_converted[pixpair & 0x1FF];
        u32 epixel = (pidx | (pidx << 16)) & BLND_MSK;
        u32 pa = bldtype == BLEND_DARK ? 0 : ((BLND_MSK * brightf) >> 4) & BLND_MSK;
        u32 pb = ((epixel * (16 - brightf)) >> 4) & BLND_MSK;
        epixel = (pa + pb) & BLND_MSK;
        dst[start++] = (epixel >> 16) | epixel;
      }
      else {
        dst[start++] = palette_ram_converted[pixpair & 0x1FF];   // No effects
      }
    }
  }
}

// Applies brighten/darken effect to a bunch of color-indexed pixels.
template <blendtype bldtype>
static void merge_brightness(u32 start, u32 end, u16 *srcdst) {
  u32 brightness = MIN(16, read_ioreg(REG_BLDY) & 0x1F);

  while (start < end) {
    u16 spix = srcdst[start];
    u16 pixcol = palette_ram_converted[spix & 0x1FF];

    if ((spix & 0x200) == 0x200) {
      // Pixel is 1st target, can apply color effect.
      u32 epixel = (pixcol | (pixcol << 16)) & BLND_MSK;
      u32 pa = bldtype == BLEND_DARK ? 0 : ((BLND_MSK * brightness) >> 4) & BLND_MSK; // B/W
      u32 pb = ((epixel * (16 - brightness)) >> 4) & BLND_MSK;  // Pixel color
      epixel = (pa + pb) & BLND_MSK;
      pixcol = (epixel >> 16) | epixel;
    }

    srcdst[start++] = pixcol;
  }
}

// Render an OBJ layer from start to end, depending on the type (1D or 2D)
// stored in dispcnt.

#define render_obj_layer(type, dest, _start, _end)                            \
  current_layer &= ~0x04;                                                     \
  if(dispcnt & 0x40)                                                          \
    render_scanline_obj_##type##_1D(current_layer, _start, _end, dest);       \
  else                                                                        \
    render_scanline_obj_##type##_2D(current_layer, _start, _end, dest)        \


// Render a target all the way with the background color as taken from the
// palette.

template<bool indexed, typename dsttype>
void fill_line_background(u32 start, u32 end, void *scanline) {
  dsttype *ptr = (dsttype*)scanline;
  while (start < end)
    if (indexed)
      ptr[start++] = 0;
    else
      ptr[start++] = palette_ram_converted[0];
}

#define COL_EFFECT_NONE   0x0
#define COL_EFFECT_BLEND  0x1
#define COL_EFFECT_BRIGHT 0x2
#define COL_EFFECT_DARK   0x3

// Renders the backdrop color (ie. whenever no layer is active) applying
// any effects that might still apply (usually darken/brighten).
static void render_backdrop(u32 start, u32 end, u16 *scanline) {
  u16 bldcnt = read_ioreg(REG_BLDCNT);
  u16 pixcol = palette_ram_converted[0];
  u32 effect = (bldcnt >> 6) & 0x03;
  u32 bd_1st_target = ((bldcnt >> 0x5) & 0x01);

  if (bd_1st_target && effect == COL_EFFECT_BRIGHT) {
    u32 brightness = MIN(16, read_ioreg(REG_BLDY) & 0x1F);

    // Unpack 16 bit pixel for fast blending operation
    u32 epixel = (pixcol | (pixcol << 16)) & BLND_MSK;
    u32 pa = ((BLND_MSK * brightness)      >> 4) & BLND_MSK;  // White color
    u32 pb = ((epixel * (16 - brightness)) >> 4) & BLND_MSK;  // Pixel color
    epixel = (pa + pb) & BLND_MSK;
    pixcol = (epixel >> 16) | epixel;
  }
  else if (bd_1st_target && effect == COL_EFFECT_DARK) {
    u32 brightness = MIN(16, read_ioreg(REG_BLDY) & 0x1F);
    u32 epixel = (pixcol | (pixcol << 16)) & BLND_MSK;
    epixel = ((epixel * (16 - brightness)) >> 4) & BLND_MSK;  // Pixel color
    pixcol = (epixel >> 16) | epixel;
  }

  // Fill the line with that color
  while (start < end)
    scanline[start++] = pixcol;
}

#define RENDER_NORMAL   0
#define RENDER_COL16    1
#define RENDER_COL32    2
#define RENDER_ALPHA    3

#define OBJ_NORMAL   0
#define OBJ_COL16    1
#define OBJ_COL32    2
#define OBJ_ALPHA    3
#define OBJ_PALPHA   4

void render_layers(u32 start, u32 end, void *dst_ptr, u32 enabled_layers,
                   u32 rend_mode, u32 obj_mode) {
  u32 lnum;
  u16 dispcnt = read_ioreg(REG_DISPCNT);
  bool obj_enabled = (enabled_layers & 0x10);   // Objects are visible
  // Renderers for this mode (affine or text pointers)
  const tile_layer_render_struct * r = tile_mode_renderers[dispcnt & 0x07];

  for (lnum = 0; lnum < layer_count; lnum++) {
    u32 layer = layer_order[lnum];
    bool is_obj = layer & 0x4;
    if (is_obj && obj_enabled) {
      // Draw an object first-layer, we need to fill backdrop color first!
      if (rend_mode == RENDER_NORMAL)
        fill_line_background<false, u16>(start, end, dst_ptr);
      else if (rend_mode == OBJ_COL16)
        fill_line_background<true, u16>(start, end, dst_ptr);
      else
        fill_line_background<true, u32>(start, end, dst_ptr);

      obj_mode_renderers[obj_mode][(dispcnt >> 6) & 1](layer & 0x3, start, end, dst_ptr);
      break;
    }
    else if (!is_obj && ((1 << layer) & enabled_layers)) {
      // Draw base layer
      r[layer].base[rend_mode](layer, start, end, dst_ptr);
      break;
    }
  }

  if (lnum == layer_count) {
    // Render background, no layers are active!
    // TODO improve this code.
    if (rend_mode == RENDER_NORMAL)
      fill_line_background<false, u16>(start, end, dst_ptr);
    else if (rend_mode == OBJ_COL16)
      fill_line_background<true, u16>(start, end, dst_ptr);
    else
      fill_line_background<true, u32>(start, end, dst_ptr);

    return;
  }

  while (++lnum < layer_count) {
    u32 layer = layer_order[lnum];
    bool is_obj = layer & 0x4;
    if (is_obj && obj_enabled)
      obj_mode_renderers[obj_mode][(dispcnt >> 6) & 1](layer & 0x3, start, end, dst_ptr);
    else if (!is_obj && ((1 << layer) & enabled_layers))
      r[layer].trans[rend_mode](layer, start, end, dst_ptr);
  }
}

// Renders a partial scanline without using any coloring effects (with the
// exception of OBJ blending).

static void render_color_no_effect(
  u32 start, u32 end, u16* scanline, u32 enable_flags
) {
  bool obj_blend = obj_alpha_count[read_ioreg(REG_VCOUNT)] > 0;

  // Default rendering mode, without layer effects (except perhaps sprites).
  if (obj_blend) {
    u32 screen_buffer[240];
    render_layers(start, end, screen_buffer, enable_flags, RENDER_COL32, OBJ_PALPHA);
    merge_blend<BLEND_ONLY>(start, end, scanline, screen_buffer);
  } else {
    render_layers(start, end, scanline, enable_flags, RENDER_NORMAL, OBJ_NORMAL);
  }
}

// Renders all layers honoring color effects (blending, brighten/darken).
// It uses different rendering routines depending on the coloring effect
// requirements, speeding up common cases where no effects are used.

// No effects use NORMAL mode (RBB565 color is written on the buffer).
// For blending, we use BLEND mode to record the two top-most pixels.
// For other effects we use COLOR16, which records an indexed color in the
// buffer (used for darken/brighten effects at later passes) or COLOR32,
// which similarly uses an indexed color for rendering but recording one
// color for the background and another one for the object layer.

static void render_color_effect(
  u32 start, u32 end, u16* scanline, u32 enable_flags = 0x1F /* all enabled */
) {
  bool obj_blend = obj_alpha_count[read_ioreg(REG_VCOUNT)] > 0;
  u16 bldcnt = read_ioreg(REG_BLDCNT);

  switch((bldcnt >> 6) & 0x03) {
  case COL_EFFECT_BRIGHT:
    {
      // If no layers are 1st target, no effect will really happen.
      bool some_1st_tgt = (read_ioreg(REG_BLDCNT) & 0x3F) != 0;
      // If the factor is zero, it's the same as "regular" rendering.
      bool non_zero_blend = (read_ioreg(REG_BLDY) & 0x1F) != 0;
      if (some_1st_tgt && non_zero_blend) {
        if (obj_blend) {
          u32 screen_buffer[240];
          render_layers(start, end, screen_buffer, enable_flags, RENDER_COL32, OBJ_PALPHA);
          merge_blend<BLEND_BRIGHT>(start, end, scanline, screen_buffer);
        } else {
          render_layers(start, end, scanline, enable_flags, RENDER_COL16, OBJ_COL16);
          merge_brightness<BLEND_BRIGHT>(start, end, scanline);
        }
        return;
      }
    }
    break;

  case COL_EFFECT_DARK:
    {
      // If no layers are 1st target, no effect will really happen.
      bool some_1st_tgt = (read_ioreg(REG_BLDCNT) & 0x3F) != 0;
      // If the factor is zero, it's the same as "regular" rendering.
      bool non_zero_blend = (read_ioreg(REG_BLDY) & 0x1F) != 0;
      if (some_1st_tgt && non_zero_blend) {
        if (obj_blend) {
          u32 screen_buffer[240];
          render_layers(start, end, screen_buffer, enable_flags, RENDER_COL32, OBJ_PALPHA);
          merge_blend<BLEND_DARK>(start, end, scanline, screen_buffer);
        } else {
          render_layers(start, end, scanline, enable_flags, RENDER_COL16, OBJ_COL16);
          merge_brightness<BLEND_DARK>(start, end, scanline);
        }
        return;
      }
    }
    break;

  case COL_EFFECT_BLEND:
    {
      // If no layers are 1st or 2nd target, no effect will really happen.
      bool some_1st_tgt = (read_ioreg(REG_BLDCNT) & 0x003F) != 0;
      bool some_2nd_tgt = (read_ioreg(REG_BLDCNT) & 0x3F00) != 0;
      // If 1st target is 100% opacity and 2nd is 0%, just render regularly.
      bool non_trns_tgt = (read_ioreg(REG_BLDALPHA) & 0x1F1F) != 0x001F;
      if (some_1st_tgt && some_2nd_tgt && non_trns_tgt) {
        u32 screen_buffer[240];
        render_layers(start, end, screen_buffer, enable_flags, RENDER_ALPHA, OBJ_ALPHA);
        merge_blend<BLEND_ONLY>(start, end, scanline, screen_buffer);
        return;
      }
    }
    break;

  case COL_EFFECT_NONE:
    // Default case, see below.
    break;
  };

  // Default case, just a regular no-effects render.
  render_color_no_effect(start, end, scanline, enable_flags);
}


// Renders an entire scanline from 0 to 240, based on current color mode.
template<bool tiled>
static void render_scanline(u16 *scanline)
{
  u32 current_layer;
  u32 layer_order_pos;

  if (tiled) {
    if (layer_count)
      render_color_effect(0, 240, scanline);
    else
      render_backdrop(0, 240, scanline);
  } else {
    u16 dispcnt = read_ioreg(REG_DISPCNT);
    const bitmap_layer_render_struct *lrend = &bitmap_mode_renderers[(dispcnt & 0x07) - 3];
    fill_line_background<false, u16>(0, 240, scanline);

    for(layer_order_pos = 0; layer_order_pos < layer_count; layer_order_pos++)
    {
      current_layer = layer_order[layer_order_pos];
      if(current_layer & 0x04)
      {
        render_obj_layer(normal, scanline, 0, 240);
      }
      else
      {
        s32 dx = (s16)read_ioreg(REG_BG2PA);
        s32 dy = (s16)read_ioreg(REG_BG2PC);

        if (dy)
          lrend->affine_render(0, 240, scanline);
        else if (dx == 256)
          lrend->blit_render(0, 240, scanline);
        else
          lrend->scale_render(0, 240, scanline);
      }
    }
  }
}

// Render all of the BG and OBJ in a tiled scanline from start to end ONLY if
// enable_flag allows that layer/OBJ. Also conditionally render color effects.

static void render_scanline_conditional_tile(u32 start, u32 end, u16 *scanline,
 u32 enable_flags)
{
  if (layer_count && (enable_flags & 0x1F)) {
    bool effects_enabled = enable_flags & 0x20;   // Window bit for effects.
    if (effects_enabled)
      render_color_effect(start, end, scanline, enable_flags);
    else
      render_color_no_effect(start, end, scanline, enable_flags);
  }
  else
    render_backdrop(start, end, scanline);
}


// Render the BG and OBJ in a bitmap scanline from start to end ONLY if
// enable_flag allows that layer/OBJ. Also conditionally render color effects.

static void render_scanline_conditional_bitmap(u32 start, u32 end, u16 *scanline,
 u32 enable_flags)
{
  u16 dispcnt = read_ioreg(REG_DISPCNT);
  const bitmap_layer_render_struct *layer_renderers =
                                    &bitmap_mode_renderers[(dispcnt & 0x07) - 3];

  u32 current_layer;
  u32 layer_order_pos;

  fill_line_background<false, u16>(start, end, scanline);

  for(layer_order_pos = 0; layer_order_pos < layer_count; layer_order_pos++)
  {
    current_layer = layer_order[layer_order_pos];
    if(current_layer & 0x04)
    {
      if(enable_flags & 0x10)
      {
        render_obj_layer(normal, scanline, start, end);
      }
    }
    else
    {
      if(enable_flags & 0x04) {
        s32 dx = (s16)read_ioreg(REG_BG2PA);
        s32 dy = (s16)read_ioreg(REG_BG2PC);

        if (dy)
          layer_renderers->affine_render(start, end, scanline);
        else if (dx == 256)
          layer_renderers->blit_render(start, end, scanline);
        else
          layer_renderers->scale_render(start, end, scanline);
      }
    }
  }
}


// If the window Y coordinates are out of the window range we can skip
// rendering the inside of the window.
inline bool in_window_y(u32 vcount, u32 top, u32 bottom) {
  // TODO: check if these are reversed when top-bottom are also reversed.
  if (top > 227)     // This causes the window to be invisible
    return false;
  if (bottom > 227)  // This makes it all visible
    return true;

  if (top > bottom)  /* Reversed: if not in the "band" */
    return vcount > top || vcount <= bottom;

  return vcount >= top && vcount < bottom;
}

// Temporary wrap functions, to be removed once all the plain calls do not exist

template <bool tiled>
static inline void render_scanline_conditional(u32 start, u32 end,
  u16 *scanline, u32 enable_flags)
{
  if (tiled)
    render_scanline_conditional_tile(start, end, scanline, enable_flags);
  else
    render_scanline_conditional_bitmap(start, end, scanline, enable_flags);
}

// Renders window1 (low priority window) and outside/obj areas for a given range.
template <bool tiled>
static void render_windowobj_pass(u16 *scanline, u32 start, u32 end)
{
  u16 dispcnt = read_ioreg(REG_DISPCNT);
  u32 winout = read_ioreg(REG_WINOUT);
  u32 wndout_enable = winout & 0x3F;

  // First we render the "window-out" segment.
  render_scanline_conditional<tiled>(start, end, scanline, wndout_enable);

  // Now we render the objects in "copy" mode. This renders the scanline in
  // WinObj-mode to a temporary buffer and performs a "copy-mode" render.
  // In this mode, we copy pixels from the temp buffer to the final buffer
  // whenever an object pixel is rendered.
  if (dispcnt >> 15) {
    // Perform the actual object rendering in copy mode
    if (tiled) {
      // TODO: Make version 1D/2D?   if (dispcnt & 0x40)
      render_scanline_objects<u16, PIXCOPY, render_scanline_conditional_tile>(start, end, scanline, 4);
    } else {
      render_scanline_objects<u16, PIXCOPY, render_scanline_conditional_bitmap>(start, end, scanline, 4);
    }
  }
}

// Renders window1 (low priority window) and outside/obj areas for a given range.
template <bool tiled>
static void render_window1_pass(u16 *scanline, u32 start, u32 end)
{
  u16 dispcnt = read_ioreg(REG_DISPCNT);
  u32 winout = read_ioreg(REG_WINOUT);
  u32 wndout_enable = winout & 0x3F;

  switch (dispcnt >> 14) {
  case 0x0:    // No Win1 nor WinObj
    render_scanline_conditional<tiled>(
      start, end, scanline, wndout_enable);
    break;
  case 0x2:    // Only winobj enabled, render it.
    render_windowobj_pass<tiled>(scanline, start, end);
    break;
  case 0x1: case 0x3:   // Win1 is enabled (and perhaps WinObj too)
    {
      // Attempt to render window 1
      u32 vcount = read_ioreg(REG_VCOUNT);
      // Check the Y coordinates to check if they fall in the right row
      u32 win_top = read_ioreg(REG_WINxV(1)) >> 8;
      u32 win_bot = read_ioreg(REG_WINxV(1)) & 0xFF;
      // Check the X coordinates and generate up to three segments
      // Clip the coordinates to the [start, end) range.
      u32 win_l = MAX(start, MIN(end, read_ioreg(REG_WINxH(1)) >> 8));
      u32 win_r = MAX(start, MIN(end, read_ioreg(REG_WINxH(1)) & 0xFF));

      if (!in_window_y(vcount, win_top, win_bot) || (win_l == win_r))
        // Window1 is completely out, just render all out.
        render_windowobj_pass<tiled>(scanline, start, end);
      else {
        // Render win1 withtin the clipped range
        // Enable bits for stuff inside the window (and outside)
        u32 winin  = read_ioreg(REG_WININ);
        u32 wnd1_enable = (winin >> 8) & 0x3F;

        // If the window is defined upside down, the areas are inverted.
        if (win_l < win_r) {
          // Render [start, win_l) range (which is outside the window)
          if (win_l != start)
            render_windowobj_pass<tiled>(scanline, start, win_l);
          // Render the actual window0 pixels
          render_scanline_conditional<tiled>(
            win_l, win_r, scanline, wnd1_enable);
          // Render the [win_l, end] range (outside)
          if (win_r != end)
            render_windowobj_pass<tiled>(scanline, win_r, end);
        } else {
          // Render [0, win_r) range (which is "inside" window0)
          if (win_r != start)
            render_scanline_conditional<tiled>(
              start, win_r, scanline, wnd1_enable);
          // The actual window is now outside, render recursively
          render_windowobj_pass<tiled>(scanline, win_r, win_l);
          // Render the [win_l, 240] range ("inside")
          if (win_l != end)
            render_scanline_conditional<tiled>(
              win_l, end, scanline, wnd1_enable);
        }
      }
    }
    break;
  };
}

// Renders window0 (high priority window) and renders window1 or out
// on the area that falls outside. It will call the above function for
// outside areas to "recursively" render segments.
template <bool tiled>
static void render_window0_pass(u16 *scanline)
{
  u32 vcount = read_ioreg(REG_VCOUNT);
  // Check the Y coordinates to check if they fall in the right row
  u32 win_top = read_ioreg(REG_WINxV(0)) >> 8;
  u32 win_bot = read_ioreg(REG_WINxV(0)) & 0xFF;
  // Check the X coordinates and generate up to three segments
  u32 win_l = MIN(240, read_ioreg(REG_WINxH(0)) >> 8);
  u32 win_r = MIN(240, read_ioreg(REG_WINxH(0)) & 0xFF);

  if (!in_window_y(vcount, win_top, win_bot) || (win_l == win_r))
    // No windowing, everything is "outside", just render win1.
    render_window1_pass<tiled>(scanline, 0, 240);
  else {
    u32 winin  = read_ioreg(REG_WININ);
    // Enable bits for stuff inside the window
    u32 wnd0_enable = (winin) & 0x3F;

    // If the window is defined upside down, the areas are inverted.
    if (win_l < win_r) {
      // Render [0, win_l) range (which is outside the window)
      if (win_l)
        render_window1_pass<tiled>(scanline, 0, win_l);
      // Render the actual window0 pixels
      render_scanline_conditional<tiled>(
        win_l, win_r, scanline, wnd0_enable);
      // Render the [win_l, 240] range (outside)
      if (win_r != 240)
        render_window1_pass<tiled>(scanline, win_r, 240);
    } else {
      // Render [0, win_r) range (which is "inside" window0)
      if (win_r)
        render_scanline_conditional<tiled>(
          0, win_r, scanline, wnd0_enable);
      // The actual window is now outside, render recursively
      render_window1_pass<tiled>(scanline, win_r, win_l);
      // Render the [win_l, 240] range ("inside")
      if (win_l != 240)
        render_scanline_conditional<tiled>(
          win_l, 240, scanline, wnd0_enable);
    }
  }
}


// Renders a full scaleline, taking into consideration windowing effects.
// Breaks the rendering step into N steps, for each windowed region.
template <bool tiled>
static void render_scanline_window(u16 *scanline)
{
  u16 dispcnt = read_ioreg(REG_DISPCNT);
  u32 win_ctrl = (dispcnt >> 13);

  // Priority decoding for windows
  switch (win_ctrl) {
  case 0x1: case 0x3: case 0x5: case 0x7:
    // Window 0 is enabled, call the win0 render function. It does recursively
    // check for window 1 and Obj, so no worries.
    render_window0_pass<tiled>(scanline);
    break;
  case 0x2: case 0x6:
    // Window 1 is active, call the window1 renderer.
    render_window1_pass<tiled>(scanline, 0, 240);
    break;
  case 0x4:
    // Only winobj seems active
    render_windowobj_pass<tiled>(scanline, 0, 240);
    break;
  case 0x0:
    // No windows are active?
    render_scanline<tiled>(scanline);
    break;
  }
}

static const u8 active_layers[] = {
  0x1F,   // Mode 0, Tile BG0-3 and OBJ
  0x17,   // Mode 1, Tile BG0-2 and OBJ
  0x1C,   // Mode 2, Tile BG2-3 and OBJ
  0x14,   // Mode 3, BMP  BG2 and OBJ
  0x14,   // Mode 4, BMP  BG2 and OBJ
  0x14,   // Mode 5, BMP  BG2 and OBJ
  0,      // Unused
  0,
};

void update_scanline(void)
{
  u32 pitch = get_screen_pitch();
  u16 dispcnt = read_ioreg(REG_DISPCNT);
  u32 vcount = read_ioreg(REG_VCOUNT);
  u16 *screen_offset = get_screen_pixels() + (vcount * pitch);
  u32 video_mode = dispcnt & 0x07;

  // If OAM has been modified since the last scanline has been updated then
  // reorder and reprofile the OBJ lists.
  if(reg[OAM_UPDATED])
  {
    order_obj(video_mode);
    reg[OAM_UPDATED] = 0;
  }

  order_layers((dispcnt >> 8) & active_layers[video_mode], vcount);

  if(skip_next_frame)
    return;

  if(dispcnt & 0x80)
  {
    // If the screen is in in forced blank draw pure white.
    memset(screen_offset, 0xff, 240*sizeof(u16));
  }
  else
  {
    // Modes 0..2 are tiled modes, 3..5 are bitmap-based modes.
    if(video_mode < 3)
      render_scanline_window<true>(screen_offset);
    else
      render_scanline_window<false>(screen_offset);
  }

  affine_reference_x[0] += (s16)read_ioreg(REG_BG2PB);
  affine_reference_y[0] += (s16)read_ioreg(REG_BG2PD);
  affine_reference_x[1] += (s16)read_ioreg(REG_BG3PB);
  affine_reference_y[1] += (s16)read_ioreg(REG_BG3PD);
}


