/* gameplaySP
 *
 * Copyright (C) 2006 Exophase <exophase@gmail.com>
 * Copyright (C) 2023 David Guillen Fandos <david@davidgf.net>
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

typedef void (* bitmap_render_function)(u32 start, u32 end, void *dest_ptr);
typedef void (* tile_render_function)(u32 layer, u32 start, u32 end, void *dest_ptr);

typedef void (*conditional_render_function)(
  u32 start, u32 end, u16 *scanline, u32 enable_flags);

typedef void (*window_render_function)(u16 *scanline, u32 start, u32 end);

typedef struct
{
  bitmap_render_function blit_render;
  bitmap_render_function scale_render;
  bitmap_render_function affine_render;
} bitmap_layer_render_struct;

// Object blending modes
#define OBJ_MOD_NORMAL     0
#define OBJ_MOD_SEMITRAN   1
#define OBJ_MOD_WINDOW     2
#define OBJ_MOD_INVALID    3

// BLDCNT color effect modes
#define COL_EFFECT_NONE   0x0
#define COL_EFFECT_BLEND  0x1
#define COL_EFFECT_BRIGHT 0x2
#define COL_EFFECT_DARK   0x3

// Background render modes
#define RENDER_NORMAL   0
#define RENDER_COL16    1
#define RENDER_COL32    2
#define RENDER_ALPHA    3


// Byte lengths of complete tiles and tile rows in 4bpp and 8bpp.

#define tile_width_4bpp   4
#define tile_size_4bpp   32
#define tile_width_8bpp   8
#define tile_size_8bpp   64

// Sprite rendering cycles
#define REND_CYC_SCANLINE      1210
#define REND_CYC_REDUCED        954

// Generate bit mask (bits 9th and 10th) with information about the pixel
// status (1st and/or 2nd target) for later blending.
static inline u16 color_flags(u32 layer) {
  u32 bldcnt = read_ioreg(REG_BLDCNT);
  return (
    ((bldcnt >> layer) & 0x01) |            // 1st target
    ((bldcnt >> (layer + 7)) & 0x02)        // 2nd target
  ) << 9;
}

static const u32 map_widths[] = { 256, 512, 256, 512 };

typedef enum
{
  FULLCOLOR,  // Regular rendering, output a 16 bit color
  INDXCOLOR,  // Rendering to indexed color, so we can later apply dark/bright
  STCKCOLOR,  // Stacks two indexed pixels (+flags) to apply blending
  PIXCOPY     // Special mode used for sprites, to allow for obj-window drawing
} rendtype;

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

// Renders non-affine tiled background layer.
// Will process a full or partial tile (start and end within 0..8) and draw
// it in either 8 or 4 bpp mode. Honors vertical and horizontal flip.

template<typename dsttype, rendtype rdtype, bool isbase, bool hflip>
static inline void render_tile_Nbpp(u32 layer,
  dsttype *dest_ptr, bool is8bpp, u32 start, u32 end, u16 tile,
  const u8 *tile_base, int vertical_pixel_flip
) {
  // tile contains the tile info (contains tile index, flip bits, pal info)
  // hflip causes the tile pixels lookup to be reversed (from MSB to LSB
  // If isbase is not set, color 0 is interpreted as transparent, otherwise
  // we are drawing the base layer, so palette[0] is used (backdrop).

  // Seek to the specified tile, using the tile number and size.
  // tile_base already points to the right tile-line vertical offset
  const u8 *tile_ptr = &tile_base[(tile & 0x3FF) * (is8bpp ? 64 : 32)];

  // Calculate combine masks. These store 2 bits of info: 1st and 2nd target.
  // If set, the current pixel belongs to a layer that is 1st or 2nd target.
  u32 bg_comb = color_flags(5);
  u32 px_comb = color_flags(layer);

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
      if (isbase || pval) {
        if (rdtype == FULLCOLOR)
          *dest_ptr = palette_ram_converted[pval];
        else if (rdtype == INDXCOLOR)
          *dest_ptr = pval | combflg;  // Add combine flags
        else if (rdtype == STCKCOLOR)
          // Stack pixels on top of the pixel value and combine flags
          *dest_ptr = pval | combflg | ((isbase ? bg_comb : *dest_ptr) << 16);
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
      if (isbase || pval) {
        u8 colidx = pval ? (pval | tilepal) : 0;
        if (rdtype == FULLCOLOR)
          *dest_ptr = palette_ram_converted[colidx];
        else if (rdtype == INDXCOLOR)
          *dest_ptr = colidx | combflg;
        else if (rdtype == STCKCOLOR)
          *dest_ptr = colidx | combflg | ((isbase ? bg_comb : *dest_ptr) << 16);  // Stack pixels
      }
    }
  }
}

template<typename stype, rendtype rdtype, bool isbase>
static void render_scanline_text(u32 layer,
 u32 start, u32 end, void *scanline)
{
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
        render_tile_Nbpp<stype, rdtype, isbase, true>(layer, dest_ptr, mode8bpp, tile_hoff, stop, tile, tile_base, vflip_off);
      else
        render_tile_Nbpp<stype, rdtype, isbase, false>(layer, dest_ptr, mode8bpp, tile_hoff, stop, tile, tile_base, vflip_off);

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
        render_tile_Nbpp<stype, rdtype, isbase, true>(layer, &dest_ptr[i * 8], mode8bpp, 0, 8, tile, tile_base, vflip_off);
      else
        render_tile_Nbpp<stype, rdtype, isbase, false>(layer, &dest_ptr[i * 8], mode8bpp, 0, 8, tile, tile_base, vflip_off);
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
          render_tile_Nbpp<stype, rdtype, isbase, true>(layer, &dest_ptr[i * 8], mode8bpp, 0, 8, tile, tile_base, vflip_off);
        else
          render_tile_Nbpp<stype, rdtype, isbase, false>(layer, &dest_ptr[i * 8], mode8bpp, 0, 8, tile, tile_base, vflip_off);
      }

      end -= todraw * 8;
      dest_ptr += todraw * 8;
    }

    // Finalize the tile rendering the left side of it (from 0 up to "end").
    if (end) {
      u16 tile = eswap16(*map_ptr++);
      if (tile & 0x400)   // Tile horizontal flip
        render_tile_Nbpp<stype, rdtype, isbase, true>(layer, dest_ptr, mode8bpp, 0, end, tile, tile_base, vflip_off);
      else
        render_tile_Nbpp<stype, rdtype, isbase, false>(layer, dest_ptr, mode8bpp, 0, end, tile, tile_base, vflip_off);
    }
  }
}

template<typename dsttype, rendtype rdtype, bool isbase>
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
  u32 bg_comb = color_flags(5);
  u32 px_comb = color_flags(layer);

  // Combine mask is different if we are rendering the backdrop color
  u16 combflg = pval ? px_comb : bg_comb;
  // Alhpa mode stacks previous value (unless rendering the first layer)
  if (isbase || pval) {
    if (rdtype == FULLCOLOR)
      *dest_ptr = palette_ram_converted[pval];
    else if (rdtype == INDXCOLOR)
      *dest_ptr = pval | combflg;  // Add combine flags
    else if (rdtype == STCKCOLOR)
      // Stack pixels on top of the pixel value and combine flags
      *dest_ptr = pval | combflg | ((isbase ? bg_comb : *dest_ptr) << 16);
  }
}

template<typename dsttype, rendtype rdtype>
static inline void render_bdrop_pixel_8bpp(dsttype *dest_ptr) {
  // Calculate combine masks. These store 2 bits of info: 1st and 2nd target.
  // If set, the current pixel belongs to a layer that is 1st or 2nd target.
  u32 bg_comb = color_flags(5);
  u32 pval = 0;

  // Alhpa mode stacks previous value (unless rendering the first layer)
  if (rdtype == FULLCOLOR)
    *dest_ptr = palette_ram_converted[pval];
  else if (rdtype == INDXCOLOR)
    *dest_ptr = pval | bg_comb;  // Add combine flags
  else if (rdtype == STCKCOLOR)
    // Stack pixels on top of the pixel value and combine flags
    *dest_ptr = pval | bg_comb;
}

// Affine background rendering logic.
// wrap extends the background infinitely, otherwise transparent/backdrop fill
// rotate indicates if there's any rotation (optimized version for no-rotation)
template <typename dsttype, rendtype rdtype, bool isbase, bool wrap, bool rotate>
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
      render_pixel_8bpp<dsttype, rdtype, isbase>(
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

        // Draw a backdrop pixel if we are the base layer.
        if (isbase)
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
        render_pixel_8bpp<dsttype, rdtype, isbase>(
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
    if (isbase)
      while (cnt--)
        render_bdrop_pixel_8bpp<dsttype, rdtype>(dst_ptr++);
  }
}


// Renders affine backgrounds. These differ substantially from non-affine
// ones. Tile maps are byte arrays (instead of 16 bit), limiting the map to
// 256 different tiles (with no flip bits and just one single 256 color pal).

template<typename dsttype, rendtype rdtype, bool isbase>
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
      render_affine_background<dsttype, rdtype, isbase, true, true>(
        layer, start, end - start, map_base, map_size, tile_base, dest_ptr);
    else
      render_affine_background<dsttype, rdtype, isbase, true, false>(
        layer, start, end - start, map_base, map_size, tile_base, dest_ptr);
  } else {
    if (has_rotation)
      render_affine_background<dsttype, rdtype, isbase, false, true>(
        layer, start, end - start, map_base, map_size, tile_base, dest_ptr);
    else
      render_affine_background<dsttype, rdtype, isbase, false, false>(
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

#define bitmap_layer_render_functions(mode, ttype, w, h)                      \
{                                                                             \
  render_scanline_bitmap<mode, ttype, w, h, false, false>,                    \
  render_scanline_bitmap<mode, ttype, w, h, true, false>,                     \
  render_scanline_bitmap<mode, ttype, w, h, true, true>,                      \
}                                                                             \

static const bitmap_layer_render_struct bitmap_mode_renderers[3] =
{
  bitmap_layer_render_functions(3, u16, 240, 160),
  bitmap_layer_render_functions(4, u8,  240, 160),
  bitmap_layer_render_functions(5, u16, 160, 128)
};


// Object/Sprite rendering logic

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
  u32 attr1, attr2;
  bool is_double;
} t_sprite;

// Renders a tile row (8 pixels) for a regular (non-affine) object/sprite.
template<typename dsttype, rendtype rdtype, bool is8bpp, bool hflip>
static inline void render_obj_tile_Nbpp(bool forcebld,
  dsttype *dest_ptr, u32 start, u32 end, u32 tile_offset, u16 palette
) {
  // tile_ptr points to the tile row (32 or 64 bits depending on bpp).
  // renders the tile honoring hflip and start/end constraints

  // Note that the last VRAM bank wrap around, hence the offset aliasing
  const u8* tile_ptr = &vram[0x10000 + (tile_offset & 0x7FFF)];

  // Calculate combine masks. These store 2 bits of info: 1st and 2nd target.
  // If set, the current pixel belongs to a layer that is 1st or 2nd target.
  u32 px_comb = (forcebld ? 0x800 : 0) | color_flags(4);

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
template <typename stype, rendtype rdtype, bool forcebld, bool is8bpp, bool hflip>
static void render_object(
  s32 delta_x, u32 cnt, stype *dst_ptr, u32 tile_offset, u16 palette
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
    tile_offset += block_off * tile_size_off;

    // Render a partial tile to the left
    if (tile_off) {
      u32 residual = 8 - tile_off;   // Pixel count to complete the first tile
      u32 maxpix = MIN(residual, cnt);
      render_obj_tile_Nbpp<stype, rdtype, is8bpp, hflip>(forcebld, dst_ptr, tile_off, tile_off + maxpix, tile_offset, palette);

      // Move to the next tile
      tile_offset += tile_size_off;
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
    render_obj_tile_Nbpp<stype, rdtype, is8bpp, hflip>(forcebld, dst_ptr, 0, 8, tile_offset, palette);
    tile_offset += tile_size_off;
    dst_ptr += 8;
  }

  // Render any partial tile on the end
  cnt = cnt % 8;
  if (cnt)
    render_obj_tile_Nbpp<stype, rdtype, is8bpp, hflip>(forcebld, dst_ptr, 0, cnt, tile_offset, palette);
}


// Renders an affine sprite row to screen.
template <typename stype, rendtype rdtype, bool forcebld, bool is8bpp, bool rotate>
static void render_affine_object(
  const t_sprite *obji, const t_affp *affp, bool is_double,
  u32 start, u32 end, stype *dst_ptr, u32 base_tile, u16 palette
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
        base_tile +                        // Character base
        ((pixel_y >> 3) * tile_pitch) +    // Skip vertical blocks
        ((pixel_x >> 3) * tile_bsize) +    // Skip horizontal blocks
        ((pixel_y & 0x7) * tile_bwidth) +  // Skip vertical rows to the pixel
        (pixel_x & 0x7);                   // Skip the horizontal offset

      pixval = vram[0x10000 + (tile_off & 0x7FFF)];   // Read pixel value!
    } else {
      const u32 tile_off =
        base_tile +                        // Character base
        ((pixel_y >> 3) * tile_pitch) +    // Skip vertical blocks
        ((pixel_x >> 3) * tile_bsize) +    // Skip horizontal blocks
        ((pixel_y & 0x7) * tile_bwidth) +  // Skip vertical rows to the pixel
        ((pixel_x >> 1) & 0x3);            // Skip the horizontal offset

      u8 pixpair = vram[0x10000 + (tile_off & 0x7FFF)];  // Read two pixels (4bit each)
      pixval = ((pixel_x & 1) ? pixpair >> 4 : pixpair & 0xF);
    }

    // Render the pixel value
    u32 comb = (forcebld ? 0x800 : 0) | color_flags(4);
    if (pixval) {
      if (rdtype == FULLCOLOR)
        *dst_ptr = palette_ram_converted[pixval | palette | 0x100];
      else if (rdtype == INDXCOLOR)
        *dst_ptr = pixval | palette | 0x100 | comb;  // Add combine flags
      else if (rdtype == STCKCOLOR) {
        // Stack pixels on top of the pixel value and combine flags
        if (*dst_ptr & 0x100)
          *dst_ptr = pixval | palette | 0x100 | comb | ((*dst_ptr) & 0xFFFF0000);
        else
          *dst_ptr = pixval | palette | 0x100 | comb | ((*dst_ptr) << 16);  // Stack pixels
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

// Renders a single sprite on the current scanline
template <typename stype, rendtype rdtype, bool forcebld, bool is8bpp>
inline static void render_sprite(
  const t_sprite *obji, bool is_affine, u32 start, u32 end, stype *scanline
) {
  s32 vcount = read_ioreg(REG_VCOUNT);
  bool obj1dmap = read_ioreg(REG_DISPCNT) & 0x40;
  const u32 msk = is8bpp && !obj1dmap ? 0x3FE : 0x3FF;
  const u32 base_tile = (obji->attr2 & msk) * 32;

  if (is_affine) {
    u32 pnum = (obji->attr1 >> 9) & 0x1f;
    const t_affp *affp_base = (t_affp*)oam_ram;
    const t_affp *affp = &affp_base[pnum];
    u16 pal = is8bpp ? 0 : ((obji->attr2 >> 8) & 0xF0);

    if (affp->dy == 0)     // No rotation happening (just scale)
      render_affine_object<stype, rdtype, forcebld, is8bpp, false>(obji, affp, obji->is_double, start, end, scanline, base_tile, pal);
    else                   // Full rotation and scaling
      render_affine_object<stype, rdtype, forcebld, is8bpp, true>(obji, affp, obji->is_double, start, end, scanline, base_tile, pal);
  } else {
    // The object could be out of the window, check and skip.
    if (obji->obj_x >= (signed)end || obji->obj_x + obji->obj_w <= (signed)start)
      return;

    // Non-affine objects can be flipped on both edges.
    bool hflip = obji->attr1 & 0x1000;
    bool vflip = obji->attr1 & 0x2000;

    // Calulate the vertical offset (row) to be displayed. Account for vflip.
    u32 voffset = vflip ? obji->obj_y + obji->obj_h - vcount - 1 : vcount - obji->obj_y;

    // Calculate base tile for the object (points to the row to be drawn).
    u32 tile_bsize  = is8bpp ? tile_size_8bpp : tile_size_4bpp;
    u32 tile_bwidth = is8bpp ? tile_width_8bpp : tile_width_4bpp;
    u32 obj_pitch = obj1dmap ? (obji->obj_w / 8) * tile_bsize : 1024;
    u32 hflip_off = hflip ? ((obji->obj_w / 8) - 1) * tile_bsize : 0;

    // Calculate the pointer to the tile.
    const u32 tile_offset =
      base_tile +                    // Char offset
      (voffset / 8) * obj_pitch +    // Select tile row offset
      (voffset % 8) * tile_bwidth +  // Skip tile rows
      hflip_off;                     // Account for horizontal flip

    // Make everything relative to start
    s32 obj_x_offset  = obji->obj_x - start;
    u32 clipped_width = obj_x_offset >= 0 ? obji->obj_w : obji->obj_w + obj_x_offset;
    u32 max_range = obj_x_offset >= 0 ? end - obji->obj_x : end - start;
    u32 max_draw = MIN(max_range, clipped_width);

    // Render the object scanline using the correct mode.
    // (in 4bpp mode calculate the palette number)
    u16 pal = is8bpp ? 0 : ((obji->attr2 >> 8) & 0xF0);

    if (hflip)
      render_object<stype, rdtype, forcebld, is8bpp, true>(obj_x_offset, max_draw, &scanline[start], tile_offset, pal);
    else
      render_object<stype, rdtype, forcebld, is8bpp, false>(obj_x_offset, max_draw, &scanline[start], tile_offset, pal);
  }
}

// Renders objects on a scanline for a given priority.
template <typename stype, rendtype rdtype, conditional_render_function copyfn>
static void render_scanline_objs(
  u32 priority, u32 start, u32 end, void *raw_ptr
) {
  stype *scanline = (stype*)raw_ptr;
  s32 vcount = read_ioreg(REG_VCOUNT);
  s32 objn;
  u32 objcnt = obj_priority_count[priority][vcount];
  u8 *objlist = obj_priority_list[priority][vcount];

  // Render all the visible objects for this priority (back to front)
  for (objn = objcnt-1; objn >= 0; objn--) {
    // Objects in the list are pre-filtered and sorted in the appropriate order
    u32 objoff = objlist[objn];
    const t_oam *oamentry = &((t_oam*)oam_ram)[objoff];

    u16 obj_attr0 = eswap16(oamentry->attr0);
    u16 obj_attr1 = eswap16(oamentry->attr1);
    u16 obj_shape = obj_attr0 >> 14;
    u16 obj_size = (obj_attr1 >> 14);
    bool is_affine = obj_attr0 & 0x100;
    bool is_trans = ((obj_attr0 >> 10) & 0x3) == OBJ_MOD_SEMITRAN;
    bool is_8bpp = (obj_attr0 & 0x2000) != 0;

    t_sprite obji = {
      .obj_x = (s32)(obj_attr1 << 23) >> 23,
      .obj_y = obj_attr0 & 0xFF,
      .obj_w = obj_dim_table[obj_shape][obj_size][0],
      .obj_h = obj_dim_table[obj_shape][obj_size][1],
      .attr1 = obj_attr1,
      .attr2 = eswap16(oamentry->attr2),
      .is_double = (obj_attr0 & 0x200) != 0,
    };

    s32 obj_maxw = (is_affine && obji.is_double) ? obji.obj_w * 2 : obji.obj_w;

    // The object could be out of the window, check and skip.
    if (obji.obj_x >= (signed)end || obji.obj_x + obj_maxw <= (signed)start)
      continue;

    // ST-OBJs force 1st target bit (forced blending)
    bool forcebld = is_trans && rdtype != FULLCOLOR;

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

    if (is_8bpp) {
      if (forcebld)
        render_sprite<stype, rdtype, true, true>(&obji, is_affine, start, end, scanline);
      else
        render_sprite<stype, rdtype, false, true>(&obji, is_affine, start, end, scanline);
    } else {
      if (forcebld)
        render_sprite<stype, rdtype, true, false>(&obji, is_affine, start, end, scanline);
      else
        render_sprite<stype, rdtype, false, false>(&obji, is_affine, start, end, scanline);
    }
  }
}


// Goes through the object list in the OAM (from #127 to #0) and adds objects
// into a sorted list by priority for the current row.
// Invisible objects are discarded.
static void order_obj(u32 video_mode)
{
  u32 obj_num;
  u32 row;
  t_oam *oam_base = (t_oam*)oam_ram;
  u16 rend_cycles[160];

  memset(obj_priority_count, 0, sizeof(obj_priority_count));
  memset(obj_alpha_count, 0, sizeof(obj_alpha_count));
  memset(rend_cycles, 0, sizeof(rend_cycles));

  for(obj_num = 0; obj_num < 128; obj_num++)
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
              bool is_affine = obj_attr0 & 0x100;
              // Clip Y coord and height to the 0..159 interval
              u32 starty = MAX(obj_y, 0);
              u32 endy   = MIN(obj_y + obj_height, 160);

              // Calculate needed cycles to render the sprite
              u16 cyccnt = is_affine ? (10 + obj_width * 2) : obj_width;

              switch (obj_mode) {
              case OBJ_MOD_SEMITRAN:
                for(row = starty; row < endy; row++)
                {
                  if (rend_cycles[row] < REND_CYC_SCANLINE) {
                    u32 cur_cnt = obj_priority_count[obj_priority][row];
                    obj_priority_list[obj_priority][row][cur_cnt] = obj_num;
                    obj_priority_count[obj_priority][row] = cur_cnt + 1;
                    rend_cycles[row] += cyccnt;
                    // Mark the row as having semi-transparent objects
                    obj_alpha_count[row] = 1;
                  }
                }
                break;
              case OBJ_MOD_WINDOW:
                obj_priority = 4;
                /* fallthrough */
              case OBJ_MOD_NORMAL:
                // Add the object to the list.
                for(row = starty; row < endy; row++)
                {
                  if (rend_cycles[row] < REND_CYC_SCANLINE) {
                    u32 cur_cnt = obj_priority_count[obj_priority][row];
                    obj_priority_list[obj_priority][row][cur_cnt] = obj_num;
                    obj_priority_count[obj_priority][row] = cur_cnt + 1;
                    rend_cycles[row] += cyccnt;
                  }
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
  OBJ_BLEND,    // No effects, just blend forced-blend pixels (ie. ST objects)
  BLEND_ONLY,   // Just alpha blending (if the pixels are 1st and 2nd target)
  BLEND_BRIGHT, // Perform alpha blending if appropiate, and brighten otherwise
  BLEND_DARK,   // Same but with darken effecg
} blendtype;

// Applies blending (and optional brighten/darken) effect to a bunch of
// color-indexed pixel pairs. Depending on the mode and the pixel target
// number, blending, darken/brighten or no effect will be applied.
// Bits 0-8 encode the color index (paletted colors)
// Bit 9 is set if the pixel belongs to a 1st target layer
// Bit 10 is set if the pixel belongs to a 2nd target layer
// Bit 11 is set if the pixel belongs to a ST-object
template <blendtype bldtype, bool st_objs>
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
      // If ST-OBJ, force blending mode (has priority over other effects).
      // If regular blending mode, blend if 1st/2nd bits are set respectively.
      // Otherwise, apply other color effects if 1st bit is set.
      bool force_blend = (pixpair & 0x04000800) == 0x04000800;
      bool do_blend    = (pixpair & 0x04000200) == 0x04000200;
      if ((st_objs && force_blend) || (do_blend && bldtype == BLEND_ONLY)) {
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
      else if ((bldtype == BLEND_DARK || bldtype == BLEND_BRIGHT) &&
               (pixpair & 0x200) == 0x200) {
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
      bool do_blend    = (pixpair & 0x04000200) == 0x04000200;
      bool force_blend = (pixpair & 0x04000800) == 0x04000800;
      if ((st_objs && force_blend) || (do_blend && bldtype == BLEND_ONLY)) {
        // Top pixel is 1st target, pixel below is 2nd target. Blend!
        u16 p1 = palette_ram_converted[(pixpair >>  0) & 0x1FF];
        u16 p2 = palette_ram_converted[(pixpair >> 16) & 0x1FF];
        u32 p1e = (p1 | (p1 << 16)) & BLND_MSK;
        u32 p2e = (p2 | (p2 << 16)) & BLND_MSK;
        u32 pfe = (((p1e * blend_a) + (p2e * blend_b)) >> 4) & BLND_MSK;
        dst[start++] = (pfe >> 16) | pfe;
      }
      else if ((bldtype == BLEND_DARK || bldtype == BLEND_BRIGHT) &&
               (pixpair & 0x200) == 0x200) {
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

// Fills a segment using the backdrop color (in the right mode).
template<rendtype rdmode, typename dsttype>
void fill_line_background(u32 start, u32 end, dsttype *scanline) {
  while (start < end)
    if (rdmode == FULLCOLOR)
      scanline[start++] = palette_ram_converted[0];
    else
      scanline[start++] = 0;
}

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

// Renders all the available and enabled layers (in tiled mode).
// Walks the list of layers in visibility order and renders them in the
// specified mode (taking into consideration the first layer, etc).
template<rendtype bgmode, rendtype objmode, typename dsttype>
void render_layers(u32 start, u32 end, dsttype *dst_ptr, u32 enabled_layers) {
  u32 lnum;
  u32 base_done = 0;
  u16 dispcnt = read_ioreg(REG_DISPCNT);
  u16 video_mode = dispcnt & 0x07;
  bool obj_enabled = (enabled_layers & 0x10);   // Objects are visible

  bool objlayer_is_1st_tgt = ((read_ioreg(REG_BLDCNT) >> 4) & 1) != 0;
  bool has_trans_obj = obj_alpha_count[read_ioreg(REG_VCOUNT)];

  for (lnum = 0; lnum < layer_count; lnum++) {
    u32 layer = layer_order[lnum];
    bool is_obj = layer & 0x4;
    if (is_obj && obj_enabled) {
      bool can_skip_blend = !has_trans_obj && !objlayer_is_1st_tgt;

      // If it's the first layer, make sure to fill with backdrop color.
      if (!base_done)
        fill_line_background<bgmode, dsttype>(start, end, dst_ptr);

      // Optimization: skip blending mode if no blending can happen to this layer
      if (objmode == STCKCOLOR && can_skip_blend)
        render_scanline_objs<dsttype, INDXCOLOR, nullptr>(layer & 0x3, start, end, dst_ptr);
      else
        render_scanline_objs<dsttype, objmode, nullptr>(layer & 0x3, start, end, dst_ptr);

      base_done = 1;
    }
    else if (!is_obj && ((1 << layer) & enabled_layers)) {
      bool layer_is_1st_tgt = ((read_ioreg(REG_BLDCNT) >> layer) & 1) != 0;
      bool can_skip_blend = !has_trans_obj && !layer_is_1st_tgt;

      bool is_affine = (video_mode >= 1) && (layer >= 2);
      u32 fnidx = (base_done) | (is_affine ? 2 : 0);

      // Can optimize rendering if no blending can really happen.
      // If stack mode, no blending and not base layer, we might speed up a bit
      if (bgmode == STCKCOLOR && can_skip_blend) {
        static const tile_render_function rdfns[4] = {
          render_scanline_text<dsttype, INDXCOLOR, true>,
          render_scanline_text<dsttype, INDXCOLOR, false>,
          render_scanline_affine<dsttype, INDXCOLOR, true>,
          render_scanline_affine<dsttype, INDXCOLOR, false>,
        };
        rdfns[fnidx](layer, start, end, dst_ptr);
      } else {
        static const tile_render_function rdfns[4] = {
          render_scanline_text<dsttype, bgmode, true>,
          render_scanline_text<dsttype, bgmode, false>,
          render_scanline_affine<dsttype, bgmode, true>,
          render_scanline_affine<dsttype, bgmode, false>,
        };
        rdfns[fnidx](layer, start, end, dst_ptr);
      }

      base_done = 1;
    }
  }

  // Render background if we did not render any active layer.
  if (!base_done)
    fill_line_background<bgmode, dsttype>(start, end, dst_ptr);
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
    render_layers<INDXCOLOR, STCKCOLOR, u32>(start, end, screen_buffer, enable_flags);
    merge_blend<OBJ_BLEND, true>(start, end, scanline, screen_buffer);
  } else {
    render_layers<FULLCOLOR, FULLCOLOR, u16>(start, end, scanline, enable_flags);
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
          render_layers<INDXCOLOR, STCKCOLOR, u32>(start, end, screen_buffer, enable_flags);
          merge_blend<BLEND_BRIGHT, true>(start, end, scanline, screen_buffer);
        } else {
          render_layers<INDXCOLOR, INDXCOLOR, u16>(start, end, scanline, enable_flags);
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
          render_layers<INDXCOLOR, STCKCOLOR, u32>(start, end, screen_buffer, enable_flags);
          merge_blend<BLEND_DARK, true>(start, end, scanline, screen_buffer);
        } else {
          render_layers<INDXCOLOR, INDXCOLOR, u16>(start, end, scanline, enable_flags);
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
        render_layers<STCKCOLOR, STCKCOLOR, u32>(start, end, screen_buffer, enable_flags);
        if (obj_blend)
          merge_blend<BLEND_ONLY, true>(start, end, scanline, screen_buffer);
        else
          merge_blend<BLEND_ONLY, false>(start, end, scanline, screen_buffer);
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


// Render all of the BG and OBJ in a tiled scanline from start to end ONLY if
// enable_flag allows that layer/OBJ. Also conditionally render color effects.

static void render_conditional_tile(
  u32 start, u32 end, u16 *scanline, u32 enable_flags)
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

static void render_conditional_bitmap(
  u32 start, u32 end, u16 *scanline, u32 enable_flags)
{
  u16 dispcnt = read_ioreg(REG_DISPCNT);
  const bitmap_layer_render_struct *layer_renderers =
                                    &bitmap_mode_renderers[(dispcnt & 0x07) - 3];

  u32 current_layer;
  u32 layer_order_pos;

  fill_line_background<FULLCOLOR, u16>(start, end, scanline);

  for(layer_order_pos = 0; layer_order_pos < layer_count; layer_order_pos++)
  {
    current_layer = layer_order[layer_order_pos];
    if(current_layer & 0x04)
    {
      if(enable_flags & 0x10)
        render_scanline_objs<u16, FULLCOLOR, nullptr>(current_layer & 3, start, end, scanline);
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


static inline void render_scanline_conditional(
  u32 start, u32 end, u16 *scanline, u32 enable_flags = 0x3F)
{
  u16 dispcnt = read_ioreg(REG_DISPCNT);
  u32 video_mode = dispcnt & 0x07;
  // Modes 0..2 are tiled modes, 3..5 are bitmap-based modes.
  if(video_mode < 3)
    render_conditional_tile(start, end, scanline, enable_flags);
  else
    render_conditional_bitmap(start, end, scanline, enable_flags);
}

// Renders the are outside of all active windows
static void render_windowout_pass(u16 *scanline, u32 start, u32 end)
{
  u32 winout = read_ioreg(REG_WINOUT);
  u32 wndout_enable = winout & 0x3F;

  render_scanline_conditional(start, end, scanline, wndout_enable);
}

// Renders window-obj. This is a pixel-level windowing effect, based on sprites
// (objects) with a special rendering mode (the sprites are not themselves
// visible but rather "enable" other pixels to be rendered conditionally).
static void render_windowobj_pass(u16 *scanline, u32 start, u32 end)
{
  u16 dispcnt = read_ioreg(REG_DISPCNT);
  u32 winout = read_ioreg(REG_WINOUT);
  u32 wndout_enable = winout & 0x3F;

  // First we render the "window-out" segment.
  render_scanline_conditional(start, end, scanline, wndout_enable);

  // Now we render the objects in "copy" mode. This renders the scanline in
  // WinObj-mode to a temporary buffer and performs a "copy-mode" render.
  // In this mode, we copy pixels from the temp buffer to the final buffer
  // whenever an object pixel is rendered.
  if (dispcnt >> 15) {
    u32 video_mode = dispcnt & 0x07;
    // Perform the actual object rendering in copy mode
    if (video_mode < 3) {
      // TODO: Make version 1D/2D?   if (dispcnt & 0x40)
      render_scanline_objs<u16, PIXCOPY, render_conditional_tile>(4, start, end, scanline);
    } else {
      render_scanline_objs<u16, PIXCOPY, render_conditional_bitmap>(4, start, end, scanline);
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

// Renders window 0/1. Checks boundaries and divides the segment into
// subsegments (if necessary) rendering each one in their right mode.
// outfn is called for "out-of-window" rendering.
template<window_render_function outfn, unsigned winnum>
static void render_window_n_pass(u16 *scanline, u32 start, u32 end)
{
  u32 vcount = read_ioreg(REG_VCOUNT);
  // Check the Y coordinates to check if they fall in the right row
  u32 win_top = read_ioreg(REG_WINxV(winnum)) >> 8;
  u32 win_bot = read_ioreg(REG_WINxV(winnum)) & 0xFF;
  // Check the X coordinates and generate up to three segments
  // Clip the coordinates to the [start, end) range.
  u32 win_l = MAX(start, MIN(end, read_ioreg(REG_WINxH(winnum)) >> 8));
  u32 win_r = MAX(start, MIN(end, read_ioreg(REG_WINxH(winnum)) & 0xFF));

  if (!in_window_y(vcount, win_top, win_bot) || (win_l == win_r))
    // WindowN is completely out, just render all out.
    outfn(scanline, start, end);
  else {
    // Render window withtin the clipped range
    // Enable bits for stuff inside the window (and outside)
    u32 winin = read_ioreg(REG_WININ);
    u32 wndn_enable = (winin >> (8 * winnum)) & 0x3F;

    // If the window is defined upside down, the areas are inverted.
    if (win_l < win_r) {
      // Render [start, win_l) range (which is outside the window)
      if (win_l != start)
        outfn(scanline, start, win_l);
      // Render the actual window0 pixels
      render_scanline_conditional(win_l, win_r, scanline, wndn_enable);
      // Render the [win_l, end] range (outside)
      if (win_r != end)
        outfn(scanline, win_r, end);
    } else {
      // Render [0, win_r) range (which is "inside" window0)
      if (win_r != start)
        render_scanline_conditional(start, win_r, scanline, wndn_enable);
      // The actual window is now outside, render recursively
      outfn(scanline, win_r, win_l);
      // Render the [win_l, 240] range ("inside")
      if (win_l != end)
        render_scanline_conditional(win_l, end, scanline, wndn_enable);
    }
  }
}

// Renders a full scaleline, taking into consideration windowing effects.
// Breaks the rendering step into N steps, for each windowed region.
static void render_scanline_window(u16 *scanline)
{
  u16 dispcnt = read_ioreg(REG_DISPCNT);
  u32 win_ctrl = (dispcnt >> 13);

  // Priority decoding for windows
  switch (win_ctrl) {
  case 0x0: // No windows are active.
    render_scanline_conditional(0, 240, scanline);
    break;

  case 0x1: // Window 0
    render_window_n_pass<render_windowout_pass, 0>(scanline, 0, 240);
    break;

  case 0x2: // Window 1
    render_window_n_pass<render_windowout_pass, 1>(scanline, 0, 240);
    break;

  case 0x3: // Window 0 & 1
    render_window_n_pass<render_window_n_pass<render_windowout_pass, 1>, 0>(scanline, 0, 240);
    break;

  case 0x4: // Window Obj
    render_windowobj_pass(scanline, 0, 240);
    break;

  case 0x5: // Window 0 & Obj
    render_window_n_pass<render_windowobj_pass, 0>(scanline, 0, 240);
    break;

  case 0x6: // Window 1 & Obj
    render_window_n_pass<render_windowobj_pass, 1>(scanline, 0, 240);
    break;

  case 0x7: // Window 0, 1 & Obj
    render_window_n_pass<render_window_n_pass<render_windowobj_pass, 1>, 0>(scanline, 0, 240);
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

  if(skip_next_frame)
    return;

  // If OAM has been modified since the last scanline has been updated then
  // reorder and reprofile the OBJ lists.
  if(reg[OAM_UPDATED])
  {
    order_obj(video_mode);
    reg[OAM_UPDATED] = 0;
  }

  order_layers((dispcnt >> 8) & active_layers[video_mode], vcount);

  // If the screen is in in forced blank draw pure white.
  if(dispcnt & 0x80)
    memset(screen_offset, 0xff, 240*sizeof(u16));
  else
    render_scanline_window(screen_offset);

  // Mode 0 does not use any affine params at all.
  if (video_mode) {
    affine_reference_x[0] += (s16)read_ioreg(REG_BG2PB);
    affine_reference_y[0] += (s16)read_ioreg(REG_BG2PD);
    affine_reference_x[1] += (s16)read_ioreg(REG_BG3PB);
    affine_reference_y[1] += (s16)read_ioreg(REG_BG3PD);
  }
}


