
#ifndef SAVESTATE_H
#define SAVESTATE_H

#define bson_write_u32(p, value)                \
{                                               \
  u32 __tval = (value);                         \
  *p++ = (u8)((__tval));                        \
  *p++ = (u8)((__tval) >>  8);                  \
  *p++ = (u8)((__tval) >> 16);                  \
  *p++ = (u8)((__tval) >> 24);                  \
}

#define bson_read_u32(p)                        \
  ((p[3] << 24) | (p[2] << 16) |                \
   (p[1] <<  8) | (p[0] <<  0))

#define bson_write_cstring(p, value)            \
  memcpy(p, value, strlen(value)+1);            \
  p += strlen(value)+1;

#define bson_write_int32(p, key, value)         \
  *p++ = 0x10;                                  \
  bson_write_cstring(p, key);                   \
  bson_write_u32(p, value);

#define bson_write_int32array(p, key, arr, cnt) \
{                                               \
  u32 *arrptr = (u32*)(arr);                    \
  int _n;                                       \
  *p++ = 0x4;                                   \
  bson_write_cstring(p, key);                   \
  bson_write_u32(p, 5 + (cnt) * 8);             \
  for (_n = 0; _n < (cnt); _n++) {              \
    char ak[3] = {'0'+(_n/10), '0'+(_n%10), 0}; \
    bson_write_int32(p, ak, arrptr[_n]);        \
  }                                             \
  *p++ = 0;                                     \
}

#define bson_write_bytes(p, key, value, vlen)   \
  *p++ = 0x05;                                  \
  bson_write_cstring(p, key);                   \
  bson_write_u32(p, vlen);                      \
  *p++ = 0;                                     \
  memcpy(p, value, vlen);                       \
  p += vlen;

#define bson_start_document(p, key, hdrptr)     \
  *p++ = 0x03;                                  \
  bson_write_cstring(p, key);                   \
  hdrptr = p;                                   \
  bson_write_u32(p, 0);

#define bson_finish_document(p, hdrptr)         \
{                                               \
  u32 _sz = p - hdrptr + 1;                     \
  *p++ = 0;                                     \
  bson_write_u32(hdrptr, _sz);                  \
}

const u8* bson_find_key(const u8 *srcp, const char *key);
bool bson_read_int32(const u8 *srcp, const char *key, u32* value);
bool bson_read_int32_array(const u8 *srcp, const char *key, u32* value, unsigned cnt);
bool bson_read_bytes(const u8 *srcp, const char *key, void* buffer, unsigned cnt);

/* this is an upper limit, leave room for future (?) stuff */
#define GBA_STATE_MEM_SIZE                    (416*1024)
#define GBA_STATE_MAGIC                       0x06BAC0DE
#define GBA_STATE_VERSION                     0x00010000

bool gba_load_state(const void *src);
void gba_save_state(void *dst);

#endif

