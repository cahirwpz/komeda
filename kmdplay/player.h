#ifndef __PLAYER_H__
#define __PLAYER_H__

#include <stdbool.h>
#include <stddef.h>

typedef struct Note {
  size_t pitch;  /* in hertz, 0 => rest*/
  float  length; /* in seconds, 0 => end */
} NoteT;

typedef struct Channel {
  size_t current;
  NoteT note[];
} ChannelT;

void PlayerInit(size_t channels, ...);
bool PlayerRun(void);

#endif
