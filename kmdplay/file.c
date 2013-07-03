#include <arpa/inet.h>
#include <stdbool.h>
#include <stdio.h>

#include "file.h"

/*
 * Raw file handling. Byte ordering is big-endian.
 */

static bool ReadByte(FILE *file, uint8_t *value_p) {
  size_t length = sizeof(uint8_t);
  return (fread(value_p, length, 1, file) == length);
}

static bool ReadWord(FILE *file, uint16_t *value_p) {
  size_t length = sizeof(uint16_t);
  bool result = (fread(value_p, length, 1, file) == length);
  (*value_p) = ntohs(*value_p);
  return result;
}

static bool ReadLong(FILE *file, uint32_t *value_p) {
  size_t length = sizeof(uint32_t);
  bool result = (fread(value_p, length, 1, file) == length);
  (*value_p) = ntohl(*value_p);
  return result;
}

static bool ReadBytes(FILE *file, void *data, size_t length) {
  return (fread(data, length, 1, file) == length);
}

/*
 * Read Komeda binary file structures.
 */

#if 0
static void ReadPatterns(FILE *file, PlayerT *player) {
  ChannelT *channel;
  int i, num;

  player->channelNum = num = ReadInt16(file);
  player->channel = calloc(num, sizeof(ChannelT));

  channel = player->channel;

  for (i = 0; i < num; i++) {
    int16_t size = ReadInt16(file);
    int16_t commands = ReadInt16(file);

    channel[i].data = malloc(size);
    channel[i].commands = commands;
    ReadBytes(file, channel[i].data, size);

    printf("Track %d: %zd commands (%d) bytes.\n", i, commands, size);
  }
}

PlayerT *LoadKomedaFile(const char *path) {
  PlayerT *player = malloc(sizeof(PlayerT));

  FILE *file = fopen(path, "rb");
  ReadYamaTracks(file, player);
  fclose(file);

  return player;
}
#endif
