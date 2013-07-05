#include <arpa/inet.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "debug.h"
#include "file.h"
#include "interp.h"

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

static bool ReadBytes(FILE *file, size_t length, uint8_t **data_p) {
  uint8_t *data = malloc(length);

  if (fread(data, length, 1, file) == length) {
    (*data_p) = data;
    return true;
  }

  free(data);
  return false;
}

static bool ReadString(FILE *file, char **string_p) {
  uint8_t length;

  return (ReadByte(file, &length) &&
          ReadBytes(file, length, (uint8_t **)string_p));
}

/*
 * Read Komeda binary file structures.
 */

static bool ReadProgram(FILE *file, ProgramT *program) {
  return (ReadWord(file, &program->num) &&
          ReadBytes(file, program->num * sizeof(uint16_t),
                    (uint8_t **)&program->cmd));
}

static bool ReadModule(FILE *file, KomedaT *komeda, int n) {
  ProgramT init;
  char *name = NULL;
  bool result = false;

  if (ReadString(file, &name)) {
    DEBUG("Module %d: instance of '%s'.", n, name);
    komeda->module[n].api = FindModule(name);

    if (ReadProgram(file, &init)) {
      DEBUG("Running initialization for module %d.", n);
      result = RunInitProgram(&init, &komeda->module[n]);
      free(init.cmd);
    }

    free(name);
  }

  return false;
}

static bool ReadKomeda(FILE *file, KomedaT **komeda_p) {
  size_t i;
  uint16_t modules, programs, channels;

  if (!(ReadWord(file, &modules) &&
        ReadWord(file, &programs) &&
        ReadWord(file, &channels)))
    return false;

  DEBUG("Komeda file (%d modules, %d programs, %d channels).",
        modules, programs, channels);

  KomedaT *komeda = CreateKomeda(modules, programs, channels);

  for (i = 0; i < modules; i++)
    if (!ReadModule(file, komeda, i))
      return false;

  for (i = 0; i < programs; i++)
    if (!ReadProgram(file, &komeda->program[i]))
      return false;

  (*komeda_p) = komeda;
  return true;
}

#if 0
PlayerT *LoadKomedaFile(const char *path) {
  PlayerT *player = malloc(sizeof(PlayerT));

  FILE *file = fopen(path, "rb");
  ReadYamaTracks(file, player);
  fclose(file);

  return player;
}
#endif
