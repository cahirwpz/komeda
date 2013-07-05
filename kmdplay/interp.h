#ifndef __INTERP_H__
#define __INTERP_H__

#include "modules/api.h"

typedef struct Channel ChannelT;

typedef struct Program {
  uint16_t num;
  uint16_t *cmd;
} ProgramT;

typedef struct Komeda {
  size_t no_modules; /* limited to 256 */
  size_t no_programs; /* limited to 256 */
  size_t no_channels; /* limited to 8 */

  ModuleT *module;
  ProgramT *program;  /* an array of programs used by interpreter */
  ChannelT *channel;  /* starting program for channel's interpreter */
} KomedaT;

KomedaT *CreateKomeda(size_t modules, size_t programs, size_t channels);
void FreeKomeda(KomedaT *komeda);

bool RunInitProgram(ProgramT *init, ModuleT *module);

#endif
