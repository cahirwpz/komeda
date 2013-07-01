#ifndef __FILE_H__
#define __FILE_H__

#include <stdint.h>

typedef struct Pattern {
  uint16_t num;
  uint16_t cmd[0];
} PatternT;

typedef struct Machine {
  uint16_t num;
  PatternT *pattern[0];
} MachineT;

#endif
