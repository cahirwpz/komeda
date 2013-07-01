#ifndef __COMMAND_H__
#define __COMMAND_H__

#include <stdint.h>

typedef enum { CMD_NONE, CMD_HALT, CMD_REST, CMD_PLAY } CommandTypeT;

typedef struct Command {
  CommandTypeT type;
  union {
    struct {
      uint8_t n, d;
    } rest;
    struct {
      uint8_t n, d;
      uint8_t pitch;
    } play;
  };
} CommandT;

#endif
