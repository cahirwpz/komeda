#include <stdint.h>
#include <stdbool.h>

#include "debug.h"

typedef struct Program {
  uint16_t cmds[0];
} ProgramT;

typedef struct MachineState {
} MachineStateT;

typedef struct Module {
  uint8_t attr[8];
  void ((*method[8])(void));
} ModuleT;

#define STACK_LENGTH 32

typedef struct ChannelState {
  uint8_t regs[8];
  ModuleT (*mods)[8];
  struct {             /* status register */
    uint8_t slurr : 1; /* slurr mode */
    uint8_t unit  : 3; /* unit length */
    uint8_t plus  : 1; /* a - b > 0 */
    uint8_t zero  : 1; /* a - b = 0 */
  } sr;
  uint8_t pc;  /* program counter */
  uint8_t sp;  /* stack pointer */
  uint8_t aor; /* active object register */
  uint8_t pnr; /* procedure number register */
  uint8_t vnr; /* voice number register */
  uint16_t *cmd;
  uint8_t stack[STACK_LENGTH];
} ChannelStateT;

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

static inline uint8_t ChannelGetReg(ChannelStateT *channel, uint8_t reg_no) {
  return 0;
}

static inline void ChannelSetReg(ChannelStateT *channel,
                                 uint8_t reg_no, uint8_t value)
{
}

void InterpreterOneStep(MachineStateT *state, ChannelStateT *channel,
                        CommandT *command)
{
  uint8_t u = ((uint8_t *)channel->cmd)[0];
  uint8_t v = ((uint8_t *)channel->cmd)[1];
  uint8_t n = u & 15;
  uint8_t x = v >> 4;
  uint8_t y = v & 15;

  channel->cmd++;
  channel->pc++;
  command->type = CMD_NONE;

  if (u < 0x80) {
    /* NOTE: 1ppp pppp nnnn dddd */
    /* play & rest fields overlap */
    command->play.n = x + 1;
    command->play.d = y + 1;

    if (u < 96) {
      command->type = CMD_PLAY;
      command->play.pitch = u;
    } else if (u == 96) {
      command->type = CMD_REST;
    } else {
      DEBUG("Illegal pitch value: %d!", u);
    }
    return;
  } 
  
  /* Handle A-form. */
  if (u < 0xc0) {
    u = (u >> 4) & 7;

    switch (u) {
      case 0: /* LD #v, Rn */
        ChannelSetReg(channel, n, v);
        break;

      case 1: /* CMOV n:CRx, Ry */
        ChannelSetReg(channel, y, channel->mods[n]->attr[x]);
        break;

      case 2: /* CMOV Rx, n:CRy */
        channel->mods[n]->attr[y] = ChannelGetReg(channel, x);
        break;

      case 3: /* CEXG n:CRx, Ry */
        {
          uint8_t tmp = ChannelGetReg(channel, y);
          ChannelSetReg(channel, y, channel->mods[n]->attr[x]);
          channel->mods[n]->attr[x] = tmp;
        }
        break;

      case 4: /* A-form: LOOP Rn, $v */
        break;

      default: /* A-form: reserved */
        DEBUG("Illegal command: A-form %d!", u);
        break;
    }
    return;
  }
 
  /* Handle B-form. */
  if (u < 0xff) {
    u -= 0xc0;

    switch (u) {
      case 0: /* CALL $v */
        break;
      case 1: /* JMP $v */
        break;
      case 2: /* JEQ $v */
        break;
      case 3: /* JNE $v */
        break;
      case 4: /* JLE $v */
        break;
      case 5: /* JLT $v */
        break;
      case 6: /* JGE $v */
        break;
      case 7: /* JGT $v */
        break;
      case 8: /* MOV Rx, Ry */
        break;
      case 9: /* EXG Rx, Ry */
        break;
      case 10: /* ADD Rx, Ry */
        break;
      case 11: /* SUB Rx, Ry */
        break;
      case 12: /* PRC Rx, Ry */
        break;
      case 13: /* CMP Rx, Ry */
        break;
      case 14: /* PUSHM ~v */
        break;
      case 15: /* POPM ~v */
        break;
      case 16: /* CPUSHM ~v */
        break;
      case 17: /* CPOPM ~v */
        break;
      case 29: /* NOTE Rx:Ry */
        break;
      case 30: /* INVOKE v */
        break;
      default:
        DEBUG("Illegal command: B-form %d!", n); 
        break;
    }
    return;
  }
 
  /* Handle C-form. */
  if (x < 15) {
    switch (x) {
      case 0: /* REST Ry */
        break;
      case 1: /* SIGNAL y */
        break;
      case 2: /* SETUNIT y */
        break;
      case 3: /* ACTIVATE y */
        break;
      default:
        DEBUG("Illegal command: C-form %d!", x);
        break;
    }
    return;
  }
 
  /* Handle D-form. */
  switch (y) {
    case 0: /* NOP */
      break;
    case 1: /* RET */
      break;
    case 2: /* CLRSIG */
      break;
    case 3: /* CHKSIG */
      break;
    case 4: /* WAITSIG */
      break;
    case 5: /* SLURR ON */
      break;
    case 6: /* SLURR OFF */
      break;
    case 15: /* HALT */
      break;
    default:
      DEBUG("Illegal command: D-form %d!", y);
      break;
  }
}
