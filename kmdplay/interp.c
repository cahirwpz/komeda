#include <strings.h>

#include "debug.h"
#include "interp.h"

/* number of general purpose registers */
#define NO_REGS 16
/* default stack depth */
#define STACK_DEPTH 32

struct Channel {
  uint8_t regs[NO_REGS];
  struct {             /* status register */
    uint8_t slurr : 1; /* slurr mode */
    uint8_t unit  : 3; /* unit length */
    uint8_t plus  : 1; /* y - x > 0 */
    uint8_t zero  : 1; /* y - x = 0 */
  } sr;
  uint8_t pc; /* program counter */
  uint8_t sp; /* stack pointer */
  uint8_t pr; /* procedure number register */
  uint8_t mr; /* active module number register */
  uint8_t vr; /* voice number register */
  uint8_t stack[STACK_DEPTH];
};

KomedaT *CreateKomeda(size_t modules, size_t programs, size_t channels) {
  KomedaT *komeda = xcalloc(1, sizeof(KomedaT));

  komeda->no_modules = modules;
  komeda->no_programs = programs;
  komeda->no_channels = channels;
  komeda->module = xcalloc(modules, sizeof(ModuleT));
  komeda->program = xcalloc(programs, sizeof(ProgramT));
  komeda->channel = xcalloc(channels, sizeof(ChannelT));

  return komeda;
}

void FreeKomeda(KomedaT *komeda) {
  int i;

  for (i = 0; i < komeda->no_programs; i++)
    free(komeda->program[i].cmd);
  free(komeda->program);

  for (i = 0; i < komeda->no_modules; i++)
    ; /* call destructor ? */

  free(komeda->channel);
  free(komeda->program);
  free(komeda);
}

void InterpreterOneStep(ProgramT *program, ChannelT *channel,
                        ModuleT *module, CommandT *command)
{
  uint16_t *cmd = &program->cmd[channel->pc++];
  uint8_t u = ((uint8_t *)cmd)[0];
  uint8_t v = ((uint8_t *)cmd)[1];
  uint8_t n = u & 15;
  uint8_t x = v >> 4;
  uint8_t y = v & 15;
  uint8_t tmp;

  command->type = CMD_CONTINUE;

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

    /* valid range: 0..5 */
    switch (u) {
      case 0: /* LD #v, Rn */
        channel->regs[n] = v;
        break;

      case 1: /* NOTE Rn:Rx:Ry*/
        command->type = CMD_PLAY;
        command->play.pitch = channel->regs[n];
        command->play.n = channel->regs[x];
        command->play.d = channel->regs[y];
        break;

      case 2: /* LOOP Rn, $v */
        if (channel->regs[n] > 0) {
          channel->regs[n]--;
          channel->pc = v;
        }
        break;

      default:
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
        channel->stack[channel->sp++] = channel->pr;
        channel->stack[channel->sp++] = channel->pc;
        channel->pr = v;
        channel->pc = 0;
        break;
      case 1: /* JMP $v */
        channel->pc = v;
        break;
      case 2: /* JEQ $v */
        if (channel->sr.zero)
          channel->pc = v;
        break;
      case 3: /* JNE $v */
        if (!channel->sr.zero)
          channel->pc = v;
        break;
      case 4: /* JLE $v */
        if (channel->sr.zero && !channel->sr.plus)
          channel->pc = v;
        break;
      case 5: /* JLT $v */
        if (!channel->sr.plus)
          channel->pc = v;
        break;
      case 6: /* JGE $v */
        if (channel->sr.zero && channel->sr.plus)
          channel->pc = v;
        break;
      case 7: /* JGT $v */
        if (channel->sr.plus)
          channel->pc = v;
        break;
      case 8: /* MOV Rx, Ry */
        channel->regs[y] = channel->regs[x];
        break;
      case 9: /* EXG Rx, Ry */
        tmp = channel->regs[y];
        channel->regs[y] = channel->regs[x];
        channel->regs[x] = tmp;
        break;
      case 10: /* ADD Rx, Ry */
        channel->regs[y] += channel->regs[x];
        break;
      case 11: /* SUB Rx, Ry */
        channel->regs[y] -= channel->regs[x];
        break;
      case 12: /* PRC Rx, Ry */
        channel->regs[y] = (channel->regs[y] * channel->regs[x]) / 100; 
        break;
      case 13: /* CMP Rx, Ry */
        x = channel->regs[x];
        y = channel->regs[y];
        channel->sr.zero = (x == y);
        channel->sr.plus = (x < y);
        break;
      case 27: /* REST Rx:Ry */
        command->type = CMD_REST;
        command->rest.n = channel->regs[x];
        command->rest.d = channel->regs[y];
        break;
      case 28: /* MGET mr:Rx, Ry */
        ModuleGet(module, command, x, &channel->regs[y]);
        break;
      case 29: /* MSET Rx, mr:Ry */
        ModuleSet(module, command, y, channel->regs[x]);
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
      case 0: /* PUSH Ry */
        channel->stack[channel->sp++] = channel->regs[y];
        break;
      case 1: /* POP Ry */
        channel->regs[y] = channel->stack[--channel->sp];
        break;
      case 2: /* MCALL y */
        ModuleCall(module, command, y);
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
      /* do nothing */
      break;
    case 1: /* RET */
      channel->pc = channel->stack[--channel->sp];
      channel->pr = channel->stack[--channel->sp];
      break;
    case 5: /* SLURR ON */
      channel->sr.slurr = true;
      break;
    case 6: /* SLURR OFF */
      channel->sr.slurr = false;
      break;
    case 15: /* HALT */
      command->type = CMD_HALT;
      break;
    default:
      DEBUG("Illegal command: D-form %d!", y);
      break;
  }
}

/*
 * These are completely broken for now.
 */

void RunChannelProgram(KomedaT *komeda, int num) {
  ChannelT *channel = &komeda->channel[num];
  ModuleT *module = &komeda->module[channel->mr];
  ProgramT *program = &komeda->program[channel->pr];
  CommandT *command = NULL;
  
  InterpreterOneStep(program, channel, module, command);
}

bool RunInitProgram(ProgramT *program, ModuleT *module) {
  ChannelT channel;
  CommandT command;

  bzero(&channel, sizeof(channel));
  bzero(&command, sizeof(command));
  
  InterpreterOneStep(program, &channel, module, &command);

  return true;
}
