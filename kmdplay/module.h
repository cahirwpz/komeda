#ifndef __MODULE_H__
#define __MODULE_H__

#include <stdbool.h>
#include <stdint.h>

#include "command.h"

typedef struct ModuleState ModuleStateT;

/*
 * TODO: all methods may block? if so then we have to design mechanism that
 *       will pass events between module's methods and the scheduler.
 *
 *       Moreover a method can fail, then it returns "false" then which should
 *       stop the machine.
 */

typedef struct ModuleInterface {
  /* module constructor and destructor */
  ModuleStateT *(*ctor)(void);
  void (*dtor)(ModuleStateT *state);
  /* properties interface */
  bool (*set)(ModuleStateT *state, CommandT *command,
              uint8_t reg_no, uint8_t value);
  bool (*get)(ModuleStateT *state, CommandT *command,
              uint8_t reg_no, uint8_t *value_p);
  /* methods */
  bool (*method[0])(ModuleStateT *state);
} ModuleInterfaceT;

typedef struct Module {
  ModuleInterfaceT *api;
  ModuleStateT *state;
} ModuleT;

static inline bool ModuleGet(ModuleT *module, CommandT *command,
                             uint8_t reg_no, uint8_t *value_p)
{
  return module->api->get(module->state, command, reg_no, value_p);
}

static inline bool ModuleSet(ModuleT *module, CommandT *command,
                             uint8_t reg_no, uint8_t value)
{
  return module->api->set(module->state, command, reg_no, value);
}

static inline bool ModuleCall(ModuleT *module, CommandT *command,
                              uint8_t method_no)
{
  return module->api->method[method_no](module->state);
}

#endif
