#ifndef __MODULE_H__
#define __MODULE_H__

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

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

/* To be defined by each module separately. */
typedef struct ModuleState ModuleStateT;

/*
 * TODO: all methods may block? if so then we have to design mechanism that
 *       will pass events between module's methods and the scheduler.
 *
 *       Moreover a method can fail, then it returns "false" then which should
 *       stop the machine.
 */

typedef enum { MOD_VOICE, MOD_SENSOR } ModuleTypeT;

typedef struct ModuleInterface {
  ModuleTypeT type;
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

/* This structure describe instantiated modules (aka local modules). */
typedef struct Module {
  ModuleInterfaceT *api;
  ModuleStateT *state;
} ModuleT;

/* These make calling module's function a little bit easier. */
static inline bool
ModuleGet(ModuleT *module, CommandT *command, uint8_t reg_no, uint8_t *value_p)
{
  return module->api->get(module->state, command, reg_no, value_p);
}

static inline bool
ModuleSet(ModuleT *module, CommandT *command, uint8_t reg_no, uint8_t value)
{
  return module->api->set(module->state, command, reg_no, value);
}

static inline bool
ModuleCall(ModuleT *module, CommandT *command, uint8_t method_no)
{
  return module->api->method[method_no](module->state);
}

/* Module registry entry. */
typedef struct ModuleRegistryEntry {
  const char *name;
  const ModuleInterfaceT *module;
} ModuleRegistryEntryT;

extern const ModuleRegistryEntryT ModuleRegistry[];

#endif
