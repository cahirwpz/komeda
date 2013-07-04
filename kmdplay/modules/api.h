#ifndef __MODULE_H__
#define __MODULE_H__

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

typedef enum {
  CMD_CONTINUE, /* continue execution by invoking previous action by calling
                   either module's function or performing interpreter step */
  CMD_ERROR,    /* can be returned by both interpreter and module to indicate
                   that it has experienced unrecoverable error */
  CMD_HALT,     /* indicates that there'll be no more data produced for this
                   channel and the machine should halt its execution */ 
  CMD_REST,     /* rest for certain amount of time */
  CMD_PLAY,     /* play a note of given pitch for certain amount of time */

  /* types below are valid only in context of interaction with a module */

  CMD_INIT,     /* to be called only at initialization stage (can't block) */
  CMD_GET,      /* access module's public property (may block) */
  CMD_SET,      /* modify module's public property (may block) */
  CMD_CALL,     /* call module's method (may block) */
  CMD_RETURN,   /* set by module to indicate that control should return to the
                   interpreter */

  /* channel can wait for one event at a time, we should know who's going to
   * send a response and its type */

  CMD_WAIT,     /* wait for an event to arrive, possibly with timeout */
  CMD_NOTIFY    /* send an event to one or more channels, with extra user data
                   if needed */
} CommandTypeT;

/* maximum number of parameters module's method can accept */
#define MAX_PARAMS 8

/* conveys information about command between three components of the system:
 * module, scheduler and interpreter */
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
    struct {
      uint8_t num;
      uint8_t reg_num;
      uint8_t value;
    } property;
    struct {
      uint8_t num;
      uint8_t param[MAX_PARAMS];
    } method;
    struct {
      uint32_t miliseconds;
    } wait;
    struct {
      uint32_t channel_mask;
      void *userdata;
    } notify;
  };
} CommandT;

/* To be defined by each module separately. */
typedef struct ModuleState ModuleStateT;

typedef enum {
  MOD_VOICE,    /* produces sound */
  MOD_SENSOR,   /* reads values from sensors */
  MOD_SYNC,     /* implements synchronization mechanism */
  MOD_COMM,     /* implements communication between devices */
  MOD_GENERATOR /* generates notes to be played*/
} ModuleTypeT;

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
  ModuleInterfaceT *module;
} ModuleRegistryEntryT;

extern const ModuleRegistryEntryT ModuleRegistry[];

ModuleInterfaceT *FindModule(const char *name);

#endif
