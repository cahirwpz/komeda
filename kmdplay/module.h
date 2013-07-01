#ifndef __MODULE_H__
#define __MODULE_H__

#include <stdint.h>

typedef struct ModuleState ModuleStateT;

typedef struct ModuleInterface {
  /* module constructor and destructor */
  ModuleStateT *(*ctor)(void);
  void (*dtor)(ModuleStateT *state);
  /* properties interface */
  void (*set)(ModuleStateT *state, uint8_t reg_no, uint8_t value);
  uint8_t (*get)(ModuleStateT *state, uint8_t reg_no);
  /* methods */
  void (*method[0])(ModuleStateT *state);
} ModuleInterfaceT;

typedef struct Module {
  ModuleInterfaceT *api;
  ModuleStateT *state;
} ModuleT;

static inline uint8_t ModuleGet(ModuleT *module, uint8_t reg_no) {
  return module->api->get(module->state, reg_no);
}

static inline void ModuleSet(ModuleT *module, uint8_t reg_no, uint8_t value) {
  module->api->set(module->state, reg_no, value);
}

#endif
