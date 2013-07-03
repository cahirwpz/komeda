#include <string.h>

#include "api.h"
#include "debug.h"

const ModuleRegistryEntryT ModuleRegistry[] = {
  { NULL, NULL }
};

ModuleInterfaceT *FindModule(const char *name) {
  const ModuleRegistryEntryT *entry = ModuleRegistry;

  while (entry->name) {
    if (!strcmp(entry->name, name))
      break;
    entry++;
  }

  assert(entry->module);
  return entry->module;
}
