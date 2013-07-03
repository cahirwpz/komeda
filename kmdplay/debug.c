#include <stdio.h>
#include <stdlib.h>

void *xmalloc(size_t size) {
  void *mem = malloc(size);
  if (!mem) {
    fprintf(stderr, "fatal: memory exhausted (xmalloc of %zd bytes).\n", size);
    abort();
  }
  return mem;
}

void *xcalloc(size_t count, size_t size) {
  void *mem = calloc(count, size);
  if (!mem) {
    fprintf(stderr, "fatal: memory exhausted (xcalloc of %zd bytes).\n", size);
    abort();
  }
  return mem;
}
