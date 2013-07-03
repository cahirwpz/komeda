#ifndef __DEBUG_H__
#define __DEBUG_H__

#include <assert.h>
#include <stdlib.h>

#ifndef NDEBUG
#include <stdio.h>
#define DEBUG(fmt, ...) fprintf(stderr, fmt "\n", __VA_ARGS__)
#else
#define DEBUG(fmt, ...)
#endif

void *xmalloc(size_t size);
void *xcalloc(size_t count, size_t size);

#endif
