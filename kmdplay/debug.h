#ifndef __DEBUG_H__
#define __DEBUG_H__

#include <assert.h>

#ifndef NDEBUG
#include <stdio.h>
#define DEBUG(fmt, ...) fprintf(stderr, fmt "\n", __VA_ARGS__)
#else
#define DEBUG(fmt, ...)
#endif

#endif
