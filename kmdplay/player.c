#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "debug.h"
#include "player.h"
#include "synth.h"

typedef struct timeval timeval_t;

/*
 * User keyboard interrupt handling.
 */

static volatile bool ExitRequest = false;

static void SigIntHandler(int signo) {
  ExitRequest = true;
  signal(SIGINT, SIG_DFL);
}

/*
 * Basic channel handling.
 */

NoteT *ChannelNextNote(ChannelT *channel) {
  NoteT *note = &channel->note[channel->current];

  if (note->length == 0.0)
    return NULL;

  channel->current++;
  return note;
}

/*
 * Event definition.
 */

typedef enum { EV_IGNORE = 0, EV_FETCH, EV_WAIT, EV_LAST } EventTypeT;

typedef struct Event {
  EventTypeT type;
  ssize_t channel;

  union {
    struct {
      timeval_t start;
      timeval_t stop;
    } wait;
  };
} EventT;

/*
 * Player structure.
 */

#define MAX_EVENT_NUM (HW_CHANNELS * 2)

struct {
  ChannelT *channel[HW_CHANNELS];
  EventT queue[MAX_EVENT_NUM];
} Player;

void PlayerInit(size_t channels, ...) {
  va_list ap;
  int i;

  bzero(&Player, sizeof(Player));

  va_start(ap, channels);

  for (i = 0; i < channels; i++) {
    Player.channel[i] = va_arg(ap, ChannelT *);

    Player.queue[i].type = EV_FETCH;
    Player.queue[i].channel = i;
  }

  va_end(ap);

  /* set up keyboard break handler */
  signal(SIGINT, SigIntHandler);
}

/*
 * Returns the ''timeval'' when next event should be scheduled.  If returns
 * false then no further events are expected.
 */
bool PlayerOneStep(timeval_t *wakeup) {
  /*
   * This routine doesn't use any O(log n) data structure, because the queue is
   * really short.
   */
  timeval_t now;
  int i;

  gettimeofday(&now, NULL);

  /* clean up expired waits */
  for (i = 0; i < MAX_EVENT_NUM; i++) {
    EventT *event = &Player.queue[i];

    if (event->type == EV_WAIT)
      if (timercmp(&event->wait.stop, &now, <)) {
        event->type = EV_FETCH;
        DEBUG("ch %zd: wait => fetch", event->channel);
      }
  }

  /* fetch data from channels */
  for (i = 0; i < MAX_EVENT_NUM; i++) {
    EventT *event = &Player.queue[i];

    if (event->type == EV_FETCH) {
      NoteT *note = ChannelNextNote(Player.channel[event->channel]);

      if (note) {
        if (note->pitch) {
          DEBUG("ch %zd: play (%zd, %f)", event->channel, note->pitch, note->length);
          SynthPlay(event->channel, note->pitch, note->length);
        } else {
          DEBUG("ch %zd: rest (%f)", event->channel, note->length);
        }

        float l_int = trunc(note->length);
        float l_frac = note->length - l_int;
        struct timeval length = { (int)l_int, (int)(l_frac * 1000000) };

        event->type = EV_WAIT;
        memcpy(&event->wait.start, &now, sizeof(timeval_t));
        timeradd(&now, &length, &event->wait.stop);
      } else {
        DEBUG("ch %zd: finished", event->channel);
        event->type = EV_IGNORE;
      }
    }
  }

  /* set up timer */
  bool found_next = false;

  memcpy(wakeup, &now, sizeof(timeval_t));
  wakeup->tv_sec += 60*60*24*365;  /* one year is large enough ;-) */

  for (i = 0; i < MAX_EVENT_NUM; i++) {
    EventT *event = &Player.queue[i];

    if (event->type == EV_WAIT) {
      if (timercmp(&event->wait.stop, wakeup, <)) {
        memcpy(wakeup, &event->wait.stop, sizeof(timeval_t));
        found_next = true;
      }
    }
  }

  return found_next;
}

bool PlayerRun() {
  struct timeval now, wakeup;

  while (!ExitRequest && PlayerOneStep(&wakeup)) {
    timeval_t interval;

    bzero(&interval, sizeof(interval));

    gettimeofday(&now, NULL);
    timersub(&wakeup, &now, &interval);

    DEBUG("wake up in : %ld:%.6d", interval.tv_sec, interval.tv_usec);

    while (select(0, NULL, NULL, NULL, &interval) == -1) {
      DEBUG("Spurious wake up: %s!", strerror(errno));
    }
  }

  return !ExitRequest;
}
